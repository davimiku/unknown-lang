//! Single-threaded execution boundary for JIT-generated Hopper frames.
//!
//! The gate is acquired on entry into the outermost Hopper frame and released
//! after the last frame exits. Direct Hopper-to-Hopper calls may nest but callbacks
//! through native code may not, since the GC walker stops at native frames.

use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::{Condvar, Mutex, OnceLock};
use std::thread::{self, ThreadId};

#[derive(Default)]
struct State {
    owner: Option<ThreadId>,
    depth: usize,
    reserved: bool,
    entry_pending: bool,
}

#[derive(Default)]
pub(crate) struct ExecutionGate {
    state: Mutex<State>,
    free: Condvar,
}

/// Exclusive Hopper entry or idle collection reservation
/// Can't be `Send` - it must be released by the thread that acquired it
pub(crate) struct ExecutionReservation<'a> {
    gate: &'a ExecutionGate,
    _not_send: PhantomData<Rc<()>>,
}

impl Drop for ExecutionReservation<'_> {
    fn drop(&mut self) {
        if let Err(error) = self.gate.release_reservation() {
            eprintln!("Hopper execution boundary: {error}");
            std::process::abort();
        }
    }
}

pub(crate) static EXECUTION_GATE: OnceLock<ExecutionGate> = OnceLock::new();

pub(crate) fn gate() -> &'static ExecutionGate {
    EXECUTION_GATE.get_or_init(ExecutionGate::default)
}

impl ExecutionGate {
    pub(crate) fn reserve_native_entry(&self) -> Result<ExecutionReservation<'_>, &'static str> {
        self.reserve(true)
    }

    /// Exclude Hopper execution while collecting only registered native roots.
    /// Acquire this before the runtime mutex, don't wait for the gate while
    /// holding that mutex.
    pub(crate) fn reserve_idle_collection(&self) -> Result<ExecutionReservation<'_>, &'static str> {
        self.reserve(false)
    }

    fn reserve(&self, entry_pending: bool) -> Result<ExecutionReservation<'_>, &'static str> {
        let current = thread::current().id();
        let mut state = self
            .state
            .lock()
            .map_err(|_| "Hopper execution gate poisoned")?;
        loop {
            match state.owner {
                Some(owner) if owner == current => {
                    return Err("cannot start a native entry during Hopper execution or handoff");
                }
                Some(_) => {
                    state = self
                        .free
                        .wait(state)
                        .map_err(|_| "Hopper execution gate poisoned")?;
                }
                None => {
                    state.owner = Some(current);
                    state.reserved = true;
                    state.entry_pending = entry_pending;
                    return Ok(ExecutionReservation {
                        gate: self,
                        _not_send: PhantomData,
                    });
                }
            }
        }
    }

    fn release_reservation(&self) -> Result<(), &'static str> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| "Hopper execution gate poisoned")?;
        if state.owner != Some(thread::current().id()) || !state.reserved || state.depth != 0 {
            return Err("execution reservation ended with an active Hopper frame");
        }
        state.reserved = false;
        state.entry_pending = false;
        state.owner = None;
        self.free.notify_one();
        Ok(())
    }

    pub(crate) fn owned_by_current_thread(&self) -> Result<bool, &'static str> {
        let state = self
            .state
            .lock()
            .map_err(|_| "Hopper execution gate poisoned")?;
        Ok(state.owner == Some(thread::current().id()))
    }

    pub(crate) fn enter(&self, parent_is_hopper: bool) -> Result<(), &'static str> {
        let current = thread::current().id();
        let mut state = self
            .state
            .lock()
            .map_err(|_| "Hopper execution gate poisoned")?;
        loop {
            match state.owner {
                Some(owner) if owner == current => {
                    if !parent_is_hopper {
                        if !state.reserved || !state.entry_pending || state.depth != 0 {
                            return Err("native-to-Hopper re-entry is not supported");
                        }
                        state.entry_pending = false;
                        state.depth = 1;
                        return Ok(());
                    }
                    if state.depth == 0 {
                        return Err("Hopper caller without an active Hopper frame");
                    }
                    state.depth = state
                        .depth
                        .checked_add(1)
                        .ok_or("Hopper frame depth overflow")?;
                    return Ok(());
                }
                Some(_) => {
                    if parent_is_hopper {
                        return Err("Hopper frame on an unregistered thread");
                    }
                    state = self
                        .free
                        .wait(state)
                        .map_err(|_| "Hopper execution gate poisoned")?;
                }
                None => {
                    if parent_is_hopper {
                        return Err("Hopper caller without an active Hopper frame");
                    }
                    state.owner = Some(current);
                    state.depth = 1;
                    return Ok(());
                }
            }
        }
    }

    pub(crate) fn leave(&self) -> Result<(), &'static str> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| "Hopper execution gate poisoned")?;
        if state.owner != Some(thread::current().id()) || state.depth == 0 {
            return Err("Hopper frame exit without matching entry");
        }
        state.depth -= 1;
        if state.depth == 0 && !state.reserved {
            state.owner = None;
            self.free.notify_one();
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{mpsc, Arc};
    use std::time::Duration;

    use super::*;

    #[test]
    fn direct_hopper_calls_nest_but_native_reentry_is_rejected() {
        let gate = ExecutionGate::default();
        assert_eq!(
            gate.enter(true),
            Err("Hopper caller without an active Hopper frame")
        );
        assert!(!gate.owned_by_current_thread().unwrap());
        gate.enter(false).unwrap();
        assert!(gate.owned_by_current_thread().unwrap());
        gate.enter(true).unwrap();
        assert_eq!(
            gate.enter(false),
            Err("native-to-Hopper re-entry is not supported")
        );
        gate.leave().unwrap();
        gate.leave().unwrap();
        assert!(!gate.owned_by_current_thread().unwrap());
        assert_eq!(
            gate.leave(),
            Err("Hopper frame exit without matching entry")
        );
    }

    #[test]
    fn reservation_covers_return_to_native_and_root_registration() {
        let gate = Arc::new(ExecutionGate::default());
        let reservation = gate.reserve_native_entry().unwrap();
        assert!(gate.owned_by_current_thread().unwrap());
        assert!(matches!(gate.reserve_native_entry(), Err(_)));
        assert_eq!(
            gate.enter(true),
            Err("Hopper caller without an active Hopper frame")
        );
        gate.enter(false).unwrap();
        gate.enter(true).unwrap();
        assert_eq!(
            gate.enter(false),
            Err("native-to-Hopper re-entry is not supported")
        );
        gate.leave().unwrap();
        gate.leave().unwrap();
        assert_eq!(
            gate.enter(false),
            Err("native-to-Hopper re-entry is not supported")
        );

        let (started_tx, started_rx) = mpsc::channel();
        let (entered_tx, entered_rx) = mpsc::channel();
        let other = Arc::clone(&gate);
        let worker = thread::spawn(move || {
            started_tx.send(()).unwrap();
            other.enter(false).unwrap();
            entered_tx.send(()).unwrap();
            other.leave().unwrap();
        });
        started_rx.recv().unwrap();
        assert!(entered_rx.recv_timeout(Duration::from_millis(30)).is_err());
        drop(reservation);
        entered_rx.recv_timeout(Duration::from_secs(2)).unwrap();
        worker.join().unwrap();
    }

    #[test]
    fn idle_collection_waits_for_hopper_and_rejects_reentry() {
        let gate = Arc::new(ExecutionGate::default());
        gate.enter(false).unwrap();
        assert!(matches!(gate.reserve_idle_collection(), Err(_)));

        let (started_tx, started_rx) = mpsc::channel();
        let (reserved_tx, reserved_rx) = mpsc::channel();
        let other = Arc::clone(&gate);
        let worker = thread::spawn(move || {
            started_tx.send(()).unwrap();
            let reservation = other.reserve_idle_collection().unwrap();
            assert_eq!(
                other.enter(false),
                Err("native-to-Hopper re-entry is not supported")
            );
            reserved_tx.send(()).unwrap();
            drop(reservation);
        });
        started_rx.recv().unwrap();
        assert!(reserved_rx.recv_timeout(Duration::from_millis(30)).is_err());
        gate.leave().unwrap();
        reserved_rx.recv_timeout(Duration::from_secs(2)).unwrap();
        worker.join().unwrap();
    }

    #[test]
    fn different_threads_wait_until_hopper_execution_finishes() {
        let gate = Arc::new(ExecutionGate::default());
        gate.enter(false).unwrap();
        let (started_tx, started_rx) = mpsc::channel();
        let (entered_tx, entered_rx) = mpsc::channel();
        let other = Arc::clone(&gate);
        let worker = thread::spawn(move || {
            started_tx.send(()).unwrap();
            other.enter(false).unwrap();
            entered_tx.send(()).unwrap();
            other.leave().unwrap();
        });
        started_rx.recv().unwrap();
        assert!(entered_rx.recv_timeout(Duration::from_millis(30)).is_err());
        gate.leave().unwrap();
        entered_rx.recv_timeout(Duration::from_secs(2)).unwrap();
        worker.join().unwrap();
    }
}

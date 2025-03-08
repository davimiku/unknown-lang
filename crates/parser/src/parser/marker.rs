use super::Parser;
use crate::event::Event;
use crate::syntax::SyntaxKind;
use drop_bomb::DropBomb;

pub(crate) struct Marker {
    pos: usize,
    bomb: DropBomb,
}

impl Marker {
    pub(crate) fn new(pos: usize) -> Self {
        Self {
            pos,
            bomb: DropBomb::new("Markers need to be completed"),
        }
    }

    /// Completes the current `Marker` by replacing the `Placeholder`
    /// event with a `StartNode` with the appropriate `SyntaxKind`.
    pub(crate) fn complete(mut self, p: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.bomb.defuse();

        let event_at_pos = &mut p.events[self.pos];
        assert_eq!(*event_at_pos, Event::Placeholder);

        *event_at_pos = Event::StartNode {
            kind,
            forward_parent: None,
        };

        p.events.push(Event::FinishNode);

        CompletedMarker { pos: self.pos }
    }
}

#[derive(Debug)]
pub(crate) struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    /// Allows "wrapping" a parsed node by a "preceding" node
    /// after the fact.
    ///
    /// Returns a `Marker` representing the wrapping node
    /// that can be completed with a different syntax kind.
    pub(crate) fn precede(self, p: &mut Parser) -> Marker {
        let new_marker = p.start();

        match p.events[self.pos] {
            Event::StartNode {
                ref mut forward_parent,
                ..
            } => *forward_parent = Some(new_marker.pos - self.pos),

            _ => unreachable!(),
        }

        new_marker
    }
}

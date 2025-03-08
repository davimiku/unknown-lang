use std::fmt;

use clap::{Args, Parser, Subcommand, ValueEnum};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub(super) struct CliArgs {
    #[command(subcommand)]
    pub(super) command: Commands,
}

#[derive(Subcommand, Debug)]
pub(super) enum Commands {
    /// Checks the script/project for any errors
    Check(CheckArgs),

    /// Checks and compiles the script/project
    Build(BuildArgs),

    /// Checks, compiles, and runs the script/project
    Run(RunArgs),
}

#[derive(Args, Debug)]
pub(super) struct CheckArgs {
    pub(super) path: EntryPath,
}

#[derive(Args, Debug)]
pub(super) struct BuildArgs {
    pub(super) path: EntryPath,

    #[clap(long, default_value_t=OutputTarget::Jit)]
    pub(super) target: OutputTarget,
}

#[derive(Args, Debug)]
pub(super) struct RunArgs {
    pub(super) path: EntryPath,
}

/// Entry point of the program.
///
/// May be a code file for script execution or a project.toml file
/// for project work.
pub(super) type EntryPath = String;

/// The language/format of the compiled output
#[derive(Default, Debug, ValueEnum, Copy, Clone, PartialEq, Eq)]
pub(super) enum OutputTarget {
    /// Produces machine code in-memory that can be executed
    #[default]
    Jit,

    /// Produces a compiled executable program
    Aot,

    /// Produces JavaScript for each source code file
    JS,
}

impl fmt::Display for OutputTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_possible_value()
            .expect("no values are skipped")
            .get_name()
            .fmt(f)
    }
}

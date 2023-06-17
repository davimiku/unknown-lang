use clap::{Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub(super) struct Args {
    #[command(subcommand)]
    pub(super) command: Commands,
}

#[derive(Subcommand, Debug)]
pub(super) enum Commands {
    /// Checks the script/project for any errors
    Check { path: Option<EntryPath> },

    /// Checks and compiles the script/project
    Build { path: Option<EntryPath> },

    /// Checks, compiles, and runs the script/project
    Run { path: Option<EntryPath> },
}

/// Entry point of the program.
///
/// May be a code file for script execution or a project.toml file
/// for project work.
pub(super) type EntryPath = String;

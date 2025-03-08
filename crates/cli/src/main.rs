mod cli_args;

use std::{
    env, fs,
    io::{self, Read, Write},
    path::{Path, PathBuf},
};

use clap::Parser;
use cli_args::{BuildArgs, CheckArgs, CliArgs, OutputTarget, RunArgs};
use exitcode::ExitCode;
use path_clean::PathClean;

use crate::cli_args::{Commands, EntryPath};

fn main() -> io::Result<()> {
    let cli = CliArgs::parse();
    println!("{cli:?}");
    let diagnostics = match cli.command {
        Commands::Check(CheckArgs { path }) => check(path),
        Commands::Build(BuildArgs { path, target }) => build(path, target),
        Commands::Run(RunArgs { path }) => run(path),
    }?;

    for d in diagnostics {
        eprintln!("{d:?}");
    }

    // let diagnostics_json = serde_json::to_string(&diagnostics)?;

    // let mut stdout = io::stdout().lock();
    // stdout.write_all(diagnostics_json.as_bytes())?;

    Ok(())
}

// TODO: lower, do not codegen
// TODO: replace with LSPDiagnostic
// fn check(path: Option<EntryPath>) -> io::Result<Vec<LSPDiagnostic>> {
fn check(path: EntryPath) -> io::Result<Vec<()>> {
    todo!();
    // let program = get_program_input(path)?;
    // Ok(match compiler::compile(&program, true) {
    //     Ok(_) => Vec::new(),
    //     Err(diagnostics) => diagnostics,
    // })

    // let diagnostics: Vec<LSPDiagnostic> = match compiler::compile(&program) {
    //     Ok(_) => vec![],
    //     Err(diagnostics) => diagnostics.iter().map(|d| d.into()).collect(),
    // };

    // Ok(diagnostics)
}

fn build(path: EntryPath, target: OutputTarget) -> io::Result<Vec<()>> {
    let program = get_program_input(path)?;

    match target {
        OutputTarget::Jit => Ok(vec![]),
        OutputTarget::Aot => Ok(vec![]),
        OutputTarget::JS => todo!(),
    }
    // Ok(match compiler::compile(&program, true) {
    //     Ok(_) => Vec::new(),
    //     Err(diagnostics) => diagnostics,
    // })
}

fn run(path: EntryPath) -> io::Result<Vec<()>> {
    todo!();
    // let program = get_program_input(path)?;

    // Ok(match compiler::compile(&program, false) {
    //     Ok(program) => {
    //         println!("{program}");
    //         // let run_result = run(program);
    //         // match run_result {
    //         //     Ok(exit_code) => {
    //         //         if exit_code != exitcode::OK {
    //         //             eprintln!("Exit code: {exit_code}");
    //         //         }
    //         //     }
    //         //     Err(panic) => {
    //         //         eprintln!("Panic: {panic:?}");
    //         //     }
    //         // }
    //         vec![]
    //     }
    //     Err(diagnostics) => diagnostics,
    // })
}

#[cfg(test)]
#[test]
#[ignore = "not a real unit test"]
fn test_main() -> io::Result<()> {
    println!("{:?}", env::current_dir());
    // when "Run Test", the cwd is crates/cli (the level of the Cargo.toml of this crate)
    // when "Debug", the cwd is the root level of this workspace
    let path = String::from("test.prog");
    let diagnostics = run(path)?;

    for d in diagnostics {
        eprintln!("{d:?}");
    }

    // let diagnostics_json = serde_json::to_string(&diagnostics)?;

    // let mut stdout = io::stdout().lock();
    // stdout.write_all(diagnostics_json.as_bytes())?;

    Ok(())
}

fn absolute_path(path: impl AsRef<Path>) -> io::Result<PathBuf> {
    let path = path.as_ref();

    let absolute_path = if path.is_absolute() {
        path.to_path_buf()
    } else {
        env::current_dir()?.join(path)
    }
    .clean();

    Ok(absolute_path)
}

fn get_program_input(path: EntryPath) -> io::Result<String> {
    let abs = absolute_path(path)?;
    let contents = fs::read_to_string(abs)?;

    Ok(contents)
}

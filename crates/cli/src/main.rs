mod args;
mod lsp_diagnostic;

use std::{
    env, fs,
    io::{self, Read, Write},
    path::{Path, PathBuf},
};

use clap::Parser;
use lsp_diagnostic::LSPDiagnostic;
use path_clean::PathClean;

use crate::args::{Args, Commands, EntryPath};

fn main() -> io::Result<()> {
    let cli = Args::parse();
    let diagnostics = match cli.command {
        Commands::Check { path } => check(path),
        Commands::Build { .. } => todo!(),
        Commands::Run { .. } => todo!(),
    }?;

    let diagnostics_json = serde_json::to_string(&diagnostics)?;

    let mut stdout = io::stdout().lock();
    stdout.write_all(diagnostics_json.as_bytes())?;

    Ok(())
}

fn check(path: Option<EntryPath>) -> io::Result<Vec<LSPDiagnostic>> {
    let program = get_program_input(path)?;

    let diagnostics: Vec<LSPDiagnostic> = match compiler::compile(&program) {
        Ok(_) => vec![],
        Err(diagnostics) => diagnostics.iter().map(|d| d.into()).collect(),
    };

    Ok(diagnostics)
}

#[cfg(test)]
#[test]
#[ignore = "not a real unit test"]
fn test_main() {
    let main_result = main();

    if let Err(ref error) = main_result {
        eprintln!("{error}")
    }

    assert!(main_result.is_ok());
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

fn get_program_input(entry_path: Option<EntryPath>) -> io::Result<String> {
    if let Some(path) = entry_path {
        let abs = absolute_path(path)?;
        let contents = fs::read_to_string(abs)?;

        Ok(contents)
    } else {
        let mut buf = String::new();
        let stdin = io::stdin();
        let mut handle = stdin.lock();
        handle.read_to_string(&mut buf)?;

        Ok(buf)
    }
}

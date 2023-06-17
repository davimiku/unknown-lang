use compiler::Diagnostic;

use serde::Serialize;

#[derive(Serialize, Debug)]
pub(super) struct LSPDiagnostic {
    /// Node server is responsible for converting offset to a Position
    range: (u32, u32),

    severity: LSPDiagnosticSeverity,

    message: String,

    code: Option<String>,

    code_description: Option<LSPDiagnosticCodeDescription>,

    source: Option<String>,

    tags: Option<Vec<LSPDiagnosticTag>>,
}

impl From<&Diagnostic> for LSPDiagnostic {
    fn from(value: &Diagnostic) -> Self {
        match value {
            Diagnostic::Type(value) => {
                let range = (value.range.start().into(), value.range.end().into());
                Self {
                    range,
                    severity: LSPDiagnosticSeverity::Error,
                    message: value.message(),
                    code: None,
                    code_description: None,
                    source: Some("Type checker".to_owned()),
                    tags: Some(vec![]),
                }
            }
            _ => {
                panic!("FIXME");
            }
        }
    }
}

#[derive(Serialize, Debug)]
struct LSPDiagnosticCodeDescription {
    href: String,
}

#[derive(Serialize, Debug)]
enum LSPDiagnosticSeverity {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4,
}

#[derive(Serialize, Debug)]
enum LSPDiagnosticTag {
    Unnecessary,
    Deprecated,
}

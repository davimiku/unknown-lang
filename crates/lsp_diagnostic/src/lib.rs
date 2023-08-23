use serde::Serialize;

#[derive(Serialize, Debug, PartialEq)]
pub struct LSPDiagnostic {
    /// Node server is responsible for converting offset to a Position
    pub range: (u32, u32),

    pub severity: LSPDiagnosticSeverity,

    pub message: String,

    pub code: Option<String>,

    pub code_description: Option<LSPDiagnosticCodeDescription>,

    pub source: Option<String>,

    pub tags: Option<Vec<LSPDiagnosticTag>>,
}

#[derive(Serialize, Debug, PartialEq)]
pub struct LSPDiagnosticCodeDescription {
    href: String,
}

#[derive(Serialize, Debug, PartialEq)]
pub enum LSPDiagnosticSeverity {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4,
}

#[derive(Serialize, Debug, PartialEq)]
pub enum LSPDiagnosticTag {
    Unnecessary,
    Deprecated,
}

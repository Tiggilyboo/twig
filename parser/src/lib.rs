#[rust_sitter::grammar("twig")]
pub mod grammar {
    use rust_sitter::Spanned;

    /*
        (. "Hello World")
        (+ 1 2 3)
        (:Chicken "Bawk")
    */

    #[rust_sitter::extra]
    pub struct Whitespace {
        #[rust_sitter::leaf(pattern = r"\s")]
        _whitespace: (),
    }

    #[derive(Debug)]
    #[rust_sitter::language]
    pub enum Expr {
        Number(#[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())] u32),
        Alpha(#[rust_sitter::leaf(pattern = r"\w", transform = |v| v.parse().unwrap())] char),
        String(#[rust_sitter::leaf(pattern = r#"\".*?\""#, transform = |v| v.to_string())] String),
        Identifier(
            #[rust_sitter::leaf(pattern = r"[A-Za-z][A-Za-z0-9_]*", transform = |v| v.to_string())]
            String,
        ),
        Define(#[rust_sitter::leaf(text = ":")] (), Spanned<Box<Expr>>),

        Add(
            #[rust_sitter::leaf(pattern = r"\(\s*\+")] (),
            Spanned<Box<Expr>>,
            #[rust_sitter::leaf(text = ")")] (),
        ),
        Sub(
            #[rust_sitter::leaf(pattern = r"\(\s*\-")] (),
            Spanned<Box<Expr>>,
            #[rust_sitter::leaf(text = ")")] (),
        ),
        Mul(
            #[rust_sitter::leaf(pattern = r"\(\s*\*")] (),
            Spanned<Box<Expr>>,
            #[rust_sitter::leaf(text = ")")] (),
        ),
        Div(
            #[rust_sitter::leaf(pattern = r"\(\s*\/")] (),
            Spanned<Box<Expr>>,
            #[rust_sitter::leaf(text = ")")] (),
        ),
        Mod(
            #[rust_sitter::leaf(pattern = r"\(\s*%")] (),
            Spanned<Box<Expr>>,
            #[rust_sitter::leaf(text = ")")] (),
        ),

        List(
            #[rust_sitter::leaf(text = "(")] (),
            Vec<Spanned<Expr>>,
            #[rust_sitter::leaf(text = ")")] (),
        ),
    }
}

use std::io::Write;

use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use rust_sitter::errors::{ParseError, ParseErrorReason};

fn convert_parse_error_to_diagnostics(
    file_span: &codemap::Span,
    error: &ParseError,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match &error.reason {
        ParseErrorReason::MissingToken(tok) => diagnostics.push(Diagnostic {
            level: Level::Error,
            message: format!("Missing token: \"{tok}\""),
            code: Some("S000".to_string()),
            spans: vec![SpanLabel {
                span: file_span.subspan(error.start as u64, error.end as u64),
                style: SpanStyle::Primary,
                label: Some(format!("missing \"{tok}\"")),
            }],
        }),

        ParseErrorReason::UnexpectedToken(tok) => diagnostics.push(Diagnostic {
            level: Level::Error,
            message: format!("Unexpected token: \"{tok}\""),
            code: Some("S000".to_string()),
            spans: vec![SpanLabel {
                span: file_span.subspan(error.start as u64, error.end as u64),
                style: SpanStyle::Primary,
                label: Some(format!("unexpected \"{tok}\"")),
            }],
        }),

        ParseErrorReason::FailedNode(errors) => {
            if errors.is_empty() {
                diagnostics.push(Diagnostic {
                    level: Level::Error,
                    message: "Failed to parse node".to_string(),
                    code: Some("S000".to_string()),
                    spans: vec![SpanLabel {
                        span: file_span.subspan(error.start as u64, error.end as u64),
                        style: SpanStyle::Primary,
                        label: Some("failed".to_string()),
                    }],
                })
            } else {
                for error in errors {
                    convert_parse_error_to_diagnostics(file_span, error, diagnostics);
                }
            }
        }
    }
}

pub fn play() {
    let stdin = std::io::stdin();

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let mut input = String::new();
        stdin.read_line(&mut input).unwrap();
        let input = input.trim();
        if input.is_empty() {
            break;
        }

        parse(input);
    }
}

pub fn parse(input: &str) -> Option<grammar::Expr> {
    match grammar::parse(input) {
        Ok(expr) => Some(expr),
        Err(errs) => {
            let mut codemap = CodeMap::new();
            let file_span = codemap.add_file("<input>".to_string(), input.to_string());
            let mut diagnostics = vec![];
            for error in errs {
                convert_parse_error_to_diagnostics(&file_span.span, &error, &mut diagnostics);
            }

            let mut emitter = Emitter::stderr(ColorConfig::Always, Some(&codemap));
            emitter.emit(&diagnostics);

            None
        }
    }
}

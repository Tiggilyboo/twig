#[rust_sitter::grammar("twig")]
pub mod grammar {
    use core::fmt;
    use std::str::FromStr;

    #[rust_sitter::extra]
    pub struct Whitespace {
        #[rust_sitter::leaf(pattern = r"\s")]
        _whitespace: (),
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum Comparator {
        #[rust_sitter::leaf(text = "!=")]
        NotEq,
        #[rust_sitter::leaf(text = "<=")]
        LessEq,
        #[rust_sitter::leaf(text = ">=")]
        GreaterEq,
        #[rust_sitter::leaf(text = "=")]
        Eq,
        #[rust_sitter::leaf(text = ">")]
        Greater,
        #[rust_sitter::leaf(text = "<")]
        Less,
        #[rust_sitter::leaf(text = "_")]
        Default,
    }

    /*
        (>= value1 (. "Value is greater or equal to value1"))
    */
    #[derive(Debug)]
    pub struct Condition {
        #[rust_sitter::leaf(text = "(")]
        _open: (),

        pub cmp: Comparator,
        pub operands: Vec<Expr>,

        #[rust_sitter::leaf(text = ")")]
        _close: (),
    }

    #[derive(Debug)]
    pub enum Operator {
        #[rust_sitter::leaf(text = "+")]
        Add,
        #[rust_sitter::leaf(text = "-")]
        Sub,
        #[rust_sitter::leaf(text = "*")]
        Mul,
        #[rust_sitter::leaf(text = "/")]
        Div,
        #[rust_sitter::leaf(text = "%")]
        Mod,
        #[rust_sitter::leaf(text = "!")]
        Not,
        #[rust_sitter::leaf(text = "&")]
        And,
        #[rust_sitter::leaf(text = "|")]
        Or,
        #[rust_sitter::leaf(text = "@")]
        At,
    }

    pub struct Identifier {
        #[rust_sitter::leaf(pattern = r"[A-Za-z][A-Za-z0-9_]*", transform = |v| v.to_string())]
        pub name: String,
    }

    impl fmt::Debug for Identifier {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let n = &self.name;
            write!(f, "{n}")
        }
    }
    impl Identifier {
        pub fn from(name: &str) -> Self {
            Self {
                name: name.to_string(),
            }
        }
    }

    #[derive(Debug)]
    pub struct Numeric {
        #[rust_sitter::leaf(pattern = r"(-)?\d+(\.\d+)?", transform = |v| v.to_string())]
        _raw: String,
    }

    impl From<i32> for Numeric {
        fn from(v: i32) -> Numeric {
            Numeric::from(v.to_string())
        }
    }
    impl From<String> for Numeric {
        fn from(v: String) -> Numeric {
            Numeric { _raw: v }
        }
    }

    impl Numeric {
        pub fn get_raw(&self) -> &str {
            &self._raw
        }

        pub fn get_value<T>(&self) -> Option<T>
        where
            T: FromStr
                + std::ops::Add<T>
                + std::ops::Sub<T>
                + std::ops::Div<T>
                + std::ops::Mul<T>
                + PartialEq<T>
                + Eq
                + PartialOrd<T>
                + From<T>,
        {
            match self._raw.parse() {
                Ok(v) => Some(v),
                Err(_) => None,
            }
        }
    }

    #[derive(Debug)]
    pub enum ReturnType {
        #[rust_sitter::leaf(text = "i")]
        Integer,
        #[rust_sitter::leaf(text = "f")]
        Float,
        #[rust_sitter::leaf(text = "s")]
        String,
        #[rust_sitter::leaf(text = "(s)")]
        ListString,
        #[rust_sitter::leaf(text = "(f)")]
        ListFloat,
        #[rust_sitter::leaf(text = "(i)")]
        ListInteger,
    }

    /*
        (i:main 123)
    */
    pub struct DefineFunc {
        #[rust_sitter::leaf(text = "(")]
        _start: (),

        pub ty: ReturnType,

        #[rust_sitter::leaf(text = ":")]
        _sep: (),

        pub identifier: Identifier,

        pub params: Vec<Param>,

        pub body: Box<Expr>,

        #[rust_sitter::leaf(text = ")")]
        _end: (),
    }

    /*
        (:variable 123)
    */
    pub struct DefineVar {
        #[rust_sitter::prec(1)]
        #[rust_sitter::leaf(text = "(")]
        _start: (),

        #[rust_sitter::leaf(text = ":")]
        _sep: (),

        pub identifier: Identifier,

        pub body: Box<Expr>,

        #[rust_sitter::leaf(text = ")")]
        _end: (),
    }

    impl DefineFunc {
        pub fn any(&self) -> bool {
            self.params.len() > 0
        }
    }

    impl fmt::Debug for DefineFunc {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("DefineFunc")
                .field("ty", &self.ty)
                .field("identifier", &self.identifier)
                .field("params", &self.params)
                .field("body", &self.body)
                .finish()
        }
    }
    impl fmt::Debug for DefineVar {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("DefineVar")
                .field("identifier", &self.identifier)
                .field("body", &self.body)
                .finish()
        }
    }

    #[derive(Debug)]
    pub enum DefineExpr {
        #[rust_sitter::prec(1)]
        Function(DefineFunc),
        #[rust_sitter::prec(2)]
        Variable(DefineVar),
    }

    #[derive(Debug)]
    #[rust_sitter::language]
    pub enum Expr {
        Number(Numeric),
        String(#[rust_sitter::leaf(pattern = r#"\".*?\""#, transform = |v| v.to_string())] String),

        Identifier(Identifier),
        Function(DefineFunc),
        Variable(DefineVar),

        /*
            (:value 123)
            (? value
              (< 0 (. "Value is less than 0"))
              (= 1 (. "Value is 1"))
              (_ (. "Value is 0 or greater than 1")))
        */
        Switch(
            #[rust_sitter::leaf(text = "(")] (),
            #[rust_sitter::leaf(text = "?")] (),
            Box<Expr>,
            Vec<Condition>,
            #[rust_sitter::leaf(text = ")")] (),
        ),

        /*
            (+ 1 3 8)
        */
        Operation(
            #[rust_sitter::leaf(text = "(")] (),
            Operator,
            Vec<Expr>,
            #[rust_sitter::leaf(text = ")")] (),
        ),
        List(
            #[rust_sitter::prec(2)]
            #[rust_sitter::leaf(text = "(")]
            (),
            Vec<Expr>,
            #[rust_sitter::leaf(text = ")")] (),
        ),
    }

    // (i:main, i:argc, (s):argv ( ...
    //        ^-------^---------
    #[derive(Debug)]
    pub struct Param {
        #[rust_sitter::leaf(text = ",")]
        _prefix: (),

        pub ty: ReturnType,

        #[rust_sitter::leaf(text = ":")]
        _sep: (),

        pub identifier: Identifier,
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

pub use grammar::*;

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

pub fn parse(input: &str) -> Option<Expr> {
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

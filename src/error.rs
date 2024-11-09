use std::path::PathBuf;

use chumsky::error::{Rich, RichReason};
use codesnake::{Block, CodeWidth, Label, LineIndex};
use yansi::Paint;

pub fn parser_errs<'src, T>(
    errs: Vec<Rich<T>>,
    path: &PathBuf,
    src: &'src str,
) where T: std::fmt::Display + Clone {
    let idx = LineIndex::new(&src);
    for err in errs {
        build_parser_err(err, path, &idx)
            .print();
    }
}

/// Constructs a [`CompilerErr`] from a `Rich`.
///
/// Should be called from a public function which executes it on each error
/// returned from the parser.
///
/// * `err` - the parser error
/// * `path` - the path to the source file (just for printing)
/// * `idx` - codesnake `LineIndex` from source, for printing the offending
///   code; should be constructed in calling function
fn build_parser_err<'src, T>(
    err: Rich<T>,
    path: &PathBuf,
    idx: &'src LineIndex<'src>,
) -> CompilerErr<'src>
where T: std::fmt::Display + Clone {
    let reason = err.reason();

    match reason {
        RichReason::ExpectedFound { expected, found } => {
            let expected = if expected.len() > 1 {
                let mut s = String::from("expected one of: ");

                for i in 0..expected.len() - 1 {
                    s.push_str(&format!("{}, ", expected[i]));
                }

                s.push_str(&format!("{}", expected[expected.len() - 1]));

                s
            } else {
                format!("expected: {}", expected[0])
            };

            let msg = format!(
                "[{:#?}]: unexpected input, {}",
                path.file_name().expect("should be a valid filename before call"),
                expected,
            );

            let label = if let Some(token) = found {
                Label::new(err.span().into_range())
                    .with_text(format!("found `{}`", token.clone().into_inner()))
                    .with_style(|s| s.red().to_string())
            } else {
                Label::new(err.span().into_range())
                    .with_style(|s| s.red().to_string())
            };

            let block = Block::new(&idx, [label])
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        RichReason::Custom(msg) => {
            let msg = format!(
                "[{:#?}]: {}",
                path.file_name().unwrap(),
                msg,
            );

            let label = Label::new(err.span().into_range())
                .with_text("here".to_owned())
                .with_style(|s| s.red().to_string());
            let block = Block::new(&idx, [label]).unwrap();
            let block = block.map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        RichReason::Many(_) => todo!(),
    }
}

struct CompilerErr<'a> {
    msg: String,
    block: Block<CodeWidth<&'a str>, String>,
}

impl<'a> CompilerErr<'a> {
    pub fn print(&self) {
        eprintln!("{}{}", self.block.prologue(), self.msg);
        eprint!("{}", self.block);
        eprintln!("{}", self.block.epilogue());
    }
}

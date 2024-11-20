use std::path::PathBuf;

use crate::analysis::TypecheckErr;
use crate::symbol::NameResErr;

use chumsky::error::{Rich, RichReason};
use codesnake::{Block, CodeWidth, Label, LineIndex};
use yansi::Paint;

/// Takes [`NameResErr`]s output by the [`NameResVisitor`] and prints
/// diagnostics.
///
/// [`NameResVisitor`]: crate::symbol::NameResVisitor
pub fn name_res_errs<'src>(
    errs: Vec<NameResErr<'_>>,
    path: &PathBuf,
    src: &'src str,
) {
    let idx = LineIndex::new(&src);
    for err in errs {
        build_name_res_err(err, path, &idx)
            .print();
    }
}

/// Constructs a [`CompilerErr`] from a [`NameResErr`].
///
/// Should be called from [`name_res_errs`].
///
/// * `err` - the name resolution error
/// * `path` - the path to the source file (just for printing)
/// * `idx` - codesnake `LineIndex` from source, for printing the offending
///   code; should be constructed in calling function
fn build_name_res_err<'src>(
    err: NameResErr<'_>,
    path: &PathBuf,
    idx: &'src LineIndex<'src>
) -> CompilerErr<'src> {
    match err {
        NameResErr::AlreadyExists { symbol, span } => {
            let msg = format!(
                "[{:#?}]: symbol with name `{}` already exists in this scope",
                path,
                symbol.borrow().ident(),
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(symbol.borrow().decl_span().into_range())
                            .with_text(String::from("first declared here"))
                            .with_style(|s| s.blue().to_string()),
                        
                        Label::new(span.into_range())
                            .with_text(String::from("attempt to redeclare here"))
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        NameResErr::NotExists { ident, span } => {
            let msg = format!(
                "[{:#?}]: unresolved reference to `{}`",
                path,
                ident,
            );

            let block = 
                Block::new(
                    &idx,
                    [
                        Label::new(span.into_range())
                            .with_text(String::from("symbol not found in this scope"))
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        NameResErr::PrototypeNotMatch { proto_symbol, span } => {
            let msg = format!(
                "[{:#?}]: definition of function `{}` doesn't match prototype",
                path,
                proto_symbol.borrow().ident(),
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(proto_symbol.borrow().decl_span().into_range())
                            .with_text(String::from("prototype here"))
                            .with_style(|s| s.blue().to_string()),
                        Label::new(span.into_range())
                            .with_text(String::from("definition here"))
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        NameResErr::MainNonInt { span } => {
            let msg = format!("[{:#?}]: `main` must return type `integer`", path);

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(span.into_range())
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        NameResErr::NoMain => {
            let msg = format!("[{:#?}]: must have a function `main`", path);

            let block = Block::new(&idx, [Label::new(0..0)]).unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
    }
}

/// Takes [`chumsky::Rich`] errors generated by the [`parser`] and prints
/// diagnostics.
///
/// [`parser`]: crate::parse::parser
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

/// Constructs a [`CompilerErr`] from a [`chumsky::Rich`].
///
/// Should be called from [`parser_errs`].
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
                path,
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

/// Takes [`TypecheckErr`]s generated by the [`TypecheckVisitor`] and prints
/// diagnostics.
///
/// [`TypecheckVisitor`]:   crate::analysis::TypecheckVisitor
pub fn typecheck_errs<'src>(
    errs: Vec<TypecheckErr<'_>>,
    path: &PathBuf,
    src: &'src str,
) {
    let idx = LineIndex::new(&src);
    for err in errs {
        build_typecheck_err(err, path, &idx)
            .print();
    }
}

/// Constructs a [`CompilerErr`] from a [`TypecheckErr`].
///
/// Should be called from [`typecheck_errs`].
///
/// * `err` - the type error
/// * `path` - the path to the source file (just for printing)
/// * `idx` - codesnake `LineIndex` from source, for printing the offending
///   code; should be constructed in calling function
fn build_typecheck_err<'src>(
    err: TypecheckErr<'_>,
    path: &PathBuf,
    idx: &'src LineIndex<'src>,
) -> CompilerErr<'src> {
    match err {
        TypecheckErr::AssignMismatch { left, right } => {
            let msg = format!(
                "[{:#?}]: cannot assign `{}` to value of type `{}`",
                path,
                left,
                right,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(left.get_span().into_range())
                            .with_text(format!("has type `{}`", left))
                            .with_style(|s| s.red().to_string()),
                    
                        Label::new(right.get_span().into_range())
                            .with_text(format!("has type `{}`", right))
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        TypecheckErr::CmpDiffTypes { left, right } => {
            let msg = format!(
                "[{:#?}]: cannot compare values of different types",
                path,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(left.get_span().into_range())
                            .with_text(format!("has type `{}`", left))
                            .with_style(|s| s.red().to_string()),

                        Label::new(right.get_span().into_range())
                            .with_text(format!("has type `{}`", right))
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        TypecheckErr::ExpectedBool { verb, found } => {
            let msg = format!(
                "[{:#?}]: operation `{}` only accepts `boolean` values",
                path,
                verb,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(found.get_span().into_range())
                            .with_text(format!("found type `{}`", found))
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        TypecheckErr::ExpectedInt { verb, found } => {
            let msg = format!(
                "[{:#?}]: operation `{}` only accepts `integer` values",
                path,
                verb,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(found.get_span().into_range())
                            .with_text(format!("found type `{}`", found))
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        TypecheckErr::IndexNonArray { found } => {
            let msg = format!(
                "[{:#?}]: cannot index access non-array values",
                path,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(found.get_span().into_range())
                            .with_text(format!("has type `{}`", found))
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        TypecheckErr::MixedTypeArray { first_expected, found } => {
            let msg = format!(
                "[{:#?}]: array contains values of multiple types",
                path,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(first_expected.get_span().into_range())
                            .with_text(
                                format!(
                                    "type of array is `{}` because of this",
                                    first_expected,
                                )
                            )
                            .with_style(|s| s.blue().to_string()),

                        Label::new(found.get_span().into_range())
                            .with_text(
                                format!(
                                    "element has type `{}`",
                                    found,
                                )
                            )
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        TypecheckErr::NonIntIndex { found } => {
            let msg = format!(
                "[{:#?}]: arrays can only be indexed with type `integer`",
                path,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(found.get_span().into_range())
                            .with_text(format!("found type `{}`", found))
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        TypecheckErr::NonSizedArray { span } => {
            let msg = format!(
                "[{:#?}]: array declaration must have either size or initializer",
                path,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(span.into_range())
                            .with_text(String::from("size cannot be inferred"))
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        TypecheckErr::InvalidPrint { found, span } => {
            let msg = format!(
                "[{:#?}]: cannot print type `{}`",
                path,
                found,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(found.get_span().into_range())
                            .with_text(format!("has type `{}`", found))
                            .with_style(|s| s.blue().to_string()),

                        Label::new(span.into_range())
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        TypecheckErr::TypeNotAssignable { r#type, span } => {
            let msg = format!(
                "[{:#?}]: type `{}` is not assignable",
                path,
                r#type,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(span.into_range())
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        TypecheckErr::WrongNumArgs { expected, found } => {
            let msg = format!(
                "[{:#?}]: wrong number of arguments to function call",
                path,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(expected.1.into_range())
                            .with_text(format!("defined with `{}` parameters", expected.0))
                            .with_style(|s| s.blue().to_string()),

                        Label::new(found.1.into_range())
                            .with_text(format!("called with `{}` arguments", found.0))
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        TypecheckErr::WrongSizeArray { expected, found } => {
            let msg = format!(
                "[{:#?}]: array initializer doesn't match given size of array",
                path,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(expected.1.into_range())
                            .with_text(format!("size `{}` given here", expected.0))
                            .with_style(|s| s.blue().to_string()),

                        Label::new(found.1.into_range())
                            .with_text(format!("initialized with size `{}`", found.0)),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        TypecheckErr::WrongTypeArg { expected, found } => {
            let msg = format!(
                "[{:#?}]: argument to function call is the wrong type",
                path,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(expected.get_span().into_range())
                            .with_text(format!("expected `{}`", expected))
                            .with_style(|s| s.blue().to_string()),

                        Label::new(found.get_span().into_range())
                            .with_text(format!("found `{}`", found))
                            .with_style(|s| s.red().to_string()),
                    ]
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
        TypecheckErr::WrongTypeReturn { expected, found } => {
            let msg = format!(
                "[{:#?}]: type of return statement doesn't match return type of function",
                path,
            );

            let block =
                Block::new(
                    &idx,
                    [
                        Label::new(expected.get_span().into_range())
                            .with_text(
                                format!("function has return type `{}`", expected)
                            )
                            .with_style(|s| s.blue().to_string()),

                        Label::new(found.get_span().into_range())
                            .with_text(format!("has type `{}`", found))
                            .with_style(|s| s.red().to_string()),
                    ],
                )
                .unwrap()
                .map_code(|c| CodeWidth::new(c, c.len()));

            CompilerErr { msg, block }
        }
    }
}

/// Type for reporting diagnostics information for compiler errors.
struct CompilerErr<'a> {
    /// The primary error message.
    msg: String,
    /// Codesnake object which holds the source code, line numbers, and labels
    /// for the error report.
    block: Block<CodeWidth<&'a str>, String>,
}

impl<'a> CompilerErr<'a> {
    pub fn print(&self) {
        eprintln!("{}{}", self.block.prologue(), self.msg);
        eprint!("{}", self.block);
        eprintln!("{}", self.block.epilogue());
    }
}

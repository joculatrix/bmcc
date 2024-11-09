use chumsky::{extra::Err, prelude::*};
use crate::ast::*;

mod lex;
mod syntax;
mod token;

pub use lex::lex;
pub use syntax::parser;
pub use token::{Token, Keyword, Op};

use chumsky::{extra::Err, prelude::*};
use crate::ast::*;

mod lex;
mod syntax;
mod token;

pub use token::{Token, Keyword, Op};

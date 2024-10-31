mod decl;
mod expr;
mod stmt;
mod r#type;

pub use self::decl::Decl;
pub use self::expr::Expr;
pub use self::stmt::Stmt;
pub use self::r#type::Type;
pub use self::r#type::Atomic;

use chumsky::span::SimpleSpan;

pub trait Spanned {
	fn set_span(&mut self, span: SimpleSpan);
}
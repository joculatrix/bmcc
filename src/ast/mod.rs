pub mod decl;
pub mod expr;
pub mod stmt;
pub mod r#type;

pub use self::decl::Decl;
pub use self::expr::Expr;
pub use self::stmt::Stmt;
pub use self::r#type::Type;
pub use self::r#type::Atomic;

use chumsky::span::SimpleSpan;

pub trait Spanned {
	fn with_span(self, span: SimpleSpan) -> Self;
	fn get_span(&self) -> Option<SimpleSpan>;
}
pub mod decl;
pub mod expr;
pub mod stmt;
pub mod r#type;

pub use self::decl::Decl;
pub use self::expr::Expr;
pub use self::r#type::Atomic;
pub use self::r#type::Type;
pub use self::stmt::Stmt;

use chumsky::span::SimpleSpan;
use crate::symbol::SymbolRef;

pub type Spanned<T> = (T, SimpleSpan);


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

pub type Spanned<T> = (T, SimpleSpan);

pub trait SpannedTrait<T> {
    fn with_span(input: T, span: SimpleSpan) -> Self;
    fn get_span(&self) -> SimpleSpan;
    fn set_span(&mut self, span: SimpleSpan);
    fn ref_inner(&self) -> &T;
}

impl<T> SpannedTrait<T> for Spanned<T> {
    fn with_span(input: T, span: SimpleSpan) -> Self {
        (input, span)
    }

    fn get_span(&self) -> SimpleSpan {
        self.1
    }

    fn set_span(&mut self, span: SimpleSpan) {
        self.1 = span;
    }

    fn ref_inner(&self) -> &T {
        &self.0
    }
}

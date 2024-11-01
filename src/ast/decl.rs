use super::*;

/// AST type for B-Minor declarations. A B-Minor program, at its top level, is
/// a list of declarations.
pub enum Decl<'src> {
	Var(Var<'src>),
	Function(Function<'src>),
}

pub struct Var<'src> {
	name: &'src str,
	r#type: Box<Type<'src>>,
	rhs: Option<Expr<'src>>,
	span: Option<SimpleSpan>,
}

pub struct Function<'src> {
	name: &'src str,
	r#type: Box<Type<'src>>,
	rhs: Option<Stmt<'src>>,
	span: Option<SimpleSpan>,
}

impl Spanned for Decl<'_> {
	fn with_span(self, span: SimpleSpan) -> Self {
		match self {
			Decl::Var(v) => Decl::Var(Var { span: Some(span), ..v }),
			Decl::Function(f) => Decl::Function(
				Function { span: Some(span), ..f }
			),
		}
	}
}
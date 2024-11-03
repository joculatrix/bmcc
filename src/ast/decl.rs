use super::*;

#[derive(Clone)]
/// AST type for B-Minor declarations. A B-Minor program, at its top level, is
/// a list of declarations.
pub enum Decl<'src> {
	Var(Var<'src>),
	Function(Function<'src>),
}

#[derive(Clone)]
pub struct Var<'src> {
	pub name: &'src str,
	pub r#type: Box<Type<'src>>,
	pub rhs: Option<Expr<'src>>,
	pub span: Option<SimpleSpan>,
}

#[derive(Clone)]
pub struct Function<'src> {
	pub name: &'src str,
	pub r#type: Box<Type<'src>>,
	pub rhs: Option<Stmt<'src>>,
	pub span: Option<SimpleSpan>,
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

	fn get_span(&self) -> Option<SimpleSpan> {
		match self {
			Decl::Var(Var { span: s, .. }) => *s,
			Decl::Function(Function { span: s, .. }) => *s,
		}
	}
}
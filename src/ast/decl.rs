use super::*;

/// AST type for B-Minor declarations. A B-Minor function, at its top level, is
/// a list of declarations.
pub enum Decl<'src> {
	Var {
		name: &'src str,
		r#type: Box<Type<'src>>,
		rhs: Option<Expr<'src>>,
		span: Option<SimpleSpan>,
	},
	Function {
		name: &'src str,
		r#type: Box<Type<'src>>,
		rhs: Option<Stmt<'src>>,
		span: Option<SimpleSpan>,
	}
}

impl Spanned for Decl<'_> {
	fn set_span(&mut self, span: SimpleSpan) {
		let s = match self {
			Decl::Var { span: s, .. } => s,
			Decl::Function { span: s, .. } => s,
		};

		*s = Some(span);
	}
}
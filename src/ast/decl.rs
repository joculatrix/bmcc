use super::*;

/// AST type for B-Minor declarations. A B-Minor function, at its top level, is
/// a list of declarations.
pub enum Decl<'src> {
	Var {
		name: &'src str,
		r#type: Box<Type<'src>>,
		rhs: Option<Expr<'src>>,
	},
	Function {
		name: &'src str,
		r#type: Box<Type<'src>>,
		rhs: Option<Stmt<'src>>,
	}
}
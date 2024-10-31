use super::*;

/// AST type for B-Minor statements.
/// 
/// Warning: The `Option<SimpleSpan>` must be the last field in ordered tuples
/// or else the `Spanned` trait will break.
pub enum Stmt<'src> {
	Decl(Box<Decl<'src>>, Option<SimpleSpan>),
	Expr(Box<Expr<'src>>, Option<SimpleSpan>),
	If(Box<Expr<'src>>, Box<Stmt<'src>>, Option<SimpleSpan>),
	IfElse {
		condition: Box<Expr<'src>>,
		body: Box<Stmt<'src>>,
		else_body: Box<Stmt<'src>>,
		span: Option<SimpleSpan>,
	},
	/// A for-loop of the form:
	/// 
	/// for (`init_expr`; `condition`; `next_expr`) `body`
	For {
		init_expr: Box<Expr<'src>>,
		condition: Box<Expr<'src>>,
		next_expr: Box<Expr<'src>>,
		body: Box<Stmt<'src>>,
		span: Option<SimpleSpan>,
	},
	/// A comma-separated list of expressions to print.
	Print(Vec<Expr<'src>>, Option<SimpleSpan>),
	/// A statement returning the value of an expression from a function.
	Return(Box<Expr<'src>>, Option<SimpleSpan>),
	/// A `{}` block of statements.
	Block(Vec<Stmt<'src>>, Option<SimpleSpan>),
}

impl Spanned for Stmt<'_> {
	fn set_span(&mut self, span: SimpleSpan) {
		let s = match self {
			Stmt::Decl(.., s) => s,
			Stmt::Expr(.., s) => s,
			Stmt::If(.., s) => s,
			Stmt::IfElse { span: s, .. } => s,
			Stmt::For { span: s, .. } => s,
			Stmt::Print(.., s) => s,
			Stmt::Return(.., s) => s,
			Stmt::Block(.., s) => s,
		};

		*s = Some(span);
	}
}
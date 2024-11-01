use super::*;

/// AST type for B-Minor statements.
/// 
/// Warning: The `Option<SimpleSpan>` must be the last field in ordered tuples
/// or else the `Spanned` trait will break.
pub enum Stmt<'src> {
	Decl(Box<Decl<'src>>, Option<SimpleSpan>),
	Expr(Box<Expr<'src>>, Option<SimpleSpan>),
	If(Box<Expr<'src>>, Box<Stmt<'src>>, Option<SimpleSpan>),
	IfElse(IfElse<'src>),
	For(For<'src>),
	/// A comma-separated list of expressions to print.
	Print(Vec<Expr<'src>>, Option<SimpleSpan>),
	/// A statement returning the value of an expression from a function.
	Return(Box<Expr<'src>>, Option<SimpleSpan>),
	/// A `{}` block of statements.
	Block(Vec<Stmt<'src>>, Option<SimpleSpan>),
}

pub struct IfElse<'src> {
	condition: Box<Expr<'src>>,
	body: Box<Stmt<'src>>,
	else_body: Box<Stmt<'src>>,
	span: Option<SimpleSpan>,
}

/// A for-loop of the form:
/// 
/// for (`init_expr`; `condition`; `next_expr`) `body`
pub struct For<'src> {
	init_expr: Box<Expr<'src>>,
	condition: Box<Expr<'src>>,
	next_expr: Box<Expr<'src>>,
	body: Box<Stmt<'src>>,
	span: Option<SimpleSpan>,
}

impl Spanned for Stmt<'_> {
	fn with_span(self, span: SimpleSpan) -> Self {
		match self {
			Stmt::Decl(a, _) => Stmt::Decl(a, Some(span)),
			Stmt::Expr(b, _) => Stmt::Expr(b, Some(span)),
			Stmt::If(a, b, _) => Stmt::If(a, b, Some(span)),
			Stmt::IfElse(x) => Stmt::IfElse(IfElse { span: Some(span), ..x }),
			Stmt::For(x) => Stmt::For(For { span: Some(span), ..x }),
			Stmt::Print(a, _) => Stmt::Print(a, Some(span)),
			Stmt::Return(a, _) => Stmt::Return(a, Some(span)),
			Stmt::Block(a, _) => Stmt::Block(a, Some(span)),
		}
	}
}
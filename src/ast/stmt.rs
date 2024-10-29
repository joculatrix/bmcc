use super::*;

/// AST type for B-Minor statements.
pub enum Stmt<'src> {
	Decl(Box<Decl<'src>>),
	Expr(Box<Expr<'src>>),
	If(Box<Expr<'src>>, Box<Stmt<'src>>),
	IfElse {
		condition: Box<Expr<'src>>,
		body: Box<Stmt<'src>>,
		else_body: Box<Stmt<'src>>,
	},
	/// A for-loop of the form:
	/// 
	/// for (`init_expr`; `condition`; `next_expr`) `body`
	For {
		init_expr: Box<Expr<'src>>,
		condition: Box<Expr<'src>>,
		next_expr: Box<Expr<'src>>,
		body: Box<Stmt<'src>>,
	},
	/// A comma-separated list of expressions to print.
	Print(Vec<Expr<'src>>),
	/// A statement returning the value of an expression from a function.
	Return(Box<Expr<'src>>),
	/// A `{}` block of statements.
	Block(Vec<Stmt<'src>>),
}
use super::*;

#[derive(Clone, Debug)]
/// AST type for B-Minor statements.
/// 
/// Thain's book technically states the while statement shouldn't exist.
/// However, it also lists `while` as a keyword. So I'm doing it anyway.
/// Call it an "extension of the language".
pub enum Stmt<'src> {
    Block(Vec<Stmt<'src>>, SimpleSpan),
    Decl(Box<Decl<'src>>, SimpleSpan),
    Expr(Expr<'src>, SimpleSpan),
    Print(Vec<Expr<'src>>, SimpleSpan),
    Return(Option<Expr<'src>>, SimpleSpan),
    If(IfStmt<'src>),
    For(ForStmt<'src>),
    While(WhileStmt<'src>),
}

#[derive(Clone, Debug)]
pub struct IfStmt<'src> {
	pub condition: Box<Expr<'src>>,
	pub body: Box<Stmt<'src>>,
	pub else_body: Option<Box<Stmt<'src>>>,
	pub span: SimpleSpan,
}

#[derive(Clone, Debug)]
/// A for-loop of the form:
/// 
/// for (`init_expr`; `condition`; `next_expr`) `body`
pub struct ForStmt<'src> {
	pub init_expr: Box<Expr<'src>>,
	pub condition: Box<Expr<'src>>,
	pub next_expr: Box<Expr<'src>>,
	pub body: Box<Stmt<'src>>,
	pub span: SimpleSpan,
}

#[derive(Clone, Debug)]
pub struct WhileStmt<'src> {
    pub condition: Box<Expr<'src>>,
    pub body: Box<Stmt<'src>>,
    pub span: SimpleSpan,
}

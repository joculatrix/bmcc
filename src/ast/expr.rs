use super::*;

#[derive(Clone, Debug)]
/// AST type for B-Minor expressions.
/// 
/// Note: Arithmetic operations in B-Minor can only be performed on integers.
pub enum Expr<'src> {

	/* =========================  Atomic  ======================== */

	/// A variable or function identifier.
	Ident(IdentExpr<'src>),
	/// A boolean literal: `true` or `false`.
	BoolLit(bool, SimpleSpan),
	/// A char literal. In B-Minor, only ASCII values are acceptable.
	CharLit(char, SimpleSpan),
	/// A 64-bit integer literal.
	IntLit(i64, SimpleSpan),
	/// A string literal. In B-Minor, strings are immutable.
	StrLit(&'src str, SimpleSpan),
	/// An indexed array access.
	Index(IndexExpr<'src>),

	/* ========================== Other ========================== */

    Binary(BinaryExpr<'src>),

	/// Unary increment.
	Inc(Box<Expr<'src>>, SimpleSpan),
	/// Unary decrement.
	Dec(Box<Expr<'src>>, SimpleSpan),
	/// Unary negation.
	Neg(Box<Expr<'src>>, SimpleSpan),
    /// Unary logical NOT.
	Not(Box<Expr<'src>>, SimpleSpan),

	/// A function call.
	Call(CallExpr<'src>),
	/// An array.
	Array(Vec<Expr<'src>>, SimpleSpan),
}

#[derive(Clone, Debug)]
pub enum BinaryExprKind {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Exp,
    Mod,
    Eq,
    NotEq,
    And,
    Or,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

#[derive(Clone, Debug)]
pub struct BinaryExpr<'src> {
    pub kind: BinaryExprKind,
    pub left: Box<Expr<'src>>,
    pub right: Box<Expr<'src>>,
    pub span: SimpleSpan,
}

#[derive(Clone, Debug)]
pub struct CallExpr<'src> {
    pub ident: &'src str,
    pub args: Vec<Expr<'src>>,
    pub span: SimpleSpan,
}

#[derive(Clone, Debug)]
pub struct IdentExpr<'src> {
    pub ident: &'src str,
    pub span: SimpleSpan,
}

#[derive(Clone, Debug)]
pub struct IndexExpr<'src> {
    pub array: Box<Expr<'src>>,
    pub index: Box<Expr<'src>>,
    pub span: SimpleSpan,
}

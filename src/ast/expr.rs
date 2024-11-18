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

impl<'a> Expr<'a> {
    pub fn to_verb(&self) -> Option<&'static str> {
        match self {
            Expr::Ident(_) => None,
            Expr::BoolLit(_, _) => None,
            Expr::CharLit(_, _) => None,
            Expr::IntLit(_, _) => None,
            Expr::StrLit(_, _) => None,
            Expr::Index(_) => Some("index"),
            Expr::Binary(BinaryExpr { kind, .. }) => match kind {
                BinaryExprKind::Assign => Some("assign"),
                BinaryExprKind::Add => Some("add"),
                BinaryExprKind::Sub => Some("subtract"),
                BinaryExprKind::Mul => Some("multiply"),
                BinaryExprKind::Div => Some("divide"),
                BinaryExprKind::Exp => Some("exponentiate"),
                BinaryExprKind::Mod => Some("modulo"),
                BinaryExprKind::Eq => Some("compare"),
                BinaryExprKind::NotEq => Some("compare"),
                BinaryExprKind::And => Some("AND"),
                BinaryExprKind::Or => Some("OR"),
                BinaryExprKind::Less => Some("compare"),
                BinaryExprKind::LessEq => Some("compare"),
                BinaryExprKind::Greater => Some("compare"),
                BinaryExprKind::GreaterEq => Some("compare"),
            }
            Expr::Inc(_, _) => Some("increment"),
            Expr::Dec(_, _) => Some("decrement"),
            Expr::Neg(_, _) => Some("negate"),
            Expr::Not(_, _) => Some("logical NOT"),
            Expr::Call(_) => Some("call"),
            Expr::Array(_, _) => None,
        }
    }
}

#[derive(Copy, Clone, Debug)]
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
    pub symbol: Option<SymbolRef<'src>>,
}

#[derive(Clone, Debug)]
pub struct IdentExpr<'src> {
    pub ident: &'src str,
    pub span: SimpleSpan,
    pub symbol: Option<SymbolRef<'src>>,
}

#[derive(Clone, Debug)]
pub struct IndexExpr<'src> {
    pub array: Box<Expr<'src>>,
    pub index: Box<Expr<'src>>,
    pub span: SimpleSpan,
}

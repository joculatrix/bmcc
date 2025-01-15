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
	Array(Vec<Expr<'src>>, Option<Type<'src>>, SimpleSpan),
}

impl<'a> Expr<'a> {
    pub fn to_verb(&self) -> Option<&'static str> {
        match self {
            Expr::Ident(..) => None,
            Expr::BoolLit(..) => None,
            Expr::CharLit(..) => None,
            Expr::IntLit(..) => None,
            Expr::StrLit(..) => None,
            Expr::Index(..) => Some("index"),
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
            Expr::Inc(..) => Some("increment"),
            Expr::Dec(..) => Some("decrement"),
            Expr::Neg(..) => Some("negate"),
            Expr::Not(..) => Some("logical NOT"),
            Expr::Call(..) => Some("call"),
            Expr::Array(..) => None,
        }
    }

    pub fn get_type(&self) -> Option<Type<'a>> {
        match self {
            Expr::Ident(IdentExpr { symbol, .. }) => match symbol {
                Some(symbol) => Some(symbol.borrow().r#type().clone()),
                None => None,
            },
            Expr::BoolLit(.., span) => Some(Type::Atomic(Atomic::Boolean, *span)),
            Expr::CharLit(.., span) => Some(Type::Atomic(Atomic::Char, *span)),
            Expr::IntLit(.., span) => Some(Type::Atomic(Atomic::Integer, *span)),
            Expr::StrLit(.., span) => Some(Type::Atomic(Atomic::String, *span)),
            Expr::Index(IndexExpr { r#type, .. }) => r#type.clone(),
            Expr::Binary(BinaryExpr { kind, span, left, .. }) => match kind {
                BinaryExprKind::Assign => left.get_type(),
                BinaryExprKind::Add | BinaryExprKind::Sub | BinaryExprKind::Mul
                | BinaryExprKind::Div | BinaryExprKind::Exp | BinaryExprKind::Mod => {
                    Some(Type::Atomic(Atomic::Integer, *span))
                }
                BinaryExprKind::Eq | BinaryExprKind::NotEq | BinaryExprKind::And
                | BinaryExprKind::Or | BinaryExprKind::Less | BinaryExprKind::LessEq
                | BinaryExprKind::Greater | BinaryExprKind::GreaterEq => {
                    Some(Type::Atomic(Atomic::Boolean, *span))
                }
            },
            Expr::Inc(.., span) => Some(Type::Atomic(Atomic::Integer, *span)),
            Expr::Dec(.., span) => Some(Type::Atomic(Atomic::Integer, *span)),
            Expr::Neg(.., span) => Some(Type::Atomic(Atomic::Integer, *span)),
            Expr::Not(.., span) => Some(Type::Atomic(Atomic::Boolean, *span)),
            Expr::Call(CallExpr { symbol, .. }) => match symbol {
                Some(symbol) => Some(symbol.borrow().r#type().clone()),
                None => None,
            },
            Expr::Array(_, r#type, _) => r#type.clone(),

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
    pub r#type: Option<Type<'src>>,
}

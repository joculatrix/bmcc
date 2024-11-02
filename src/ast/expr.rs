use super::*;

/// AST type for B-Minor expressions.
/// 
/// Note: Arithmetic operations in B-Minor can only be performed on integers.
/// 
/// Warning: The `Option<SimpleSpan>` must be the last field in ordered tuples
/// or else the `Spanned` trait will break.
pub enum Expr<'src> {

	/* =========================  Atomic  ======================== */

	/// A variable or function identifier.
	Ident(&'src str, Option<SimpleSpan>),
	/// A boolean literal: `true` or `false`.
	BoolLit(bool, Option<SimpleSpan>),
	/// A char literal. In B-Minor, only ASCII values are acceptable.
	CharLit(char, Option<SimpleSpan>),
	/// A 64-bit integer literal.
	IntLit(i64, Option<SimpleSpan>),
	/// A string literal. In B-Minor, strings are immutable.
	StrLit(&'src str, Option<SimpleSpan>),
	/// An indexed array access.
	Index(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),

	/* ======================== Arithmetic ======================== */
	
	Add(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	Sub(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	Mul(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	Div(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	Exp(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	Mod(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	/// Unary increment.
	Inc(Box<Expr<'src>>, Option<SimpleSpan>),
	/// Unary decrement.
	Dec(Box<Expr<'src>>, Option<SimpleSpan>),
	/// Unary negation.
	Neg(Box<Expr<'src>>, Option<SimpleSpan>),

	/* ========================  Boolean  ======================== */

	Eq(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	NotEq(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	Less(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	LessEq(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	Greater(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	GreaterEq(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	And(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	Or(Box<Expr<'src>>, Box<Expr<'src>>, Option<SimpleSpan>),
	Not(Box<Expr<'src>>, Option<SimpleSpan>),

	/* ========================   Misc   ======================== */

	/// A function call.
	Call(Box<Expr<'src>>, Vec<Expr<'src>>, Option<SimpleSpan>),
	/// An array.
	Array(Vec<Expr<'src>>, Option<SimpleSpan>),
}

impl Spanned for Expr<'_> {
	fn with_span(self, span: SimpleSpan) -> Self {
		match self {
			Expr::Ident(a, _) => Expr::Ident(a, Some(span)),
			Expr::BoolLit(a, _) => Expr::BoolLit(a, Some(span)),
			Expr::CharLit(a, _) => Expr::CharLit(a, Some(span)),
			Expr::IntLit(a, _) => Expr::IntLit(a, Some(span)),
			Expr::StrLit(a, _) => Expr::StrLit(a, Some(span)),
			Expr::Index(a, b, _) => Expr::Index(a, b, Some(span)),
			Expr::Add(a, b, _) => Expr::Add(a, b, Some(span)),
			Expr::Sub(a, b, _) => Expr::Sub(a, b, Some(span)),
			Expr::Mul(a, b, _) => Expr::Mul(a, b, Some(span)),
			Expr::Div(a, b, _) => Expr::Div(a, b, Some(span)),
			Expr::Exp(a, b, _) => Expr::Exp(a, b, Some(span)),
			Expr::Mod(a, b, _) => Expr::Mod(a, b, Some(span)),
			Expr::Inc(a, _) => Expr::Inc(a, Some(span)),
			Expr::Dec(a, _) => Expr::Dec(a, Some(span)),
			Expr::Neg(a, _) => Expr::Neg(a, Some(span)),
			Expr::Eq(a, b, _) => Expr::Eq(a, b, Some(span)),
			Expr::NotEq(a, b, _) => Expr::NotEq(a, b, Some(span)),
			Expr::Less(a, b, _) => Expr::Less(a, b, Some(span)),
			Expr::LessEq(a, b, _) => Expr::LessEq(a, b, Some(span)),
			Expr::Greater(a, b, _) => Expr::Greater(a, b, Some(span)),
			Expr::GreaterEq(a, b, _) => Expr::GreaterEq(a, b, Some(span)),
			Expr::And(a, b, _) => Expr::And(a, b, Some(span)),
			Expr::Or(a, b, _) => Expr::Or(a, b, Some(span)),
			Expr::Not(a, _) => Expr::Not(a, Some(span)),
			Expr::Call(a, b, _) => Expr::Call(a, b, Some(span)),
			Expr::Array(a, _) => Expr::Array(a, Some(span)),
		}
	}

	fn get_span(&self) -> Option<SimpleSpan> {
		match self {
			Expr::Ident(.., s) => *s,
			Expr::BoolLit(.., s) => *s,
			Expr::CharLit(.., s) => *s,
			Expr::IntLit(.., s) => *s,
			Expr::StrLit(.., s) => *s,
			Expr::Index(.., s) => *s,
			Expr::Add(.., s) => *s,
			Expr::Sub(.., s) => *s,
			Expr::Mul(.., s) => *s,
			Expr::Div(.., s) => *s,
			Expr::Exp(.., s) => *s,
			Expr::Mod(.., s) => *s,
			Expr::Inc(.., s) => *s,
			Expr::Dec(.., s) => *s,
			Expr::Neg(.., s) => *s,
			Expr::Eq(.., s) => *s,
			Expr::NotEq(.., s) => *s,
			Expr::Less(.., s) => *s,
			Expr::LessEq(.., s) => *s,
			Expr::Greater(.., s) => *s,
			Expr::GreaterEq(.., s) => *s,
			Expr::And(.., s) => *s,
			Expr::Or(.., s) => *s,
			Expr::Not(.., s) => *s,
			Expr::Call(.., s) => *s,
			Expr::Array(.., s) => *s,
		}
	}
}
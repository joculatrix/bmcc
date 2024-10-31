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
	fn set_span(&mut self, span: SimpleSpan) {
		let s = match self {
			Expr::Ident(.., s) => s,
			Expr::BoolLit(.., s) => s,
			Expr::CharLit(.., s) => s,
			Expr::IntLit(.., s) => s,
			Expr::StrLit(.., s) => s,
			Expr::Index(.., s) => s,
			Expr::Add(.., s) => s,
			Expr::Sub(.., s) => s,
			Expr::Mul(.., s) => s,
			Expr::Div(.., s) => s,
			Expr::Exp(.., s) => s,
			Expr::Mod(.., s) => s,
			Expr::Inc(.., s) => s,
			Expr::Dec(.., s) => s,
			Expr::Neg(.., s) => s,
			Expr::Eq(.., s) => s,
			Expr::NotEq(.., s) => s,
			Expr::Less(.., s) => s,
			Expr::LessEq(.., s) => s,
			Expr::Greater(.., s) => s,
			Expr::GreaterEq(.., s) => s,
			Expr::And(.., s) => s,
			Expr::Or(.., s) => s,
			Expr::Not(.., s) => s,
			Expr::Call(.., s) => s,
			Expr::Array(.., s) => s,
		};

		*s = Some(span);
	}
}
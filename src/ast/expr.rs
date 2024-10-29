/// AST type for B-Minor expressions.
/// 
/// Note: Arithmetic operations in B-Minor can only be performed on integers.
pub enum Expr<'src> {

	/* =========================  Atomic  ======================== */

	/// A variable or function identifier.
	Ident(&'src str),
	/// A boolean literal: `true` or `false`.
	BoolLit(bool),
	/// A char literal. In B-Minor, only ASCII values are acceptable.
	CharLit(char),
	/// A 64-bit integer literal.
	IntLit(i64),
	/// A string literal. In B-Minor, strings are immutable.
	StrLit(&'src str),
	/// An indexed array access.
	Index(Box<Expr<'src>>, Box<Expr<'src>>),

	/* ======================== Arithmetic ======================== */
	
	Add(Box<Expr<'src>>, Box<Expr<'src>>),
	Sub(Box<Expr<'src>>, Box<Expr<'src>>),
	Mul(Box<Expr<'src>>, Box<Expr<'src>>),
	Div(Box<Expr<'src>>, Box<Expr<'src>>),
	Exp(Box<Expr<'src>>, Box<Expr<'src>>),
	Mod(Box<Expr<'src>>, Box<Expr<'src>>),
	/// Unary increment.
	Inc(Box<Expr<'src>>),
	/// Unary decrement.
	Dec(Box<Expr<'src>>),
	/// Unary negation.
	Neg(Box<Expr<'src>>),

	/* ========================  Boolean  ======================== */

	Eq(Box<Expr<'src>>, Box<Expr<'src>>),
	NotEq(Box<Expr<'src>>, Box<Expr<'src>>),
	Less(Box<Expr<'src>>, Box<Expr<'src>>),
	LessEq(Box<Expr<'src>>, Box<Expr<'src>>),
	Greater(Box<Expr<'src>>, Box<Expr<'src>>),
	GreaterEq(Box<Expr<'src>>, Box<Expr<'src>>),
	And(Box<Expr<'src>>, Box<Expr<'src>>),
	Or(Box<Expr<'src>>, Box<Expr<'src>>),
	Not(Box<Expr<'src>>),

	/* ========================   Misc   ======================== */

	/// A function call.
	Call(Box<Expr<'src>>, Vec<Expr<'src>>),
	/// An array.
	Array(Vec<Expr<'src>>),
}
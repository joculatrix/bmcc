use chumsky::span::SimpleSpan;
use crate::ast::Spanned;

/// Warning: The `Option<SimpleSpan>` must be the last field in ordered tuples
/// or else the `Spanned` trait will break.
pub enum Token<'src> {
	/// `[`
	BraceLeft(Option<SimpleSpan>),
	/// `]`
	BraceRight(Option<SimpleSpan>),
	/// `:`
	Colon(Option<SimpleSpan>),
	/// `{`
	CurlyLeft(Option<SimpleSpan>),
	/// `}`
	CurlyRight(Option<SimpleSpan>),
	/// variable or function identifier
	Ident(&'src str, Option<SimpleSpan>),
	/// ASCII char
	LitChar(char, Option<SimpleSpan>),
	/// 64-bit int literal
	LitInt(i64, Option<SimpleSpan>),
	/// immutable string value
	LitString(&'src str, Option<SimpleSpan>),
	/// keyword `array`
	KwArray(Option<SimpleSpan>),
	/// keyword `boolean`
	KwBoolean(Option<SimpleSpan>),
	/// keyword `char`
	KwChar(Option<SimpleSpan>),
	/// keyword `else`
	KwElse(Option<SimpleSpan>),
	/// keyword `false`
	KwFalse(Option<SimpleSpan>),
	/// keyword `for`
	KwFor(Option<SimpleSpan>),
	/// keyword `function`
	KwFunction(Option<SimpleSpan>),
	/// keyword `if`
	KwIf(Option<SimpleSpan>),
	/// keyword `integer`
	KwInteger(Option<SimpleSpan>),
	/// keyword `print`
	KwPrint(Option<SimpleSpan>),
	/// keyword `return`
	KwReturn(Option<SimpleSpan>),
	/// keyword `string`
	KwString(Option<SimpleSpan>),
	/// keyword `true`
	KwTrue(Option<SimpleSpan>),
	/// keyword `void`
	KwVoid(Option<SimpleSpan>),
	/// `+`
	OpAdd(Option<SimpleSpan>),
	/// `&&`
	OpAnd(Option<SimpleSpan>),
	/// `=`
	OpAssign(Option<SimpleSpan>),
	/// `--`
	OpDec(Option<SimpleSpan>),
	/// `/`
	OpDiv(Option<SimpleSpan>),
	/// `==`
	OpEq(Option<SimpleSpan>),
	/// `^`
	OpExp(Option<SimpleSpan>),
	/// `>`
	OpGreater(Option<SimpleSpan>),
	/// `>=`
	OpGreaterEq(Option<SimpleSpan>),
	/// `++`
	OpInc(Option<SimpleSpan>),
	/// `<`
	OpLess(Option<SimpleSpan>),
	/// `<=`
	OpLessEq(Option<SimpleSpan>),
	/// `%`
	OpMod(Option<SimpleSpan>),
	/// `*`
	OpMul(Option<SimpleSpan>),
	/// `!`
	OpNot(Option<SimpleSpan>),
	/// `!=`
	OpNotEq(Option<SimpleSpan>),
	/// `||`
	OpOr(Option<SimpleSpan>),
	/// `-`
	OpSub(Option<SimpleSpan>),
	/// `(`
	ParenLeft(Option<SimpleSpan>),
	/// `)`
	ParenRight(Option<SimpleSpan>),
	/// `;`
	Semicolon(Option<SimpleSpan>),
}

impl Spanned for Token<'_> {
	fn set_span(&mut self, span: SimpleSpan) {
		let s = match self {
			Token::BraceLeft(.., s) => s,
			Token::BraceRight(.., s) => s,
			Token::Colon(.., s) => s,
			Token::CurlyLeft(.., s) => s,
			Token::CurlyRight(.., s) => s,
			Token::Ident(.., s) => s,
			Token::LitChar(.., s) => s,
			Token::LitInt(.., s) => s,
			Token::LitString(.., s) => s,
			Token::KwArray(.., s) => s,
			Token::KwBoolean(.., s) => s,
			Token::KwChar(.., s) => s,
			Token::KwElse(.., s) => s,
			Token::KwFalse(.., s) => s,
			Token::KwFor(.., s) => s,
			Token::KwFunction(.., s) => s,
			Token::KwIf(.., s) => s,
			Token::KwInteger(.., s) => s,
			Token::KwPrint(.., s) => s,
			Token::KwReturn(.., s) => s,
			Token::KwString(.., s) => s,
			Token::KwTrue(.., s) => s,
			Token::KwVoid(.., s) => s,
			Token::OpAdd(.., s) => s,
			Token::OpAnd(.., s) => s,
			Token::OpAssign(.., s) => s,
			Token::OpDec(.., s) => s,
			Token::OpDiv(.., s) => s,
			Token::OpEq(.., s) => s,
			Token::OpExp(.., s) => s,
			Token::OpGreater(.., s) => s,
			Token::OpGreaterEq(.., s) => s,
			Token::OpInc(.., s) => s,
			Token::OpLess(.., s) => s,
			Token::OpLessEq(.., s) => s,
			Token::OpMod(.., s) => s,
			Token::OpMul(.., s) => s,
			Token::OpNot(.., s) => s,
			Token::OpNotEq(.., s) => s,
			Token::OpOr(.., s) => s,
			Token::OpSub(.., s) => s,
			Token::ParenLeft(.., s) => s,
			Token::ParenRight(.., s) => s,
			Token::Semicolon(.., s) => s,
		};

		*s = Some(span);
	}
}
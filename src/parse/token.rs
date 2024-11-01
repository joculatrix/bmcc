use chumsky::{error::Simple, span::SimpleSpan};
use crate::ast::Spanned;

#[derive(Clone)]
/// Warning: The `Option<SimpleSpan>` must be the last field in ordered tuples
/// or else the `Spanned` trait will break.
pub enum Token<'src> {
	/// `[`
	BraceLeft(Option<SimpleSpan>),
	/// `]`
	BraceRight(Option<SimpleSpan>),
	/// ','
	Comma(Option<SimpleSpan>),
	/// `:`
	Colon(Option<SimpleSpan>),
	/// `{`
	CurlyLeft(Option<SimpleSpan>),
	/// `}`
	CurlyRight(Option<SimpleSpan>),
	/// invalid input
	Error(Option<SimpleSpan>),
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
	fn with_span(self, span: SimpleSpan) -> Self {
		match self {
			Token::BraceLeft(_) => Token::BraceLeft(Some(span)),
			Token::BraceRight(simple_span) => todo!(),
			Token::Comma(simple_span) => todo!(),
			Token::Colon(simple_span) => todo!(),
			Token::CurlyLeft(simple_span) => todo!(),
			Token::CurlyRight(simple_span) => todo!(),
			Token::Error(simple_span) => todo!(),
			Token::Ident(_, simple_span) => todo!(),
			Token::LitChar(_, simple_span) => todo!(),
			Token::LitInt(_, simple_span) => todo!(),
			Token::LitString(_, simple_span) => todo!(),
			Token::KwArray(simple_span) => todo!(),
			Token::KwBoolean(simple_span) => todo!(),
			Token::KwChar(simple_span) => todo!(),
			Token::KwElse(simple_span) => todo!(),
			Token::KwFalse(simple_span) => todo!(),
			Token::KwFor(simple_span) => todo!(),
			Token::KwFunction(simple_span) => todo!(),
			Token::KwIf(simple_span) => todo!(),
			Token::KwInteger(simple_span) => todo!(),
			Token::KwPrint(simple_span) => todo!(),
			Token::KwReturn(simple_span) => todo!(),
			Token::KwString(simple_span) => todo!(),
			Token::KwTrue(simple_span) => todo!(),
			Token::KwVoid(simple_span) => todo!(),
			Token::OpAdd(simple_span) => todo!(),
			Token::OpAnd(simple_span) => todo!(),
			Token::OpAssign(simple_span) => todo!(),
			Token::OpDec(simple_span) => todo!(),
			Token::OpDiv(simple_span) => todo!(),
			Token::OpEq(simple_span) => todo!(),
			Token::OpExp(simple_span) => todo!(),
			Token::OpGreater(simple_span) => todo!(),
			Token::OpGreaterEq(simple_span) => todo!(),
			Token::OpInc(simple_span) => todo!(),
			Token::OpLess(simple_span) => todo!(),
			Token::OpLessEq(simple_span) => todo!(),
			Token::OpMod(simple_span) => todo!(),
			Token::OpMul(simple_span) => todo!(),
			Token::OpNot(simple_span) => todo!(),
			Token::OpNotEq(simple_span) => todo!(),
			Token::OpOr(simple_span) => todo!(),
			Token::OpSub(simple_span) => todo!(),
			Token::ParenLeft(simple_span) => todo!(),
			Token::ParenRight(simple_span) => todo!(),
			Token::Semicolon(simple_span) => todo!(),
		}
	}
}
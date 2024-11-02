use chumsky::span::SimpleSpan;
use crate::ast::Spanned;

#[derive(Clone, PartialEq)]
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
	/// keywords
	Keyword(Keyword, Option<SimpleSpan>),
	/// operators
	Operator(Op, Option<SimpleSpan>),
	/// `(`
	ParenLeft(Option<SimpleSpan>),
	/// `)`
	ParenRight(Option<SimpleSpan>),
	/// `;`
	Semicolon(Option<SimpleSpan>),
}

#[derive(Clone, PartialEq)]
pub enum Keyword {
	Array,
	Boolean,
	Char,
	Else,
	False,
	For,
	Function,
	If,
	Integer,
	Print,
	Return,
	r#String,
	True,
	Void,
	While,
}

#[derive(Clone, PartialEq)]
pub enum Op {
	/// `+`
	Add,
	/// `&&`
	And,
	/// `=`
	Assign,
	/// `--`
	Dec,
	/// `/`
	Div,
	/// `==`
	Eq,
	/// `^`
	Exp,
	/// `>`
	Greater,
	/// `>=`
	GreaterEq,
	/// `++`
	Inc,
	/// `<`
	Less,
	/// `<=`
	LessEq,
	/// `%`
	Mod,
	/// `*`
	Mul,
	/// `!`
	Not,
	/// `!=`
	NotEq,
	/// `||`
	Or,
	/// `-`
	Sub,
}

impl std::fmt::Display for Token<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Token::BraceLeft(..) => write!(f, "`[`"),
			Token::BraceRight(..) => write!(f, "`]`"),
			Token::Comma(..) => write!(f, "`,`"),
			Token::Colon(..) => write!(f, "`:`"),
			Token::CurlyLeft(..) => write!(f, "`{{`"),
			Token::CurlyRight(..) => write!(f, "`}}`"),
			Token::Error(..) => write!(f, "ERROR"),
			Token::Ident(..) => write!(f, "identifier"),
			Token::LitChar(..) => write!(f, "char literal"),
			Token::LitInt(..) => write!(f, "integer literal"),
			Token::LitString(..) => write!(f, "string literal"),
			Token::Keyword(k, ..) => match k {
				Keyword::Array => write!(f, "`array`"),
				Keyword::Boolean => write!(f, "`boolean`"),
				Keyword::Char => write!(f, "`char`"),
				Keyword::Else => write!(f, "`else`"),
				Keyword::False => write!(f, "`false`"),
				Keyword::For => write!(f, "`for`"),
				Keyword::Function => write!(f, "`function`"),
				Keyword::If => write!(f, "`if`"),
				Keyword::Integer => write!(f, "`integer`"),
				Keyword::Print => write!(f, "`print`"),
				Keyword::Return => write!(f, "`return`"),
				Keyword::r#String => write!(f, "`string`"),
				Keyword::True => write!(f, "`true`"),
				Keyword::Void => write!(f, "`void`"),
				Keyword::While => write!(f, "`while`"),
			}
			Token::Operator(op, ..) => match op {
				Op::Add => write!(f, "`+`"),
				Op::And => write!(f, "`&&`"),
				Op::Assign => write!(f, "`=`"),
				Op::Dec => write!(f, "`--`"),
				Op::Div => write!(f, "`/`"),
				Op::Eq => write!(f, "`==`"),
				Op::Exp => write!(f, "`^`"),
				Op::Greater => write!(f, "`>`"),
				Op::GreaterEq => write!(f, "`>=`"),
				Op::Inc => write!(f, "`++`"),
				Op::Less => write!(f, "`<`"),
				Op::LessEq => write!(f, "`<=`"),
				Op::Mod => write!(f, "`%`"),
				Op::Mul => write!(f, "`*`"),
				Op::Not => write!(f, "`!`"),
				Op::NotEq => write!(f, "`!=`"),
				Op::Or => write!(f, "`||`"),
				Op::Sub => write!(f, "`-`"),
			},
			Token::ParenLeft(..) => write!(f, "`(`"),
			Token::ParenRight(..) => write!(f, "`)`"),
			Token::Semicolon(..) => write!(f, "`;`"),
		}
	}
}

impl Spanned for Token<'_> {
	fn with_span(self, span: SimpleSpan) -> Self {
		match self {
			Token::BraceLeft(_) => Token::BraceLeft(Some(span)),
			Token::BraceRight(_) => Token::BraceRight(Some(span)),
			Token::Comma(_) => Token::Comma(Some(span)),
			Token::Colon(_) => Token::Colon(Some(span)),
			Token::CurlyLeft(_) => Token::CurlyLeft(Some(span)),
			Token::CurlyRight(_) => Token::CurlyRight(Some(span)),
			Token::Error(_) => Token::Error(Some(span)),
			Token::Ident(a, _) => Token::Ident(a, Some(span)),
			Token::LitChar(a, _) => Token::LitChar(a, Some(span)),
			Token::LitInt(a, _) => Token::LitInt(a, Some(span)),
			Token::LitString(a, _) => Token::LitString(a, Some(span)),
			Token::Keyword(a, _) => Token::Keyword(a, Some(span)),
			Token::Operator(a, _) => Token::Operator(a, Some(span)),
			Token::ParenLeft(_) => Token::ParenLeft(Some(span)),
			Token::ParenRight(_) => Token::ParenRight(Some(span)),
			Token::Semicolon(_) => Token::Semicolon(Some(span)),
		}
	}

	fn get_span(&self) -> Option<SimpleSpan> {
		match self {
			Token::BraceLeft(.., s) => *s,
			Token::BraceRight(.., s) => *s,
			Token::Comma(.., s) => *s,
			Token::Colon(.., s) => *s,
			Token::CurlyLeft(.., s) => *s,
			Token::CurlyRight(.., s) => *s,
			Token::Error(.., s) => *s,
			Token::Ident(.., s) => *s,
			Token::LitChar(.., s) => *s,
			Token::LitInt(.., s) => *s,
			Token::LitString(.., s) => *s,
			Token::Keyword(.., s) => *s,
			Token::Operator(.., s) => *s,
			Token::ParenLeft(.., s) => *s,
			Token::ParenRight(.., s) => *s,
			Token::Semicolon(.., s) => *s,
		}
	}
}
use super::*;
use extra::Err;

use Keyword::*;

pub fn lex<'src>()
-> impl Parser<'src, &'src str, Vec<Token<'src>>, Err<Rich<'src, char>>> {
	// handles escaped characters
	let escape = just('\\').ignore_then(
		choice((
			just('n').to('\n'),
			just('b').to('\u{0008}'),
			just('t').to('\t'),
			just('f').to('\u{000C}'),
			just('0').to('\0'),
			any()
		))
	);

	let string_lit = any()
		.and_is(just('"').not())
		.filter(|c| *c != '\\').or(escape)
		.validate(|c, e, emitter| {
			if !c.is_ascii() {
				emitter.emit(Rich::custom(
					e.span(),
					format!("`{}` is not a valid ASCII character", c)
				))
			}
			c
		})
		.repeated()
		.to_slice()
		.validate(|s: &str, e, emitter| {
			if s.len() == 0 {
				emitter.emit(Rich::custom(
					e.span(),
					"string literal cannot be empty"
				))
			}
			s
		})
		.delimited_by(just('"'), just('"'))
		.padded()
		.map_with(|s, e| Token::LitString(s, Some(e.span())));

	let char_lit = any()
		.and_is(just('\'').not())
		.filter(|c| *c != '\\').or(escape)
		.validate(|c, e, emitter| {
			if !c.is_ascii() {
				emitter.emit(Rich::custom(
					e.span(),
					format!("`{}` is not a valid ASCII character", c)
				))
			}
			c
		})
		.delimited_by(just('\''), just('\''))
		.padded()
		.map_with(|c, e| Token::LitChar(c, Some(e.span())));
	
	let comment = choice((
		just("//")
			.then(
				any().and_is(just('\n').not()).repeated()
			)
			.padded()
			.ignored(),
		any()
			.and_is(just("*/").not())
			.repeated()
			.delimited_by(just("/*"), just("*/"))
			.ignored(),
	));

	let int_lit = text::int(10)
		.validate(|s: &str, e, emitter| {
			let i = s.parse::<i64>().unwrap_or_else(|_| {
				emitter.emit(Rich::custom(
					e.span(),
					format!("`{}` is not a valid 64-bit signed integer", s)
				));
				0
			});
			Token::LitInt(i, Some(e.span()))
		});

	let ident = text::ascii::ident().map(|ident| match ident {
		"array" => Token::Keyword(Array, None),
		"boolean" => Token::Keyword(Boolean, None),
		"char" => Token::Keyword(Char, None),
		"else" => Token::Keyword(Else, None),
		"false" => Token::Keyword(False, None),
		"for" => Token::Keyword(For, None),
		"function" => Token::Keyword(Function, None),
		"if" => Token::Keyword(If, None),
		"integer" => Token::Keyword(Integer, None),
		"print" => Token::Keyword(Print, None),
		"return" => Token::Keyword(Return, None),
		"string" => Token::Keyword(r#String, None),
		"true" => Token::Keyword(True, None),
		"void" => Token::Keyword(Void, None),
		_ => Token::Ident(ident, None),
	})
		.map_with(|t, e| t.with_span(e.span()));

	let punctuation = choice((
		just('[').to(Token::BraceLeft(None)),
		just(']').to(Token::BraceRight(None)),
		just(',').to(Token::Comma(None)),
		just(':').to(Token::Colon(None)),
		just('{').to(Token::CurlyLeft(None)),
		just('}').to(Token::CurlyRight(None)),
		just('(').to(Token::ParenLeft(None)),
		just(')').to(Token::ParenRight(None)),
		just(';').to(Token::Semicolon(None)),
	))
		.map_with(|t, e| t.with_span(e.span()));

	let op = choice((
		just("==").to(Token::Operator(Op::Eq, None)),
		just("=").to(Token::Operator(Op::Assign, None)),
		just("!=").to(Token::Operator(Op::NotEq, None)),
		just("!").to(Token::Operator(Op::Not, None)),
		just("<=").to(Token::Operator(Op::LessEq, None)),
		just("<").to(Token::Operator(Op::Less, None)),
		just(">=").to(Token::Operator(Op::GreaterEq, None)),
		just(">").to(Token::Operator(Op::Greater, None)),
		just("&&").to(Token::Operator(Op::And, None)),
		just("||").to(Token::Operator(Op::Or, None)),
		just("++").to(Token::Operator(Op::Inc, None)),
		just("+").to(Token::Operator(Op::Add, None)),
		just("--").to(Token::Operator(Op::Dec, None)),
		just("-").to(Token::Operator(Op::Sub, None)),
		just("*").to(Token::Operator(Op::Mul, None)),
		just("/").to(Token::Operator(Op::Div, None)),
		just("^").to(Token::Operator(Op::Exp, None)),
		just("%").to(Token::Operator(Op::Mod, None)),
	))
		.map_with(|t, e| t.with_span(e.span()));

	choice((
		string_lit,
		char_lit,
		int_lit,
		op,
		punctuation,
		ident,
	))
		.or(any().map_with(|_, e| Token::Error(Some(e.span()))))
		.padded()
		.padded_by(comment)
		.repeated()
		.collect()
		.padded()
		.then_ignore(end())
}
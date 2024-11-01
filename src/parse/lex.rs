use super::*;
use extra::Err;

fn lex<'src>()
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
		"array" => Token::KwArray(None),
		"boolean" => Token::KwBoolean(None),
		"char" => Token::KwChar(None),
		"else" => Token::KwElse(None),
		"false" => Token::KwFalse(None),
		"for" => Token::KwFor(None),
		"function" => Token::KwFunction(None),
		"if" => Token::KwIf(None),
		"integer" => Token::KwInteger(None),
		"print" => Token::KwPrint(None),
		"return" => Token::KwReturn(None),
		"string" => Token::KwString(None),
		"true" => Token::KwTrue(None),
		"void" => Token::KwVoid(None),
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
		just("==").to(Token::OpEq(None)),
		just("=").to(Token::OpAssign(None)),
		just("!=").to(Token::OpNotEq(None)),
		just("!").to(Token::OpNot(None)),
		just("<=").to(Token::OpLessEq(None)),
		just("<").to(Token::OpLess(None)),
		just(">=").to(Token::OpGreaterEq(None)),
		just(">").to(Token::OpGreater(None)),
		just("&&").to(Token::OpAdd(None)),
		just("||").to(Token::OpOr(None)),
		just("++").to(Token::OpInc(None)),
		just("+").to(Token::OpAdd(None)),
		just("--").to(Token::OpDec(None)),
		just("-").to(Token::OpSub(None)),
		just("*").to(Token::OpMul(None)),
		just("/").to(Token::OpDiv(None)),
		just("^").to(Token::OpExp(None)),
		just("%").to(Token::OpMod(None)),
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
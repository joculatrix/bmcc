use super::*;

use Keyword::*;
use Op::*;

pub fn lex<'src>()
-> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, Err<Rich<'src, char>>>
{
    // handles escaped characters
    let escape = just('\\').ignore_then(choice((
        just('n').to('\n'),
        just('b').to('\u{0008}'),
        just('t').to('\t'),
        just('f').to('\u{000C}'),
        just('0').to('\0'),
        any(),
    )));

    let string_lit = any()
        .and_is(just('"').not())
        .filter(|c| *c != '\\')
        .or(escape)
        .validate(|c, e, emitter| {
            if !c.is_ascii() {
                emitter.emit(Rich::custom(
                    e.span(),
                    format!("`{}` is not a valid ASCII character", c),
                ))
            }
            c
        })
        .repeated()
        .collect::<std::string::String>()
        .validate(|s: std::string::String, e, emitter| {
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
        .map_with(|s, e| (Token::LitString(s), e.span()));

    let char_lit = any()
        .and_is(just('\'').not())
        .filter(|c| *c != '\\')
        .or(escape)
        .validate(|c, e, emitter| {
            if !c.is_ascii() {
                emitter.emit(Rich::custom(
                    e.span(),
                    format!("`{}` is not a valid ASCII character", c),
                ))
            }
            c
        })
        .delimited_by(just('\''), just('\''))
        .padded()
        .map_with(|c, e| (Token::LitChar(c), e.span()));

    let comment = choice((
        just("//")
            .then(any().and_is(just('\n').not()).repeated())
            .padded()
            .ignored(),
        any()
            .and_is(just("*/").not())
            .repeated()
            .delimited_by(just("/*"), just("*/"))
            .ignored(),
    ));

    let int_lit = text::int(10).validate(|s: &str, e, emitter| {
        let i = s.parse::<i64>().unwrap_or_else(|_| {
            emitter.emit(Rich::custom(
                e.span(),
                format!("`{}` is not a valid 64-bit signed integer", s),
            ));
            0
        });
        (Token::LitInt(i), e.span())
    });

    let ident = text::ascii::ident()
        .map(|ident| match ident {
            "array" => Token::Keyword(Array),
            "boolean" => Token::Keyword(Boolean),
            "char" => Token::Keyword(Char),
            "else" => Token::Keyword(Else),
            "false" => Token::Keyword(False),
            "for" => Token::Keyword(For),
            "function" => Token::Keyword(Function),
            "if" => Token::Keyword(If),
            "integer" => Token::Keyword(Integer),
            "print" => Token::Keyword(Print),
            "return" => Token::Keyword(Return),
            "string" => Token::Keyword(r#String),
            "true" => Token::Keyword(True),
            "void" => Token::Keyword(Void),
            "while" => Token::Keyword(While),
            _ => Token::Ident(ident),
        })
        .map_with(|t, e| (t, e.span()));

    let punctuation = choice((
        just('[').to(Token::BraceLeft),
        just(']').to(Token::BraceRight),
        just(',').to(Token::Comma),
        just(':').to(Token::Colon),
        just('{').to(Token::CurlyLeft),
        just('}').to(Token::CurlyRight),
        just('(').to(Token::ParenLeft),
        just(')').to(Token::ParenRight),
        just(';').to(Token::Semicolon),
    ))
    .map_with(|t, e| (t, e.span()));

    let op = choice((
        just("==").to(Token::Operator(Eq)),
        just("=").to(Token::Operator(Assign)),
        just("!=").to(Token::Operator(NotEq)),
        just("!").to(Token::Operator(Not)),
        just("<=").to(Token::Operator(LessEq)),
        just("<").to(Token::Operator(Less)),
        just(">=").to(Token::Operator(GreaterEq)),
        just(">").to(Token::Operator(Greater)),
        just("&&").to(Token::Operator(And)),
        just("||").to(Token::Operator(Or)),
        just("++").to(Token::Operator(Inc)),
        just("+").to(Token::Operator(Add)),
        just("--").to(Token::Operator(Dec)),
        just("-").to(Token::Operator(Sub)),
        just("*").to(Token::Operator(Mul)),
        just("/").to(Token::Operator(Div)),
        just("^").to(Token::Operator(Exp)),
        just("%").to(Token::Operator(Mod)),
    ))
    .map_with(|t, e| (t, e.span()));

    choice((string_lit, char_lit, int_lit, ident, op, punctuation))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

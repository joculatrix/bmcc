use super::*;
use chumsky::input::Stream;
use extra::Err;

/// Convenience macro to ignore filler tokens the AST doesn't care about.
/// 
/// # Examples
/// 
/// ```ignore
/// let parser = discard!(Token::Semicolon(..));
/// ```
/// 
/// expands to:
/// 
/// ```ignore
/// let parser =
/// 	custom(|input| {
/// 		let Some(token) = input.next() else {
/// 			return Err(Rich::custom(SimpleSpan::splat(0), "unexpected EOF"));
/// 		};
/// 	
/// 		match token {
/// 			Token::Semicolon(..) => Ok(()),
/// 			_ => {
/// 				let _span = match token.get_span() {
/// 					Some(span) => span,
/// 					None => SimpleSpan::splat(0),
/// 				};
/// 				todo!();
/// 			}
/// 		}
/// 	})
/// 	.ignored()
/// ```
macro_rules! discard {
	($p:pat) => {
		custom(|input| {
			let Some(token) = input.next() else {
				return Err(Rich::custom(SimpleSpan::splat(0), "unexpected EOF"));
			};

			match token {
				$p => Ok(()),
				_ => {
					let _span = match token.get_span() {
						Some(span) => span,
						None => SimpleSpan::splat(0),
					};
					// figure out how to construct a useful error from within
					// the macro...
					todo!();
				}
			}
		})
		.ignored()
	};
}

pub fn parser<'src>()
-> impl Parser<'src, &[Token<'src>], Vec<Decl>, Err<Rich<'src, Token<'src>>>> {
	let decl = choice((

	))
}

fn decl_var<'src>()
-> impl Parser<'src, &'src [Token<'src>], Decl<'src>, Err<Rich<'src, Token<'src>>>> {
	let ident = select! { Token::Ident(id, _span) => id }
		.labelled("identifier");

	let val = expr().labelled("value");

	ident
		.then_ignore(discard!(Token::Colon(..)))
		.then(r#type())
		.then(
			choice((
				discard!(Token::Operator(Op::Assign, ..))
					.then(val)
					.then(discard!(Token::Semicolon(..)))
					.map(|((_, e), _)| Some(e)),
				discard!(Token::Semicolon(..))
					.map(|_| None),
			))
		)
		.map_with(|((ident, r#type), val), e|
			Decl::Var(decl::Var {
				name: ident,
				r#type: Box::new(r#type),
				rhs: val,
				span: Some(e.span()),
			})
		)
}

fn r#type<'src>()
-> impl Parser<'src, &[Token<'src>], Type<'src>, Err<Rich<'src, Token<'src>>>> {

}

fn expr<'src>()
-> impl Parser<'src, &[Token<'src>], Expr<'src>, Err<Rich<'src, Token<'src>>>> {

}
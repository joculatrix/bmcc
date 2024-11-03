use super::*;
use chumsky::input::Stream;
use decl::Function;
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
-> impl Parser<'src, &'src [Token<'src>], Vec<Decl>, Err<Rich<'src, Token<'src>>>> {
	let decl = choice((

	))
}

fn ident<'src>()
-> impl Parser<'src, &'src [Token<'src>], &'src str, Err<Rich<'src, Token<'src>>>> {
	select!{ Token::Ident(id, _span) => id }.labelled("identifier")
}

fn decl_func<'src>()
-> impl Parser<'src, &'src [Token<'src>], Decl<'src>, Err<Rich<'src, Token<'src>>>> {
	let params_list = ident()
		.then_ignore(discard!(Token::Colon(..)))
		.then(r#type())
		.separated_by(discard!(Token::Comma(..)))
		.collect::<Vec<_>>()
		.delimited_by(
			discard!(Token::ParenLeft(..)),
			discard!(Token::ParenRight(..))
		)
		.labelled("parameters");
	
	let fn_type = discard!(Token::Keyword(Keyword::Function, ..))
		.ignore_then(r#type())
		.then(params_list)
		.map(|(r#type, params)| {
			let span = r#type.get_span().clone();
			Type::Function(
				Box::new(r#type),
				params,
				span,
			)
		})
		.labelled("function type");
	
	ident()
		.then(fn_type)
		.then(
			choice((
				discard!(Token::Semicolon(..))
					.ignored()
					.map(|_| None),
				discard!(Token::Operator(Op::Assign, ..))
					.ignore_then(stmt())
					.map(|s| Some(s)),
			))
		)
		.map(|((ident, r#type), body)| {
			let type_span = r#type.get_span().unwrap();

			let end = match r#type {
				Type::Function(_, ref params, _) => {
					if let Some(param) = params.last() {
						param.1.get_span().unwrap().end
					} else {
						type_span.end
					}
				}
				_ => type_span.end
			};

			let span = Some(SimpleSpan::new(type_span.start, end));

			Decl::Function(
				Function {
					name: ident,
					r#type: Box::new(r#type),
					rhs: body,
					span,
				}
			)
		})
		.labelled("function declaration")
}

fn decl_var<'src>()
-> impl Parser<'src, &'src [Token<'src>], Decl<'src>, Err<Rich<'src, Token<'src>>>> {
	let val = expr().labelled("value");

	ident()
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
		.labelled("variable declaration")
}

fn r#type<'src>()
-> impl Parser<'src, &'src [Token<'src>], Type<'src>, Err<Rich<'src, Token<'src>>>> {

}

fn expr<'src>()
-> impl Parser<'src, &'src [Token<'src>], Expr<'src>, Err<Rich<'src, Token<'src>>>> {

}

fn stmt<'src>()
-> impl Parser<'src, &'src [Token<'src>], Stmt<'src>, Err<Rich<'src, Token<'src>>>> {

}
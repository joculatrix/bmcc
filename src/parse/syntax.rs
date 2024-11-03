use super::*;
use chumsky::input::Stream;
use decl::Function;
use extra::Err;


pub fn parser<'src>()
-> impl Parser<'src, &'src [Token<'src>], Vec<Decl>, Err<Rich<'src, Token<'src>>>> {
	let decl = choice((

	))
}

fn ident<'src>()
-> impl Parser<'src, &'src [Token<'src>], &'src str, Err<Rich<'src, Token<'src>>>> {
	select!{ Token::Ident(id, _span) => id }.labelled("identifier")
}

/// DECL_FN     ::= ident `:` FN_TYPE `;`
///               | ident `:` FN_TYPE `=` STMT
/// 
/// FN_TYPE     ::= `function` VAR_TYPE PARAMS_LIST
/// 
/// PARAMS_LIST ::= `(` (ident `:` VAR_TYPE (`,` ident `:` VAR_TYPE )*)? `)`
fn decl_fn<'src>()
-> impl Parser<'src, &'src [Token<'src>], Decl<'src>, Err<Rich<'src, Token<'src>>>> {
	// PARAMS_LIST ::= `(` (ident `:` VAR_TYPE (`,` ident `:` VAR_TYPE )*)? `)`
	let params_list = ident()
		.then_ignore(select!{ Token::Colon(..) => () })
		.then(r#type())
		.separated_by(select!{ Token::Comma(..) => () })
		.collect::<Vec<_>>()
		.delimited_by(
			select!{ Token::ParenLeft(..) => () },
			select!{ Token::ParenRight(..) => () }
		)
		.labelled("parameters");
	
	// FN_TYPE ::= `function` VAR_TYPE PARAMS_LIST
	let fn_type = select!{ Token::Keyword(Keyword::Function, ..) => () }
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
				// DECL_FN ::= ident `:` FN_TYPE `;`
				select!{ Token::Semicolon(..) => () }
					.ignored()
					.map(|_| None),
				// DECL_FN ::= ident `:` FN_TYPE `=` STMT
				select!{ Token::Operator(Op::Assign, ..) => () }
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

/// DECL_VAR ::= ident `:` VAR_TYPE `=` EXPR `;`
///            | ident `:` VAR_TYPE `;`
fn decl_var<'src>()
-> impl Parser<'src, &'src [Token<'src>], Decl<'src>, Err<Rich<'src, Token<'src>>>> {
	let val = expr().labelled("value");

	ident()
		.then_ignore(select!{ Token::Colon(..) => () })
		.then(r#type())
		.then(
			choice((
				// DECL_VAR ::= ident `:` VAR_TYPE `=` EXPR `;`
				select!{ Token::Operator(Op::Assign, ..) => () }
					.then(val)
					.then(select!{ Token::Semicolon(..) => () })
					.map(|((_, e), _)| Some(e)),
				// DECL_VAR ::= ident `:` VAR_TYPE `;`
				select!{ Token::Semicolon(..) => () }
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
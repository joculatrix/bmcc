use super::*;
use decl::Function;
use extra::Err;

/// PROGRAM ::= DECL*
pub fn parser<'src>()
-> impl Parser<'src, &'src [Token<'src>], Vec<Decl>, Err<Rich<'src, Token<'src>>>> {
	let decl = choice((

	))
}

fn ident<'src>()
-> impl Parser<'src, &'src [Token<'src>], &'src str, Err<Rich<'src, Token<'src>>>>
+ Clone {
	select!{ Token::Ident(id, _span) => id }.labelled("identifier")
}

/// DECL_FN     ::= ident `:` FN_TYPE `;`
///             |   ident `:` FN_TYPE `=` STMT
/// 
/// FN_TYPE     ::= `function` VAR_TYPE PARAMS_LIST
/// 
/// PARAMS_LIST ::= `(` (ident `:` VAR_TYPE (`,` ident `:` VAR_TYPE )*)? `)`
fn decl_fn<'src>()
-> impl Parser<'src, &'src [Token<'src>], Decl<'src>, Err<Rich<'src, Token<'src>>>> {
	// PARAMS_LIST ::= `(` (ident `:` VAR_TYPE (`,` ident `:` VAR_TYPE )*)? `)`
	let params_list = ident()
		.then_ignore(select!{ Token::Colon(..) => () })
		.then(var_type())
		.separated_by(select!{ Token::Comma(..) => () })
		.collect::<Vec<_>>()
		.delimited_by(
			select!{ Token::ParenLeft(..) => () },
			select!{ Token::ParenRight(..) => () }
		)
		.labelled("parameters");
	
	// FN_TYPE ::= `function` VAR_TYPE PARAMS_LIST
	let fn_type = select!{ Token::Keyword(Keyword::Function, ..) => () }
		.ignore_then(var_type())
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
///          |   ident `:` VAR_TYPE `;`
fn decl_var<'src>()
-> impl Parser<'src, &'src [Token<'src>], Decl<'src>, Err<Rich<'src, Token<'src>>>>
+ Clone {
	let val = expr().labelled("value");

	ident()
		.then_ignore(select!{ Token::Colon(..) => () })
		.then(var_type())
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

/// VAR_TYPE ::= ATOMIC
///          |  `array` `[` EXPR? `]` ATOMIC
/// 
/// ATOMIC   ::= `boolean`
///          |   `character`
///          |   `integer`
///          |   `string`
///          |   `void`
fn var_type<'src>()
-> impl Parser<'src, &'src [Token<'src>], Type<'src>, Err<Rich<'src, Token<'src>>>> {

}

/// EXPR ::= EXPR [`+`|`-`|`*`|`/`|`^`|`%`|`=`] EXPR
///      |   EXPR [`==`|`!=`|`<`|`<=`|`>`|`>=`|`&&`|`||`] EXPR
///      |   [`!`|`-`] EXPR
///      |   EXPR [`++`|`--`]
///      |   EXPR `(` (EXPR (`,` EXPR)*)? `)`
///      |   `{` EXPR (`,` EXPR)* `}`
///      |   EXPR `[` EXPR `]`
///      |   ident | `true` | `false` | char_lit | int_lit | str_lit
fn expr<'src>()
-> impl Parser<'src, &'src [Token<'src>], Expr<'src>, Err<Rich<'src, Token<'src>>>>
+ Clone {
	
}

/// STMT ::= DECL `;`
///      |   EXPR `;`
///      |  `if` `(` EXPR `)` STMT
///      |  `if` `(` EXPR `)` STMT `else` `(` EXPR `)` STMT
///      |  `for` `(` EXPR `;` EXPR `;` EXPR `)` STMT
///      |  `while` `(` EXPR `)` STMT
///      |  `print` EXPR (`,` EXPR)* `;`
///      |  `return` EXPR `;`
///      |  `{` STMT* `}`
fn stmt<'src>()
-> impl Parser<'src, &'src [Token<'src>], Stmt<'src>, Err<Rich<'src, Token<'src>>>> {
	let r#if = select!{ Token::Keyword(Keyword::If, s) => s }
		.then(expr())
		.delimited_by(
			select!{ Token::ParenLeft(..) => () },
			select!{ Token::ParenRight(..) => () },
		)
		.then(stmt())
		.then(
			select!{ Token::Keyword(Keyword::Else, ..) => () }
			.ignore_then(stmt())
			.or_not()
		)
		.map(|(((if_span, if_cond), if_body), r#else)| {
			let span = Some(SimpleSpan::new(
				if_span.unwrap().start,
				if_cond.get_span().unwrap().end,
			));

			match r#else {
				Some(r#else) => {
					Stmt::IfElse(stmt::IfElse{
						condition: Box::new(if_cond),
						body: Box::new(if_body),
						else_body: Box::new(r#else),
						span,
					})
				}
				None => {
					Stmt::If(
						Box::new(if_cond),
						Box::new(if_body),
						span,
					)
				}
			}
		});
	
	let r#for = select!{ Token::Keyword(Keyword::For, s) => s }
		.then(
			expr()
			.then_ignore(select!{ Token::Semicolon(..) => () })
			.then(expr())
			.then_ignore(select!{ Token::Semicolon(..) => () })
			.then(expr())
			.delimited_by(
				select!{ Token::ParenLeft(..) => () },
				select!{ Token::ParenRight(..) => () }
			)
		)
		.then(stmt())
		.map(|((for_span, ((init_expr, condition), next_expr)), body)| {
			let span = Some(SimpleSpan::new(
				for_span.unwrap().start,
				next_expr.get_span().unwrap().end,
			));

			Stmt::For(stmt::For {
				init_expr: Box::new(init_expr),
				condition: Box::new(condition),
				next_expr: Box::new(next_expr),
				body: Box::new(body),
				span,
			})
		});

	let r#while = select!{ Token::Keyword(Keyword::While, s) => s }
		.then(
			expr()
			.delimited_by(
				select!{ Token::ParenLeft(..) => () },
				select!{ Token::ParenRight(..) => () }
			)
		)
		.then(stmt())
		.map(|((while_span, condition), body)| {
			let span = Some(SimpleSpan::new(
				while_span.unwrap().start,
				condition.get_span().unwrap().end,
			));
			
			Stmt::While(
				Box::new(condition),
				Box::new(body),
				span,
			)
		});

	let print = select!{ Token::Keyword(Keyword::Print, s) => s }
		.then(
			expr()
				.separated_by(select!{ Token::Comma(..) => () })
				.at_least(1)
				.collect::<Vec<_>>()
		)
		.then_ignore(select!{ Token::Semicolon(..) => () })
		.map(|(print_span, exprs)| {
			let span = Some(SimpleSpan::new(
				print_span.unwrap().start,
				exprs.last().unwrap().get_span().unwrap().end,
			));

			Stmt::Print(
				exprs,
				span,
			)
		});
	
	let r#return = select!{ Token::Keyword(Keyword::Return, s) => s }
		.then(expr())
		.then_ignore(select!{ Token::Semicolon(..) => () })
		.map(|(return_span, expr)| {
			let span = Some(SimpleSpan::new(
				return_span.unwrap().start,
				expr.get_span().unwrap().end,
			));
			
			Stmt::Return(
				Box::new(expr),
				span,
			)
		});
	
	let block = select!{ Token::CurlyLeft(s) => s }
		.then(
			stmt()
			.repeated()
			.collect::<Vec<_>>()
		)
		.then(select!{ Token::CurlyRight(s) => s })
		.map(|((s1, stmts), s2)| Stmt::Block(
			stmts,
			Some(SimpleSpan::new(
				s1.unwrap().start,
				s2.unwrap().end,
			))
		))
		.labelled("block");
	
	let stmt = recursive(|stmt| 
		choice((
			decl_var()
				.then_ignore(select!{ Token::Semicolon(..) => () })
				.map(|d| {
					let span = d.get_span();
					Stmt::Decl(Box::new(d), span)
				}),
			expr()
				.then_ignore(select!{ Token::Semicolon(..) => () })
				.map(|e| {
					let span = e.get_span();
					Stmt::Expr(Box::new(e), span)
				}),
			r#for,
		))
	);

	stmt
}
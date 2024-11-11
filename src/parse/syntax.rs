use chumsky::input::ValueInput;
use expr::BinaryExprKind;
use r#type::FunctionType;
use super::*;

/// PROGRAM ::= DECL*
pub fn parser<'src, I>()
-> impl Parser<'src, I, Vec<Decl<'src>>, Err<Rich<'src, Token<'src>>>>
    + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>
{
    choice((
        decl_fn(),
        decl_var(),
    ))
    .labelled("declaration")
    .repeated()
    .collect::<Vec<_>>()
}

/// DECL_FN ::= ident `:` FN_TYPE `=` STMT
///         |   ident `:` FN_TYPE `;`
/// FN_TYPE ::= `function` VAL_TYPE PARAMS
/// PARAMS  ::= `(` ( PARAM ( `,` PARAM )* )? `)`
/// PARAM   ::= ident `:` VAL_TYPE
fn decl_fn<'src, I>()
-> impl Parser<'src, I, Decl<'src>, Err<Rich<'src, Token<'src>>>>
    + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>
{
    let params =
        select!{ Token::Ident(i) => i }
        .then_ignore(just(Token::Colon))
        .then(val_type())
        .map_with(|(i, t), extra| r#type::Param {
            ident: i,
            r#type: t,
            span: extra.span(),
            symbol: None,
        })
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::ParenLeft), just(Token::ParenRight))
        .labelled("parameters");

    let fn_type =
        just(Token::Keyword(Keyword::Function))
        .ignore_then(val_type())
        .then(params)
        .map_with(|(t, params), extra|
            Type::Function(FunctionType {
                return_type: Box::new(t),
                params,
                span: extra.span(),
            })
        );

    select!{ Token::Ident(i) => i }
        .then_ignore(just(Token::Colon))
        .then(fn_type)
        .then(
            choice((
                just(Token::Operator(Op::Assign))
                    .ignore_then(stmt())
                    .map(|s| Some(s)),
                just(Token::Semicolon).ignored().map(|_| None),
            ))
        )
        .map_with(|((i, r#type), body), extra|
            Decl::Function(decl::Function {
                name: i,
                r#type: Box::new(r#type),
                body,
                span: extra.span(),
                symbol: None,
            })
        )
}

/// DECL_VAR ::= ident `:` VAL_TYPE `=` EXPR `;`
///          |   ident `:` VAL_TYPE `;`
fn decl_var<'src, I>()
-> impl Parser<'src, I, Decl<'src>, Err<Rich<'src, Token<'src>>>>
    + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>
{
    select!{ Token::Ident(i) => i }
        .then_ignore(just(Token::Colon))
        .then(val_type())
        .then(
            choice((
                just(Token::Operator(Op::Assign))
                    .ignore_then(expr())
                    .then_ignore(just(Token::Semicolon))
                    .map(|e| Some(e)),
                just(Token::Semicolon).ignored().map(|_| None),
            ))
        )
        .map_with(|((i, r#type), rhs), extra|
            Decl::Var(decl::Var {
                name: i,
                r#type: Box::new(r#type),
                rhs,
                span: extra.span(),
                symbol: None,
            })
        )
}

/// EXPR    ::= ASSIGN
///         |   LOGIC
///         |   ARRAY
///
/// ASSIGN  ::= [IDENT|INDEX] `=` EXPR
///
/// ARRAY   ::= `{` EXPR (`,` EXPR)* `}`
///
/// LOGIC   ::= CMP [`&&`|`||`] CMP
///         |   CMP
///
/// CMP     ::= SUM [`==`|`!=`|`<`|`<=`|`>`|`>=`] SUM
///         |   SUM
///
/// SUM     ::= PRODUCT [`+`|`-`] PRODUCT
///         |   PRODUCT
///
/// PRODUCT ::= INDEX [`*`|`/`|`^`|`%`] INDEX
///         |   INDEX
///
/// FACTOR  ::= `-` FACTOR
///         |   `!` FACTOR
///         |   `(` EXPR `)`
///         |   CALL
///         |   IDENT
///         |   LIT
///
/// CALL    ::= IDENT `(` (EXPR (`,` EXPR)*)? `)`
///
/// INDEX   ::= FACTOR `[` EXPR `]`
///         |   FACTOR
fn expr<'src, I>()
-> impl Parser<'src, I, Expr<'src>, Err<Rich<'src, Token<'src>>>> 
    + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>
{
    recursive(|expr| {
        // FACTOR ::= ident
        let ident =
            select!{ Token::Ident(i) => i }
            .map_with(|i, extra| Expr::Ident(expr::IdentExpr{
                ident: i,
                span: extra.span(),
                symbol: None,
            }))
            .labelled("identifier");

        // LIT ::= lit_bool
        let lit_bool =
            select! {
                Token::Keyword(Keyword::True) => true,
                Token::Keyword(Keyword::False) => false,
            }
            .map_with(|b, extra| Expr::BoolLit(b, extra.span()));

        // LIT ::= lit_char
        let lit_char =
            select!{ Token::LitChar(c) => c }
            .map_with(|c, extra| Expr::CharLit(c, extra.span()));

        // LIT ::= lit_int
        let lit_int =
            select!{ Token::LitInt(i) => i }
            .map_with(|i, extra| Expr::IntLit(i, extra.span()));

        // LIT ::= lit_str
        let lit_str =
            select!{ Token::LitString(s) => s }
            .map_with(|s, extra| Expr::StrLit(s, extra.span()));

        // FACTOR ::= LIT
        let lit = choice((
            lit_bool,
            lit_char,
            lit_int,
            lit_str,
        ))
            .labelled("literal");

        // FACTOR ::= ident `(` ( EXPR ( `,` EXPR )* )? `)`
        let call =
            select!{ Token::Ident(i) => i }
            .then(
                expr.clone()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(
                        just(Token::ParenLeft),
                        just(Token::ParenRight),
                )
            )
            .map_with(|(ident, args), extra| Expr::Call(expr::CallExpr {
                ident,
                args,
                span: extra.span(),
                symbol: None,
            }))
            .labelled("call");

        let factor = recursive(|factor|
            choice((
                just(Token::Operator(Op::Sub))
                    .ignore_then(factor.clone())
                    .map_with(|f, extra| Box::new(Expr::Neg(f, extra.span()))),
                just(Token::Operator(Op::Not))
                    .ignore_then(factor.clone())
                    .map_with(|f, extra| Box::new(Expr::Not(f, extra.span()))),
                expr.clone()
                    .delimited_by(
                        just(Token::ParenLeft),
                        just(Token::ParenRight),
                    )
                    .map(|e| Box::new(e)),
                call.map(|e| Box::new(e)),
                lit.map(|e| Box::new(e)),
                ident.then_ignore(just(Token::Operator(Op::Inc)))
                    .map_with(|ident, extra|
                        Box::new(Expr::Inc(Box::new(ident), extra.span()))
                    ),
                ident.then_ignore(just(Token::Operator(Op::Dec)))
                    .map_with(|ident, extra|
                        Box::new(Expr::Dec(Box::new(ident), extra.span()))
                    ),
                ident.map(|e| Box::new(e)),
            ))
        );
        
        // INDEX ::= FACTOR `[` EXPR `]`
        //       |   FACTOR
        let index = choice((
            factor.clone()
                .then(
                    expr.clone()
                    .delimited_by(
                        just(Token::BraceLeft),
                        just(Token::BraceRight),
                    )
                )
                .map_with(|(array, index), extra| Box::new(
                    Expr::Index(expr::IndexExpr {
                        array,
                        index: Box::new(index),
                        span: extra.span(),
                    })
                )),
            factor.clone(),
        ));

        // PRODUCT ::= INDEX [`*`|`/`|`^`|`%`] INDEX
        //         |   INDEX
        let product =
            index.clone().foldl_with(
                choice((
                    just(Token::Operator(Op::Mul)).to(BinaryExprKind::Mul),
                    just(Token::Operator(Op::Div)).to(BinaryExprKind::Div),
                    just(Token::Operator(Op::Exp)).to(BinaryExprKind::Exp),
                    just(Token::Operator(Op::Mod)).to(BinaryExprKind::Mod),
                ))
                .then(index.clone())
                .repeated(),
                |left, (kind, right), extra| Box::new(
                    Expr::Binary(expr::BinaryExpr {
                        kind,
                        left,
                        right,
                        span: extra.span(),
                    })
                )
            );

        // SUM ::= PRODUCT [`+`|`-`] PRODUCT
        //     |   PRODUCT
        let sum =
            product.clone().foldl_with(
                choice((
                    just(Token::Operator(Op::Add)).to(BinaryExprKind::Add),
                    just(Token::Operator(Op::Sub)).to(BinaryExprKind::Sub),
                ))
                .then(product)
                .repeated(),
                |left, (kind, right), extra| Box::new(
                    Expr::Binary(expr::BinaryExpr {
                        kind,
                        left,
                        right,
                        span: extra.span(),
                    })
                )
            );
        
        // CMP ::= SUM [`==`|`!=`|`<`|`<=`|`>`|`>=`] SUM
        //     |   SUM
        let cmp =
            sum.clone().foldl_with(
                choice((
                    just(Token::Operator(Op::Eq)).to(BinaryExprKind::Eq),
                    just(Token::Operator(Op::NotEq)).to(BinaryExprKind::NotEq),
                    just(Token::Operator(Op::Less)).to(BinaryExprKind::Less),
                    just(Token::Operator(Op::LessEq)).to(BinaryExprKind::LessEq),
                    just(Token::Operator(Op::Greater)).to(BinaryExprKind::Greater),
                    just(Token::Operator(Op::GreaterEq)).to(BinaryExprKind::GreaterEq),
                ))
                .then(sum)
                .repeated(),
                |left, (kind, right), extra| Box::new(
                    Expr::Binary(expr::BinaryExpr {
                        kind,
                        left,
                        right,
                        span: extra.span(),
                    })
                )
            );

        // LOGIC ::= CMP [`&&`|`||`] CMP
        //       |   CMP
        let logic =
            cmp.clone().foldl_with(
                choice((
                    just(Token::Operator(Op::And)).to(BinaryExprKind::And),
                    just(Token::Operator(Op::Or)).to(BinaryExprKind::Or),
                ))
                .then(cmp)
                .repeated(),
                |left, (kind, right), extra| Box::new(
                    Expr::Binary(expr::BinaryExpr {
                        kind,
                        left,
                        right,
                        span: extra.span(),
                    })
                )
            );

        // ASSIGN ::= [ident|index] `=` EXPR
        let assign =
            choice((
                ident.map(|i| Box::new(i)),
                index,
            ))
            .then_ignore(just(Token::Operator(Op::Assign)))
            .then(expr.clone())
            .map_with(|(left, right), extra|
                Expr::Binary(expr::BinaryExpr {
                    kind: BinaryExprKind::Assign,
                    left,
                    right: Box::new(right),
                    span: extra.span(),
                })
            );

        let array =
            expr.clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .validate(|v, extra, emitter| {
                if v.len() == 0 {
                    emitter.emit(Rich::custom(
                        extra.span(),
                        "arrays cannot be empty"
                    ))
                }
                v
            })
            .delimited_by(
                just(Token::CurlyLeft),
                just(Token::CurlyRight),
            )
            .map_with(|a, extra| Expr::Array(a, extra.span()));

        choice((assign, logic.map(|e| *e), array))
    })
    .labelled("expression")
}

/// STMT ::= `{` STMT* `}`
///      |   DECL
///      |   EXPR `;`
///      |   `print` EXPR ( `,` EXPR )* `;`
///      |   `return` EXPR `;`
///      |   `if` `(` EXPR `)` STMT
///      |   `if` `(` EXPR `)` STMT `else` STMT
///      |   `for` `(` EXPR `;` EXPR `;` EXPR `)` STMT
///      |   `while` `(` EXPR `)` STMT
fn stmt<'src, I>()
-> impl Parser<'src, I, Stmt<'src>, Err<Rich<'src, Token<'src>>>>
    + Clone
where 
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>
{
    recursive(|stmt| { 
        // STMT ::= `{` STMT* `}`
        let block =
            stmt.clone()
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::CurlyLeft), just(Token::CurlyRight))
            .map_with(|body, extra| Stmt::Block(body, extra.span()))
            .labelled("block");
        
        // STMT ::= DECL `;`
        let decl =
            decl_var()
            .map_with(|decl, extra| Stmt::Decl(Box::new(decl), extra.span()))
            .labelled("variable declaration");

        // STMT ::= EXPR `;`
        let expr_stmt =
            expr()
            .then_ignore(just(Token::Semicolon))
            .map_with(|expr, extra| Stmt::Expr(expr, extra.span()))
            .labelled("expression");

        // STMT ::= `print` EXPR ( `,` EXPR )* `;`
        let print =
            just(Token::Keyword(Keyword::Print))
            .ignore_then(
                expr()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
            )
            .then_ignore(just(Token::Semicolon))
            .map_with(|exprs, extra| Stmt::Print(exprs, extra.span()))
            .labelled("print");

        // STMT ::= `return` EXPR `;`
        let r#return =
            just(Token::Keyword(Keyword::Return))
            .ignore_then(expr())
            .then_ignore(just(Token::Semicolon))
            .map_with(|expr, extra| Stmt::Return(expr, extra.span()))
            .labelled("return");

        // STMT ::= `if` `(` EXPR `)` STMT
        //      |   `if` `(` EXPR `)` STMT `else` STMT
        let r#if =
            just(Token::Keyword(Keyword::If))
            .ignore_then(
                expr()
                .delimited_by(just(Token::ParenLeft), just(Token::ParenRight))
            )
            .then(stmt.clone())
            .then(
                just(Token::Keyword(Keyword::Else))
                .ignore_then(stmt.clone())
                .map(|s| Box::new(s))
                .or_not()
            )
            .map_with(|((condition, body), else_body), extra|
                Stmt::If(stmt::IfStmt {
                    condition: Box::new(condition),
                    body: Box::new(body),
                    else_body,
                    span: extra.span(),
                })
            )
            .labelled("if-statement");

        // STMT ::= `for` `(` EXPR `;` EXPR `;` EXPR `)` STMT
        let r#for =
            just(Token::Keyword(Keyword::For))
            .ignore_then(
                expr()
                .then_ignore(just(Token::Semicolon))
                .then(expr())
                .then_ignore(just(Token::Semicolon))
                .then(expr())
                .delimited_by(just(Token::ParenLeft), just(Token::ParenRight))
            )
            .then(stmt.clone())
            .map_with(|(((init_expr, condition), next_expr), body), extra|
                Stmt::For(stmt::ForStmt {
                    init_expr: Box::new(init_expr),
                    condition: Box::new(condition),
                    next_expr: Box::new(next_expr),
                    body: Box::new(body),
                    span: extra.span(),
                })
            )
            .labelled("for-loop");

        // STMT ::= `while` `(` EXPR `)` STMT
        let r#while =
            just(Token::Keyword(Keyword::While))
            .ignore_then(
                expr()
                .delimited_by(just(Token::ParenLeft), just(Token::ParenRight))
            )
            .then(stmt.clone())
            .map_with(|(condition, body), extra|
                Stmt::While(stmt::WhileStmt {
                    condition: Box::new(condition),
                    body: Box::new(body),
                    span: extra.span(),
                })
            )
            .labelled("while-loop");
    
        choice((
            r#return,
            print,
            r#if,
            r#for,
            r#while,
            decl,
            expr_stmt,
            block,
        ))
    })
}

/// VAL_TYPE ::= `array` `[` EXPR? `]` VAL_TYPE
///          |   `boolean`
///          |   `char`
///          |   `integer`
///          |   `string`
///          |   `void`
fn val_type<'src, I>()
-> impl Parser<'src, I, Type<'src>, Err<Rich<'src, Token<'src>>>>
    + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>
{
    recursive(|val_type| {
        let array =
            just(Token::Keyword(Keyword::Array))
            .ignore_then(
                expr().or_not()
                .delimited_by(
                    just(Token::BraceLeft),
                    just(Token::BraceRight),
                )
            )
            .then(val_type.clone())
            .map_with(|(expr, r#type), extra|
                Type::Array(r#type::ArrayType {
                    r#type: Box::new(r#type),
                    size: r#type::ArraySize::Expr(expr),
                    span: extra.span(),
                }) 
            );

        let atomic =
            choice((
                just(Token::Keyword(Keyword::Boolean)).to(Atomic::Boolean),
                just(Token::Keyword(Keyword::Char)).to(Atomic::Char),
                just(Token::Keyword(Keyword::Integer)).to(Atomic::Integer),
                just(Token::Keyword(Keyword::String)).to(Atomic::String),
                just(Token::Keyword(Keyword::Void)).to(Atomic::Void),
            ))
            .map_with(|t, extra| Type::Atomic(t, extra.span()));

        choice((
            array,
            atomic,
        ))
    })
}

# Grammar

A listing of the rules making up this compiler's implementation of the B-Minor grammar.

- [PROGRAM](#program)
- [DECL](#decl)
   - [DECL_FN](#decl_fn)
      - [FN_TYPE](#fn_type)
      - [PARAM](#param)
      - [PARAMS](#params)
   - [DECL_VAR](#decl_var)
- [EXPR](#expr)
   - [ARRAY](#array)
   - [ASSIGN](#assign)
   - [CALL](#call)
   - [CMP](#cmp)
   - [FACTOR](#factor)
   - [INDEX](#index)
   - [LOGIC](#logic)
   - [MAYBE_INDEX](#maybe_index)
   - [PRODUCT](#product)
   - [SUM](#sum)
- [STMT](#stmt)
- [IDENT](#ident)
- [VAL_TYPE](#val_type)

## PROGRAM

> PROGRAM ::= DECL*

## DECL

> DECL ::= DECL_FN | DECL_VAR

### DECL_FN

> DECL_FN ::= IDENT `:` FN_TYPE `=` STMT\
> DECL_FN ::= IDENT `:` FN_TYPE `;`

A function declaration can either contain its definition, or it can be a forward declaration.
As in C, a forward declaration/prototype is required in order to call a function whose body is
defined later.

#### FN_TYPE

> FN_TYPE ::= `function` \[`void` | VAL_TYPE] PARAMS

B-Minor functions can not return other functions, so the return type is either `void` or a VAL_TYPE.

#### PARAM

> PARAM ::= IDENT `:` VAL_TYPE

#### PARAMS

> PARAMS ::= `(` ( PARAM ( `,` PARAM )* )? `)`

### DECL_VAR

> DECL_VAR ::= IDENT `:` VAL_TYPE `=` EXPR `;`\
> DECL_VAR ::= IDENT `:` VAL_TYPE `;`

Like functions, variable declarations can either contain an initializer or not (though unlike functions,
an uninitialized variable will be initialized to a default "zero-value").

## EXPR

> EXPR ::= ASSIGN\
> EXPR ::= LOGIC\
> EXPR ::= ARRAY

### ASSIGN

> ASSIGN ::= \[IDENT | INDEX] `=` EXPR

Assignments must pertain to assignable entities - variables and array indices. It's worth noting an array itself
is not assignable after initialization, as in C.

### LOGIC

> LOGIC ::= CMP \[`&&`|`||`] CMP\
> LOGIC ::= CMP

Logic AND and OR operators.

### CMP

> CMP ::= SUM \[`==`|`!=`|`<`|`<=`|`>`|`>=`] SUM\
> CMP ::= SUM

Boolean comparison expressions between values of summation expressions.

### SUM

> SUM ::= PRODUCT \[`+`|`-`] PRODUCT\
> SUM ::= PRODUCT

Addition or subtraction expressions. This is positioned here to enforce operator precedence,
ensuring product expressions have higher precedence.

### PRODUCT

> PRODUCT ::= MAYBE_INDEX \[`*`|`/`|`^`|`%`] MAYBE_INDEX\
> PRODUCT ::= MAYBE_INDEX

Multiplication, division, exponentiation, or modulo.

### MAYBE_INDEX

> MAYBE_INDEX ::= INDEX | FACTOR

### INDEX

> INDEX ::= FACTOR `[` EXPR `]`\

### FACTOR

> FACTOR ::= `-` FACTOR\
> FACTOR ::= `!` FACTOR\
> FACTOR ::= `(` EXPR `)`\
> FACTOR ::= CALL | IDENT | BOOL_LIT | CHAR_LIT | INT_LIT | STR_LIT

### CALL

> CALL ::= IDENT `(` ( EXPR ( `,` EXPR )* )? `)`

### ARRAY

> ARRAY ::= `{` EXPR ( `,` EXPR )* `}`

An array literal. These are used mainly to initialize arrays when allocated.

## IDENT

> IDENT ::= \[a-zA-Z]\[a-zA-Z0-9_]*

## STMT

> STMT ::= `{` STMT* `}`\
> STMT ::= DECL\
> STMT ::= EXPR `;`\
> STMT ::= `print` EXPR ( `,` EXPR )* `;`\
> STMT ::= `return` EXPR? `;`\
> STMT ::= `if` `(` EXPR `)` STMT\
> STMT ::= `if` `(` EXPR `)` STMT `else` STMT\
> STMT ::= `for` `(` EXPR `;` EXPR `;` EXPR `)` STMT\
> STMT ::= `while` `(` EXPR `)` STMT

Statements make up the body of functions; most people will write a function body as a block (`{}`) at
the top level.

## VAL_TYPE

> VAL_TYPE ::= `array` `[` (INT_LIT)? `]` VAL_TYPE\
> VAL_TYPE ::= `boolean` | `char` | `integer` | `string`

VAL_TYPE (value type) refers to any type a variable could have, e.g. excluding functions or `void`.
An array type can omit the int literal size indicator if it's given an initializer or if it's a function
parameter, as arrays are passed into functions as pointers. Hopefully, in the future, array size indicators
will be expanded to allow identifiers of constant values.

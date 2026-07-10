#set document(
    title: [`S--` specification],
)
#set heading(numbering: "1.1")
#show math.equation: set block(breakable: true)

#import "@preview/curryst:0.6.0": rule, prooftree, rule-set

#title()

= Syntax
#let syntax_table(body) = {
    table(
        columns: (2fr, 7fr, 4fr),
        align: left,
        stroke: none,
        ..body,
    )
}

#let syntax_row(a, b, c) = (
    [#a],
    [#b],
    [#c]
)

#syntax_table((
    ..syntax_row($italic("Expression e")$, $-> n$, "integer"),
    ..syntax_row("", $ | mono("true") | mono("false")$, "boolean"),
    // ..syntax_row("", $ | s$, "string"),
    ..syntax_row("", $ | x$, "identifier"),
    ..syntax_row("", $ | e mono("+") e | e mono("-") e | e mono("*") e | e mono("/") e | e mono("%") e$, "arithmetic expression"),
    ..syntax_row("", $ | e mono("=") e | e mono("<") e /*| e mono("and") e | e mono("or") e */| mono("not") e$, "boolean expression"),
    // ..syntax_row("", $ | e ; e$, "sequence"),
    // ..syntax_row("", $ | e mono(",") e$, "pair"),
    // ..syntax_row("", $ | e mono(".1") | e mono(".2")$, "pair deconstruction"),
    ..syntax_row("", $ | mono("if") e mono("then") e mono("else") e$, "branch"),
    // ..syntax_row("", $ | mono("fn (") x^*, mono(") => ") e$, "function"),
    ..syntax_row("", $ | mono("let") x mono(":=") e mono("in") e$, "let binding"),
    ..syntax_row("", $ | mono("let fn") f mono("(") x^*, mono(") =>") e mono("in") e$, "function binding"),
    ..syntax_row("", $ | f mono("(") x^*, mono(")")$, "call (by reference)"),
))

== Program
A program is an expression.

== Identifiers
Alpha-numeric identifiers are `[a-zA-Z][a-zA-Z0-9_]*`.
Identifiers are case sensitive: `z` and `Z` are different.
The reserved words cannot be used as identifiers: `true`, `false`, /*`and`, `or`,*/ `not`, `if`, `then`, `else`, `let`, `in`, `fn`.

== Numbers
Numbers are integers, optionally prefixed with `-`(for negative integers): `-?[0-9]+`.

== Comments
A comment is any character sequence within the comment block `(* *)`. The comment block can be nested.

== Precedence / Associativity
In parsing `S--` program text, the precedence of the `S--` constructs in decreasing order is as follows.
Symbols in the same set have identical precedence.
Symbols with subscript $L$ (respectively $R$) are left- (respectively right-) associative.
Symbols without subscript are non-associative.
Precedence can be overridden by parentheses.

$
//   {text("function calling")}_L,
//   \ {mono(".1"), mono(".2")}_L,
  \ {mono("not")}_R,
  \ {mono("*"), mono("/"), mono("%")}_L,
  \ {mono("+"), mono("-")}_L,
  \ {mono("="), mono("<")}_L,
//   \ {mono("and")}_L,
//   \ {mono("or")}_L,
  \ {mono("else")},
  \ {mono("then")},
//   \ {mono(";")}_L,
  \ {mono("in")}
//   \ {mono("in")},
//   \ {mono("=>")},
//   \ {mono(",")text("(for pairs)")}_R
$

#pagebreak()

= Semantics


#let sem_item_table(body) = {
    table(
        columns: (4fr, 1fr, 4fr, 1fr, 7fr),
        align: (right, center, right, center, left),
        stroke: none,
        ..body,
    )
}

#let sem_item_row(a, b, c, iseq) = (
    [#a],
    [$in$],
    [#b],
    if iseq {
       [$=$] 
    } else {
        []
    },
    [#c]
) 

#sem_item_table((
    ..sem_item_row($n$, $ZZ$, "integers", false),
    ..sem_item_row($b$, $BB$, "booleans", false),
    ..sem_item_row($x$, $italic("Id")$, "identifiers", false),
    ..sem_item_row($l$, $italic("Addr")$, "addresses", false),
    ..sem_item_row($v$, $italic("Val")$, $ZZ + BB /*+ italic("Pair") + italic("Function")*/$, true),
    // ..sem_item_row($chevron.l v_1, v_2 chevron.r$, $italic("Pair")$, $italic("Val") times italic("Val")$, true),
    ..sem_item_row($sigma$, $italic("Env")$, $italic("Id") attach(->, t: "fin") italic("Addr") + italic("Function")$, true),
    ..sem_item_row($M$, $italic("Mem")$, $italic("Addr") attach(->, t: "fin") italic("Val")$, true),
    ..sem_item_row($f, chevron.l X, e, sigma chevron.r$, $italic("Function")$, $(italic("Id list")) times italic("Expression") times italic("Env")$, true)
))

#align(center, rule-set(
    prooftree(rule(
        name: [Num],
        $sigma, M tack.r mono("n") arrow.b.double n, M$,
    )),
    // prooftree(rule(
    //     name: [Fn],
    //     $sigma, M tack.r mono("fn (") x^*, mono(") =>") e arrow.b.double chevron.l [x^*,], e, sigma chevron.r, M$,
    // )),
    prooftree(rule(
        name: [True],
        $sigma, M tack.r mono("true") arrow.b.double t r u e, M$,
    )),
    prooftree(rule(
        name: [False],
        $sigma, M tack.r mono("false") arrow.b.double f a l s e, M$,
    )),
    prooftree(rule(
        name: [Var],
        $sigma, M tack.r x arrow.b.double M(sigma(x)), M$,
    )),
    prooftree(rule(
        name: [Add],
        $sigma, M tack.r e_1 arrow.b.double n_1, M_1$,
        $sigma, M_1 tack.r e_2 arrow.b.double n_2, M_2$,
        $sigma, M tack.r e_1 mono("+") e_2 arrow.b.double n_1 + n_2, M_2$,
    )),
    prooftree(rule(
        name: [Sub],
        $sigma, M tack.r e_1 arrow.b.double n_1, M_1$,
        $sigma, M_1 tack.r e_2 arrow.b.double n_2, M_2$,
        $sigma, M tack.r e_1 mono("-") e_2 arrow.b.double n_1 - n_2, M_2$,
    )),
    prooftree(rule(
        name: [Mul],
        $sigma, M tack.r e_1 arrow.b.double n_1, M_1$,
        $sigma, M_1 tack.r e_2 arrow.b.double n_2, M_2$,
        $sigma, M tack.r e_1 mono("*") e_2 arrow.b.double n_1 times n_2, M_2$,
    )),
    prooftree(rule(
        name: [Div],
        $sigma, M tack.r e_1 arrow.b.double n_1, M_1$,
        $sigma, M_1 tack.r e_2 arrow.b.double n_2, M_2$,
        $sigma, M tack.r e_1 mono("/") e_2 arrow.b.double n_1 \/ n_2, M_2$,
    )),
    prooftree(rule(
        name: [Mod],
        $sigma, M tack.r e_1 arrow.b.double n_1, M_1$,
        $sigma, M_1 tack.r e_2 arrow.b.double n_2, M_2$,
        $sigma, M tack.r e_1 mono("%") e_2 arrow.b.double n_1 % n_2, M_2$,
    )),
    prooftree(rule(
        name: [Eq],
        $sigma, M tack.r e_1 arrow.b.double v_1, M_1$,
        $sigma, M_1 tack.r e_2 arrow.b.double v_2, M_2$,
        $sigma, M tack.r e_1 mono("=") e_2 arrow.b.double v_1 = v_2, M_2$,
    )),
    prooftree(rule(
        name: [Less],
        $sigma, M tack.r e_1 arrow.b.double n_1, M_1$,
        $sigma, M_1 tack.r e_2 arrow.b.double n_2, M_2$,
        $sigma, M tack.r e_1 mono("<") e_2 arrow.b.double n_1 < n_2, M_2$,
    )),
    // prooftree(rule(
    //     name: [AndL],
    //     $sigma, M tack.r e_1 arrow.b.double t r u e, M_1$,
    //     $sigma, M_1 tack.r e_2 arrow.b.double b_2, M_2$,
    //     $sigma, M tack.r e_1 mono("and") e_2 arrow.b.double b_2, M_2$,
    // )),
    // prooftree(rule(
    //     name: [AndS],
    //     $sigma, M tack.r e_1 arrow.b.double f a l s e, M_1$,
    //     $sigma, M tack.r e_1 mono("and") e_2 arrow.b.double f a l s e, M_1$,
    // )),
    // prooftree(rule(
    //     name: [OrL],
    //     $sigma, M tack.r e_1 arrow.b.double f a l s e, M_1$,
    //     $sigma, M_1 tack.r e_2 arrow.b.double b_2, M_2$,
    //     $sigma, M tack.r e_1 mono("or") e_2 arrow.b.double b_2, M_2$,
    // )),
    // prooftree(rule(
    //     name: [OrS],
    //     $sigma, M tack.r e_1 arrow.b.double t r u e, M_1$,
    //     $sigma, M tack.r e_1 mono("or") e_2 arrow.b.double t r u e, M_1$,
    // )),
    prooftree(rule(
        name: [Not],
        $sigma, M tack.r e arrow.b.double b, M_1$,
        $sigma, M tack.r mono("not") e arrow.b.double not b, M_1$,
    )),
    // prooftree(rule(
    //     name: [Seq],
    //     $sigma, M tack.r e_1 arrow.b.double v_1, M_1$,
    //     $sigma, M_1 tack.r e_2 arrow.b.double v_2, M_2$,
    //     $sigma, M tack.r e_1 ; e_2 arrow.b.double v_2, M_2$,
    // )),
    // prooftree(rule(
    //     name: [Pair],
    //     $sigma, M tack.r e_1 arrow.b.double v_1, M_1$,
    //     $sigma, M_1 tack.r e_2 arrow.b.double v_2, M_2$,
    //     $sigma, M tack.r e_1 mono(",") e_2 arrow.b.double chevron.l v_1, v_2 chevron.r, M_2$,
    // )),
    // prooftree(rule(
    //     name: [Pair1],
    //     $sigma, M tack.r e arrow.b.double chevron.l v_1, v_2 chevron.r, M_1$,
    //     $sigma, M tack.r e mono(".1") arrow.b.double v_1, M_1$,
    // )),
    // prooftree(rule(
    //     name: [Pair2],
    //     $sigma, M tack.r e arrow.b.double chevron.l v_1, v_2 chevron.r, M_1$,
    //     $sigma, M tack.r e mono(".2") arrow.b.double v_2, M_1$,
    // )),
    prooftree(rule(
        name: [IfT],
        $sigma, M tack.r e arrow.b.double t r u e, M_1$,
        $sigma, M_1 tack.r e_1 arrow.b.double v, M_2$,
        $sigma, M tack.r mono("if") e mono("then") e_1 mono("else") e_2 arrow.b.double v, M_2$,
    )),
    prooftree(rule(
        name: [IfF],
        $sigma, M tack.r e arrow.b.double f a l s e, M_1$,
        $sigma, M_1 tack.r e_2 arrow.b.double v, M_2$,
        $sigma, M tack.r mono("if") e mono("then") e_1 mono("else") e_2 arrow.b.double v, M_2$,
    )),
    // prooftree(rule(
    //     name: [Call],
    //     $sigma, M tack.r e arrow.b.double chevron.l [x'_1, x'_2, ..., x'_k], e', sigma' chevron.r, M_1$,
    //     $sigma' {x'_1 |-> sigma(x_1)}{x'_2 |-> sigma(x_2)}...{x'_k |-> sigma(x_k)}, M_1 tack.r e' arrow.b.double v, M_2$,
    //     $sigma, M tack.r e mono("(") x_1 mono(",") x_2 mono(",") ... mono(",") x_k mono(")") arrow.b.double v, M_2$,
    // )),
    prooftree(rule(
        name: [Let],
        $sigma, M tack.r e_1 arrow.b.double v_1, M_1$,
        $l in.not d o m M_1$,
        $sigma{x |-> l}, M_1{l |-> v_1} tack.r e arrow.b.double v, M_2$,
        $sigma, M tack.r mono("let") x mono(":=") e_1 mono("in") e arrow.b.double v, M_2$,
    )),
    prooftree(rule(
        name: [LetFn],
        $sigma{f |-> chevron.l X, e, sigma chevron.r}, M tack.r e_1 arrow.b.double v, M_1$,
        $sigma, M tack.r mono("let fn") f mono("(") X mono(") =>") e mono("in") e_1 arrow.b.double v, M_1$,
    )),
    prooftree(rule(
        name: [Call],
        $sigma(f) = chevron.l [x'_1, x'_2, ..., x'_k], e', sigma' chevron.r$,
        $sigma(x_i) = l_i, M(l_i) = v_i quad (1 <= i <= k)$,
        $sigma'{x'_1 |-> l_1}{x'_2 |-> l_2}...{x'_k |-> l_k}, M tack.r e' arrow.b.double v, M_1$,
        $sigma, M tack.r f mono("(") x_1 mono(",") x_2 mono(",") ... mono(",") x_k mono(")") arrow.b.double v, M_1$,
    ))
))

== Value Equality
- Values of different types are always unequal.
// - Functions are equal if and only if they originate from the same evaluation of a `fn` expression.
// - Pairs are equal if and only if their first and second components are equal respectively.

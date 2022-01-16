foo.
% above: indentation in first line
bar.
baz.

% ---------------------------------------------------------------------------
% Term indentation with argument and block indentation

foobar(
    foobar(
        foobar(
            foobar
        )
    )
).

foobar([[1,0,0],
        [0,1,0],
        [0,0,1]]).

foobar([
    [1,0,0],
    [0,1,0],
    [0,0,1]
]).

fooeee(aa,
       bbb,
       ccc,
       ewe
      ).

foo([
    bar([
        baz([
            foo,
            bar])]),
    foo]).

foo([
    bar([
        baz([
            foo,
            bar
        ])
    ]),
    foo
]).

( foo,
  bar,
  baz ).

[
    foo,
    bar,
    baz
].

f(
    f(
        f(
            foo( foo,
                 bar,
                 ba
               ),
            g
        )
    )
).

% block indentation
f(f(f(
    f,
    h(),
    i,
    j,
    g
))).

% closing parenthesis should not advance
f(f(f(
    f,
    h(),
    i,
    j,
    g
)
)
).

% same with [
f[f[f[
    f,
    g
]]].

% same with [
f[f[f[
    f,
    g
]
]
].

f( f(a,
     b,
     c),
   g(a,
     b,
     c) ).

% TODO: strange block like, closing parenthesis should not advance
foobar(foo(
    a,
    b,
    c), foo(
    a,
    b,
    c)
).

% ---------------------------------------------------------------------------
% Block-like indentation with multiline prefix

foobar(b, c) {
    aaa.
    bbb.
}.

foobar(bar,
       bar) {
    aaa.
    bbb.
}.

foobar(bar,
       bar) {
    aaa.
    foobar(bar,
           bar) {
        aaa.
        bbb.
    }.
    bbb.
}.

% ---------------------------------------------------------------------------
% Simple rules

a :- b.
c :- d.
d :- e.

a :- !,
    b.

a --> b.

a -> b.
c -> d.
d -> e.

a --> !,
    b.

% ---------------------------------------------------------------------------
% Heads and bodies

p :-
    X = (foo +
         4 + 
         8 +
         3),
    Y = 32.

a :-
    foo = ( 3,
            5,
            6
          ),
    foo = (  3,
             5,
             6
          ),
    foo = (   3,
              5,
              6
          ),
    bar = (a
           + b
           + c).

foo :-
    Var = ( a ? 3
          | 4 ).

foo :-
    Var = ( a ?
            3
          | 4 
          ).

foo(R) :-
    ( a ->
        R=3
    ; R=4
    ).

foo :=
    ( a ? 3
    | b ? 4
    | c ? 5
    | d
    ).

foo := ( a ? 3
       | b ? 4
       | c ? 5
       | d
       ).

foobar(bar,
       baz) -->
    bar.

foobar(
    bar,
    baz) -->
    bar. % OK like this

foobar(
    bar,
    baz
) -->
    bar. % OK like this

foo :- bar,
    baz. % OK like this

foo :-
    bar ->
        baz,
        foo
    ;
        no.

foo :-
    ( bar ->
        baz
    ;
        no
    ).

% ';' indent like open paren
foo :-
    ( bar ->
        baz
    ; no,
      bar
    ).

% '|' indent like open paren
foo :=
    ( 3 ?
      aaa
    | bb,
      ccc,
      dd
    ).

foo :-
    bar(a),
    baz(a),
    foo.

foo :-
    foobar,
    foobarfoo,
    ( foo ->
        bar
    ; baz
    ),
    foo(
        bar,
        baz
    ),
    foo([
        bar,
        baz
    ]).

% ---------------------------------------------------------------------------
% Bodies with custom control

foo :-
    +
    -
    1.

foo :-
    X = ( foo
          :3
          :4
        ).

foo :-
    ( a 
    ; b
    ; c
    ).

foo :-
    a 
    ; b
    ; c.

( foo :- a
    ; 33
    ).

( foo -> a
; 33
).

% ---------------------------------------------------------------------------
% Bodies and comments

foobar :-
    bar, % {
    baz, /* {
    bar */
    bar.

% ---------------------------------------------------------------------------
% Directives (special cases for block syntax, commas, etc.)

:- module(foo, [
    foo/3
], [
    assertions
]).

:- foo(a,b,c),
   foo(a,[
       aaa
   ]),
   foo(a,b,c).

% ---------------------------------------------------------------------------
% Assertions

:- pred foo(X) : var(X) => int(X) # "string".

:- pred foo(X)
   : var(X) => int(X) # "string".

:- pred foo(X)
   : var(X)
   => int(X) # "string".

:- pred foo(X)
   : var(X)
   =>
   int(X) # "string".

:- pred foo(X) : var(X) =>
   int(X)
   # "string".

:- pred foo(X) : var(X)
   => int(X)
   # "string".

% ---------------------------------------------------------------------------
% Assertions
% TODO: alternative notation with | (see assrtbar.html)

:- pred foo(X) | var(X) => int(X) # "string".

:- pred foo(X)
   | var(X) => int(X) # "string".

:- pred foo(X)
   | var(X) => int(X)
     # "string".

:- pred foo(X)
   | var(X)
     => int(X) # "string".

:- pred foo(X)
   | var(X)
     =>
     int(X) # "string".

:- pred foo(X)
   | var(X) =>
     int(X) # "string".

% TODO: useful?
:- pred foo(X) | var(X)
                 => int(X)
                 # "string".

% ---------------------------------------------------------------------------
% Nested assertions

:- module {
    :- pred foo(X) : foo
       => bar
       # "string".
}.

% ---------------------------------------------------------------------------
% Rules inside blocks

foo({
    bar :-
        bbb,
        fffooo.
    foobar + 
        barfoo.
    foobar +
        foobar.
}).

foo({
    (bar :-
        bbb,
        fffooo),
    (foobar + 
     bar),
    werwer,
    (foobar +
     foobar)
}).

% ---------------------------------------------------------------------------
% Nested modules with many rules

:- module {
    a.
    a :-
        b,
        ( foo ->
            bar
        ; baz
        ),
        c(ee).
    b.
    b :- b.
}.

:- module {
    :- module {
        a -->
            b.
        b :- !,
            a.
        b :-
            a.
        b :- a.
        b := c :-
            d.
    }.
}.

% ---------------------------------------------------------------------------
% Do not look inside comments for previous indentation

foobar.
/*
    foobar :- foobar,
*/
foo :-
    bar.

% ---------------------------------------------------------------------------
% Detect "closed" neck skipping comments

foo :-
    bar.

% foo({
%      baz
%     }).

/* foo
   bar
 */

foo.

% ---------------------------------------------------------------------------
% Do not indent inside strings or comments

foobarfoo(  "foo(
  ba",
            foo).

foobarfoo(  "fooP
  ba",
            foo).

/*
 weirdly indented code follows

 foo({
  baz
   }).

   :- module(foo, [
     bar,
    bar
   ], [
  foo
 ]).
*/


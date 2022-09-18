# Parser and Abstract Syntax Tree Report

Alice Tedeschi

## Grammar
```
program ::= {var def | func def | class def }∗ stmt∗

class def ::= class ID ( ID ) : NEWLINE INDENT class body DEDENT

class body ::= pass NEWLINE
| {var def | func def }+

func def ::= def ID ( {typed var {, typed var }∗}? ) {-> type}? : NEWLINE INDENT func body DEDENT

func body ::= {global decl | nonlocal decl | var def | func def }∗ stmt+

typed var ::= ID : type

type ::= ID | IDSTRING | [ type ]

global decl ::= global ID NEWLINE

nonlocal decl ::= nonlocal ID NEWLINE

var def ::= typed var = literal NEWLINE

stmt ::= simple stmt NEWLINE
| if expr : block {elif expr : block }∗ {else : block }?
| while expr : bloc}
| for ID in expr : bloc}

simple stmt ::= pass
| return {expr }?
| expr
| { target = }+ expr
block ::= NEWLINE INDENT stmt+ DEDENT

literal ::= None
| True
| False
| INTEGER
| IDSTRING | STRING

# expr ::= e_or0_expr e_if0_expr

# e_if0_expr ::= e_if_expr | eps

# e_if_expr ::= if e_if_expr else e_if_expr | eps

# e_or0_expr - e_and0_expr e_or_expr

# e_or_expr - or e_and0_expr e_or_expr | eps

# e_and0_expr - e_not_expr e_and_expr

# e_and_expr - and e_not_expr e_and_expr | eps

# e_not_expr - not e_not_expr | cexpr

# cexpr ::= fexpr c_0_expr
# | - cexpr

c_0_expr ::= c_0_expr c_1_expr | eps

c_1_expr ::= . ID c_2_expr
| [ expr ]
| bin_op cexpr

c_2_expr ::= ( {expr {, expr }∗}? ) | eps

bin_op ::= + | - | * | // | % | == | != | <= | >= | < | > | is

target ::= ID
| cexpr target_1

target_1 ::= . ID | [expr]

fexpr ::= ID f_1_expr
| literal
| [ {expr {, expr }∗}? ]
| ( expr )
f_1_expr ::= ( {expr {, expr }∗}? ) | eps
```

This is a rewritten form of the original reference grammar, refactored to eliminate ambiguity, and left-recursion. 

---

Likely the most difficult part of this project was refactoring the grammar in such a way that an Abstact Syntax Tree could still be writtin into the parser without having to pass objects *down* the call stack, only upwards through the the returns.

Another sticking point was the `target` non-terminal. It is still not completly working, and right now just works with simple one target assignments.




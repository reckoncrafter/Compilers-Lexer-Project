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
first_stmt ::= if, while, for, first_simplestmt
first_simplestmt ::= return, pass, first_expr
first_expr ::= if, not, first_cexpr
first_cexpr ::= -, first_fexpr
first_fexpr ::= ID, [, (, first_literal
first_literal = None, True, False, Integer, String, IDSTRING
expr ::= e_00_expr expr | eps
e_00_expr ::= if e_00_expr else e_00_expr | e_01_expr
e_01_expr - e_02_expr e_03_expr
e_03_expr - or e_02_expr e_03_expr | eps
e_02_expr - e_04_expr e_05_expr
e_05_expr - and e_04_expr e_05_expr | eps
e_04_expr - not e_04_expr | cexpr
cexpr ::= fexpr c_0_expr
| - cexpr
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




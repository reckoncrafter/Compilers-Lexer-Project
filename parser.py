from lexer import Lexer, Tokentype, SyntaxErrorException
import ast


class Parser:

    def __init__(self, f):
        self.lexer = Lexer(f)
        self.token = self.lexer.next()

    # Helper function.
    def match(self, type):
        print(self.token.type)
        if self.token.type == type:
            self.token = self.lexer.next()
        else:
            text = "Syntax error: expected {:s} but got {:s} ({:s}).".format(
                type, self.token.type, self.token.lexeme
            )
            raise SyntaxErrorException(text, self.token.location)

    # Helper function
    def match_if(self, type):
        if self.token.type == type:
            self.match(type)
            return True
        return False

    # Finish implementing the parser. A call to parse, parses a single Boolean expression.
    # The file should return an AST if parsing is successful, otherwise a syntax-error exception is thrown.
    def parse(self):
        node = self.cexpr()
        print(node)
        self.match(Tokentype.EOI)
        return node

    #  1 + 2 + 3
    #        +            +
    #       /\           1   +
    #      +  3             2  3
    #     /\
    #    1  2

    # CHOCOPY FULL REFERENCE GRAMMAR
    # program ::= Jvar def | func def | class def K∗ stmt∗
    # class def ::= class ID ( ID ) : NEWLINE INDENT class body DEDENT
    # class body ::= pass NEWLINE
    # | Jvar def | func def K+
    # func def ::= def ID ( Jtyped var J, typed var K∗K? ) J-> typeK? : NEWLINE INDENT func body DEDENT
    # func body ::= Jglobal decl | nonlocal decl | var def | func def K∗ stmt+
    # typed var ::= ID : type
    # type ::= ID | IDSTRING | [ type ]
    # global decl ::= global ID NEWLINE
    # nonlocal decl ::= nonlocal ID NEWLINE
    # var def ::= typed var = literal NEWLINE
    # stmt ::= simple stmt NEWLINE
    # | if expr : block Jelif expr : block K∗ Jelse : block K?
    # | while expr : block
    # | for ID in expr : block
    # simple stmt ::= pass
    # | expr
    # | return Jexpr K?
    # | J target = K+ expr
    # block ::= NEWLINE INDENT stmt+ DEDENT
    # literal ::= None
    # | True
    # | False
    # | INTEGER
    # | IDSTRING | STRING
    # expr ::= cexpr
    # | not expr
    # | expr Jand | orK expr
    # | expr if expr else expr
    # cexpr ::= ID
    # | literal
    # | [ Jexpr J, expr K∗K? ]
    # | ( expr )
    # | member expr
    # | index expr
    # | member expr ( Jexpr J, expr K∗K? )
    # | ID ( Jexpr J, expr K∗K? )
    # | cexpr bin op cexpr
    # | - cexpr
    # bin op ::= + | - | * | // | % | == | != | <= | >= | < | > | is
    # member expr ::= cexpr . ID
    # index expr ::= cexpr [ expr ]
    # target ::= ID
    # | member expr
    # | index expr

    #
    # foo.a.b.c(1,2).d[2].e(4,5)[3]  OK
    # (((((foo.a).b).c(1,2)).d)[2]).e(4,5)[3]  left-associative
    # e(4,5)[3] OK
    # f[3](6,7) NOT OK in ChocoPy

    # cexpr     -> aexpr rel_op aexpr | aexpr

    # rel_op    -> == | != | ... | is
    # aexpr     -> aexpr add_op mexpr | mexpr

    # add_op    -> + | -
    # mexpr     -> mexpr mul_op uexpr | uexpr
    # mul_op    -> * | // | %
    # uexpr     -> - uexpr | mi_expr
    # mi_expr   -> mi_expr . i_or_f | mi_expr [ expr ] | fexpr
    # i_or_f    -> ID | ID '(' arguments ')'
    # fexpr     -> ...

    # cexpr     -> aexpr [ rel_op aexpr ]
    def cexpr(self):
        opmap = {Tokentype.OpGt: ast.Operator.Gt,
            Tokentype.OpLt: ast.Operator.Lt,
            Tokentype.OpEq: ast.Operator.Eq,
            Tokentype.OpGtEq: ast.Operator.GtEq,
            Tokentype.OpLtEq: ast.Operator.LtEq}
        node = self.aexpr()

        if self.token.type in opmap.keys():
            op = opmap[self.token.type]
            self.match(self.token.type) # rel_op
            rhs = self.aexpr()
            node = ast.BinaryOpNode(op, node, rhs)
        return node

    # aexpr     -> mexpr { add_op mexpr }
    def aexpr(self):
        #ops = [Tokentype.OpPlus, Tokentype.OpMinus]
        opmap = {Tokentype.OpPlus: ast.Operator.Plus,
                 Tokentype.OpMinus: ast.Operator.Minus}
        node = self.mexpr()

        if self.token.type in opmap.keys():
            op = opmap[self.token.type]
            self.match(self.token.type) # add_op
            rhs = self.mexpr()
            node = ast.BinaryOpNode(op, node, rhs)
        return node

    # mexpr     -> uexpr { mul_op uexpr }
    def mexpr(self):
        opmap = {Tokentype.OpMultiply: ast.Operator.Multiply,
                 Tokentype.OpIntDivide: ast.Operator.Divide,
                 Tokentype.OpModulus: ast.Operator.Modulus,}
        node = self.uexpr()

        if self.token.type in opmap.keys():
            op = opmap[self.token.type]
            self.match(self.token.type) # mul_op
            rhs = self.uexpr()
            node = ast.BinaryOpNode(op, node, rhs)
        return node
    # uexpr     -> - uexpr | mi_expr
    def uexpr(self):
        if self.match_if(Tokentype.OpMinus):
            child = self.uexpr()
            op = self.token.type
            node = ast.UnaryOpNode(op, child)
        else:
            node = self.mi_expr()
        return node
    # mi_expr   -> fexpr { . i_or_f | '[' expr ']' }
    def mi_expr(self):
        node = self.fexpr()
        if self.match_if(Tokentype.Period):
            node = self.i_or_f()
        if self.match_if(Tokentype.BracketL):
            node = self.expr()
            self.match(Tokentype.BracketR)
    
    # i_or_f    -> ID [ '(' arguments ')' ]
    def i_or_f(self):
        if self.match_if(Tokentype.Identifier):
            if self.match_if(Tokentype.ParenthesisL):
                node = self.arguments()
                self.match(Tokentype.ParenthesisR)
            lexeme = self.token.lexeme
            node = ast.OpNode()
            return node

    def fexpr(self):
        self.match(Tokentype.Identifier)
        node = ast.Node()
        return node

    # Exercise:   1 + 2 > 5 - 4
    #
    #                >
    #            +        -
    #          1   2     5  4

    # sexpr     -> aexpr [ rel_op aexpr ]
    # rel_op    -> > | <
    # aexpr     -> INT { add_op INT }
    # add_op    -> + | -

    # def sexpr(self):
    #     opmap = {Tokentype.OpGt: ast.Operator.Gt,
    #              Tokentype.OpLt: ast.Operator.Lt,
    #              Tokentype.OpEq: ast.Operator.Eq}
    #     node = self.aexpr()

    #     if self.token.type in opmap.keys():
    #         op = opmap[self.token.type]
    #         self.match(self.token.type)
    #         rhs = self.aexpr()
    #         node = ast.BinaryOpNode(op, node, rhs)

    #     return node

    # def aexpr(self):
    #     ops = [Tokentype.OpPlus, Tokentype.OpMinus]
    #     opmap = {Tokentype.OpPlus: ast.Operator.Plus,
    #              Tokentype.OpMinus: ast.Operator.Minus}
    #     lexeme = self.token.lexeme
    #     self.match(Tokentype.IntegerLiteral)
    #     node = ast.IntegerLiteral(int(lexeme))
    #     while self.token.type in ops:
    #         op = opmap[self.token.type]
    #         self.match(self.token.type)
    #         lexeme = self.token.lexeme
    #         self.match(Tokentype.IntegerLiteral)
    #         rhs = ast.IntegerLiteral(int(lexeme))
    #         node = ast.BinaryOpNode(op, node, rhs)
    #     return node
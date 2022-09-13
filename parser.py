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
        node = self.sexpr()
        print(node)
        self.match(Tokentype.EOI)
        return node

    #  1 + 2 + 3
    #        +            +
    #       /\           1   +
    #      +  3             2  3
    #     /\
    #    1  2

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
            Tokenrtype.OpLtEq: ast.Operator.LtEq}
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
    def uxepr(self):
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
            return 

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

    def aexpr(self):
        ops = [Tokentype.OpPlus, Tokentype.OpMinus]
        opmap = {Tokentype.OpPlus: ast.Operator.Plus,
                 Tokentype.OpMinus: ast.Operator.Minus}
        lexeme = self.token.lexeme
        self.match(Tokentype.IntegerLiteral)
        node = ast.IntegerLiteral(int(lexeme))
        while self.token.type in ops:
            op = opmap[self.token.type]
            self.match(self.token.type)
            lexeme = self.token.lexeme
            self.match(Tokentype.IntegerLiteral)
            rhs = ast.IntegerLiteral(int(lexeme))
            node = ast.BinaryOpNode(op, node, rhs)
        return node
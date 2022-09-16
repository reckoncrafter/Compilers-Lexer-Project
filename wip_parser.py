from lexer import Lexer, Tokentype, SyntaxErrorException
import ast


class Parser:

    def __init__(self, f):
        self.lexer = Lexer(f)
        self.token = self.lexer.next()
        self.token_peek = None

        self.bin_op_tokens = {Tokentype.OpPlus, Tokentype.OpMinus, Tokentype.OpMultiply, Tokentype.OpIntDivide,
                              Tokentype.OpModulus,
                              Tokentype.OpEq, Tokentype.OpNotEq, Tokentype.OpLtEq, Tokentype.OpGtEq, Tokentype.OpLt,
                              Tokentype.OpGt, Tokentype.OpIs}

        self.c_1_expr_tokens = {Tokentype.Period, Tokentype.BracketL, Tokentype.OpPlus, Tokentype.OpMinus,
                                Tokentype.OpMultiply, Tokentype.OpIntDivide,
                                Tokentype.OpModulus,
                                Tokentype.OpEq, Tokentype.OpNotEq, Tokentype.OpLtEq, Tokentype.OpGtEq, Tokentype.OpLt,
                                Tokentype.OpGt, Tokentype.OpIs}

        self.first_e_01_expr_tokens = {Tokentype.KwIf, Tokentype.OpNot, Tokentype.OpMinus, Tokentype.Identifier,
                                       Tokentype.BracketL, Tokentype.ParenthesisL, Tokentype.KwNone,
                                       Tokentype.BoolFalseLiteral, Tokentype.BoolTrueLiteral,
                                       Tokentype.IntegerLiteral, Tokentype.StringLiteral}

        self.first_literal_tokens = {Tokentype.KwNone, Tokentype.BoolFalseLiteral, Tokentype.BoolTrueLiteral,
                                     Tokentype.IntegerLiteral, Tokentype.StringLiteral}
        self.first_fexpr_tokens = {Tokentype.Identifier, Tokentype.BracketL, Tokentype.ParenthesisL,
                                   Tokentype.KwNone, Tokentype.BoolFalseLiteral, Tokentype.BoolTrueLiteral,
                                   Tokentype.IntegerLiteral, Tokentype.StringLiteral}
        self.first_cexpr_tokens = {Tokentype.OpMinus, Tokentype.Identifier, Tokentype.BracketL, Tokentype.ParenthesisL,
                                   Tokentype.KwNone, Tokentype.BoolFalseLiteral, Tokentype.BoolTrueLiteral,
                                   Tokentype.IntegerLiteral, Tokentype.StringLiteral}
        self.first_expr_tokens = {Tokentype.OpNot, Tokentype.OpMinus, Tokentype.Identifier, Tokentype.BracketL,
                                  Tokentype.ParenthesisL, Tokentype.KwNone, Tokentype.BoolFalseLiteral,
                                  Tokentype.BoolTrueLiteral, Tokentype.IntegerLiteral, Tokentype.StringLiteral}
        self.first_simplestmt_tokens = {Tokentype.KwReturn, Tokentype.KwPass, Tokentype.OpNot, Tokentype.OpMinus,
                                        Tokentype.Identifier, Tokentype.BracketL, Tokentype.ParenthesisL,
                                        Tokentype.KwNone, Tokentype.BoolFalseLiteral, Tokentype.BoolTrueLiteral,
                                        Tokentype.IntegerLiteral, Tokentype.StringLiteral}
        self.first_stmt_tokens = {Tokentype.KwIf, Tokentype.KwWhile, Tokentype.KwReturn, Tokentype.KwPass,
                                  Tokentype.OpNot, Tokentype.OpMinus, Tokentype.Identifier, Tokentype.BracketL,
                                  Tokentype.ParenthesisL, Tokentype.KwNone, Tokentype.BoolFalseLiteral,
                                  Tokentype.BoolTrueLiteral, Tokentype.IntegerLiteral, Tokentype.StringLiteral}

    # Helper function.
    def match(self, type):
        print('trying matching: ', self.token.type)
        if self.token.type == type:
            print('matching: ', self.token.type)
            if self.token_peek is None:
                self.token = self.lexer.next()
            else:
                self.token = self.token_peek
                self.token_peek = None
        else:
            text = "Syntax error: expected {:s} but got {:s} ({:s}).".format(
                type, self.token.type, self.token.lexeme
            )
            raise SyntaxErrorException(text, self.token.location)

    # Helper function
    def match_if(self, type):
        print('matching: ', self.token.type)
        if self.token.type == type:
            self.match(type)
            return True
        return False

    # Helper function (peek at the next token without (conceptually) consuming it.
    def peek(self):
        if self.token_peek is None:
            self.token_peek = self.lexer.next()
        return self.token_peek

    # Finish implementing the parser. A call to parse, parses a single Boolean expression.
    # The file should return an AST if parsing is successful, otherwise a syntax-error exception is thrown.
    def parse(self):
        self.program()
        self.match(Tokentype.EOI)
        return

    # program ::= {var def | func def | class def }∗ stmt∗
    def program(self):
        print("program()")
        decl = []
        stmt = []
        while self.token.type in {Tokentype.Identifier, Tokentype.KwDef, Tokentype.KwClass}:
            if self.token.type == Tokentype.Identifier and self.peek().type == Tokentype.Colon:
                decl.append(self.var_def())
            elif self.token.type == Tokentype.KwDef:
                decl.append(self.func_def())
            elif self.token.type == Tokentype.KwClass:
                decl.append(self.class_def())
        while self.token.type in self.first_stmt_tokens:
            stmt.append(self.stmt())
        return ast.ProgramNode(decl, stmt)

    # class def ::= class ID ( ID ) : NEWLINE INDENT class body DEDENT
    def class_def(self):
        print("class_def()")
        self.match(Tokentype.KwClass)
        self.match(Tokentype.Identifier)

        self.match(Tokentype.ParenthesisL)
        self.match(Tokentype.Identifier)
        self.match(Tokentype.ParenthesisR)

        self.match(Tokentype.Colon)
        self.match(Tokentype.Newline)
        self.match(Tokentype.Indent)

        self.class_body()

        self.match(Tokentype.Dedent)

    # class body ::= pass NEWLINE
    # | {var def | func def }+
    def class_body(self):
        print("class_body()")
        if self.token.type == Tokentype.KwPass:
            self.match(Tokentype.KwPass)
            self.match(Tokentype.Newline)
            return

        if self.token.type in {Tokentype.Identifier, Tokentype.KwDef}:
            while self.token.type in {Tokentype.Identifier, Tokentype.KwDef}:
                if self.token.type == Tokentype.Identifier and self.peek().type == Tokentype.Colon:
                    self.var_def()
                elif self.token.type == Tokentype.KwDef:
                    self.func_def()
        else:
            raise(SyntaxErrorException("At least one member required", self.token.location))

    # func def ::= def ID ( {typed var {, typed var }∗}? ) {-> type}? : NEWLINE INDENT func body DEDENT
    def func_def(self):
        print("func_def()")
        self.match(Tokentype.KwDef)
        id = self.token.lexeme
        params = []
        return_type = None
        self.match(Tokentype.Identifier)

        self.match(Tokentype.ParenthesisL)
        if self.token.type == Tokentype.Identifier:
            params.append(self.typed_var())
            while self.token.type == Tokentype.Comma:
                self.match(Tokentype.Comma)
                params.append(self.typed_var())
        self.match(Tokentype.ParenthesisR)

        if self.token.type == Tokentype.Arrow:
            self.match(Tokentype.Arrow)
            return_type = self._type()

        self.match(Tokentype.Colon)
        self.match(Tokentype.Newline)
        self.match(Tokentype.Indent)

        fun_decl, fun_stmt = self.func_body()

        self.match(Tokentype.Dedent)
        return ast.FuncDefNode(id, params, return_type, fun_decl, fun_stmt)

    # func body ::= {global decl | nonlocal decl | var def | func def }∗ stmt+
    def func_body(self):
        print("func_body()")

        fun_decl = []
        fun_stmt = []
        while self.token.type in {Tokentype.KwGlobal, Tokentype.KwNonLocal, Tokentype.KwDef} or self.peek().type == Tokentype.Colon:
            if self.token.type == Tokentype.KwGlobal:
                fun_decl.append(self.global_decl())
            elif self.token.type == Tokentype.KwNonLocal:
                fun_decl.append(self.nonlocal_decl())
            elif self.token.type == Tokentype.Identifier and self.peek().type == Tokentype.Colon:
                fun_decl.append(self.var_def())
            elif self.token.type == Tokentype.KwDef:
                fun_decl.append(self.func_def())

        fun_stmt.append(self.stmt())
        while self.token.type in self.first_stmt_tokens:
            fun_stmt.append(self.stmt())

        return fun_decl, fun_stmt

    # typed var ::= ID : type
    def typed_var(self):
        print("typed_var()")
        id_node = ast.IdentifierNode(self.token.lexeme)
        self.match(Tokentype.Identifier)
        self.match(Tokentype.Colon)
        self._type()
        return ast.TypedVarNode(id_node)

    # type ::= ID | IDSTRING | [ type ]
    def _type(self):
        print("_type()")
        node = ast.TypeAnnotationNode()
        if self.token.type == Tokentype.Identifier:
            node = ast.IdentifierNode(self.token.lexeme)
            self.match(Tokentype.Identifier)
        elif self.token.type == Tokentype.StringLiteral:
            node = ast.IdentifierNode(self.token.lexeme)
            self.match(Tokentype.StringLiteral)
        elif self.token.type == Tokentype.BracketL:
            self.match(Tokentype.BracketL)
            self._type()
            self.match(Tokentype.BracketR)
        return node

    # global decl ::= global ID NEWLINE
    def global_decl(self):
        print("global_decl()")
        self.match(Tokentype.KwGlobal)
        ID = self.token.lexeme
        self.match(Tokentype.Identifier)
        self.match(Tokentype.Newline)
        return ast.GlobalDeclNode(ID)

    # nonlocal decl ::= nonlocal ID NEWLINE
    def nonlocal_decl(self):
        print("nonlocal_def()")
        self.match(Tokentype.KwNonLocal)
        ID = self.token.lexeme
        self.match(Tokentype.Identifier)
        self.match(Tokentype.Newline)
        return ast.NonLocalDeclNode(ID)

    # var def ::= typed var = literal NEWLINE
    def var_def(self):
        print("var_def()")
        node = self.typed_var()
        self.match(Tokentype.OpAssign)
        node2 = self.literal()
        self.match(Tokentype.Newline)
        return ast.VarDefNode(node, node2)

    # stmt ::= simple stmt NEWLINE
    # | if expr : block {elif expr : block }∗ {else : block }?
    # | while expr : bloc}
    # | for ID in expr : bloc}
    def stmt(self):
        print("stmt()")
        if self.token.type == Tokentype.KwIf:
            print('token if')
            self.match(Tokentype.KwIf)
            self.expr()
            self.match(Tokentype.Colon)
            self.block()
            while self.token.type == Tokentype.KwElif:
                self.match(Tokentype.KwElif)
                self.expr()
                self.match(Tokentype.Colon)
                self.block()
            if self.token.type == Tokentype.KwElse:
                self.match(Tokentype.KwElse)
                self.match(Tokentype.Colon)
                self.block()
        elif self.token.type == Tokentype.KwWhile:
            self.match(Tokentype.KwWhile)
            self.expr()
            self.match(Tokentype.Colon)
            self.block()
        elif self.token.type == Tokentype.KwFor:
            self.match(Tokentype.Identifier)
            self.match(Tokentype.OpIn)
            self.expr()
            self.match(Tokentype.KwColon)
            self.block()
        elif self.token.type in self.first_simplestmt_tokens:
            print('simple_stmt')
            self.simple_stmt()
            self.match(Tokentype.Newline)

    # simple stmt ::= pass
    # | return {expr }?
    # | expr
    # | { target = }+ expr <--- do this with backtrack
    def simple_stmt(self):
        print("simple_stmt()")
        if self.token.type == Tokentype.KwPass:
            self.match(Tokentype.KwPass)
        elif self.token.type == Tokentype.KwReturn:
            self.match(Tokentype.KwReturn)
            if self.token.type in self.first_expr_tokens:
                self.expr()
        else:
            print("simple_stmt()")
            self.expr()

    # block ::= NEWLINE INDENT stmt+ DEDENT
    def block(self):
        print("block()")
        self.match(Tokentype.Newline)
        self.match(Tokentype.Indent)

        self.stmt()
        while self.token.type in self.first_stmt_tokens:
            self.stmt()

        self.match(Tokentype.Dedent)

    # literal ::= None
    # | True
    # | False
    # | INTEGER
    # | IDSTRING | STRING
    def literal(self):
        print("literal()")
        node = ast.LiteralExprNode()
        if self.token.type == Tokentype.KwNone:
            self.match(Tokentype.KwNone)
            node = ast.NoneLiteralExprNode()
        elif self.token.type == Tokentype.BoolTrueLiteral:
            self.match(Tokentype.BoolTrueLiteral)
            node = ast.BooleanLiteralExprNode(True)
        elif self.token.type == Tokentype.BoolFalseLiteral:
            self.match(Tokentype.BoolFalseLiteral)
            node = ast.BooleanLiteralExprNode(False)
        elif self.token.type == Tokentype.IntegerLiteral:
            id_ = self.token.lexeme
            self.match(Tokentype.IntegerLiteral)
            node = ast.IntegerLiteralExprNode(id_)
        elif self.token.type == Tokentype.StringLiteral:
            id_ = self.token.lexeme
            self.match(Tokentype.StringLiteral)
            node = ast.StringLiteralExprNode(id_)
        return node

    # first_stmt = if, while, for, first_simplestmt
    # first_simplestmt = return, pass, first_expr
    # first_expr = not, first_cexpr
    # first_c_expr = -, first_fexpr
    # first_fexpr = ID, [, (, first_literal
    # first_literal = None, True, False, Integer, String, IDSTRING

    # expr ::= e_01_expr e_000_expr
    def expr(self):
        print("expr()")
        self.e_01_expr()
        self.e_000_expr()
        return

    # e_000_expr ::= e_00_expr e_000_expr | eps
    def e_000_expr(self):
        print("e_000_expr()")
        if self.token.type in self.first_e_01_expr_tokens:
            self.e_00_expr()
            self.e_000_expr()
        return

    # e_00_expr ::= if e_00_expr else e_00_expr | e_01_expr
    def e_00_expr(self):
        print("e_00_expr()")
        if self.token.type == Tokentype.KwIf:
            self.match(Tokentype.KwIf)
            self.e_00_expr()
            self.match(Tokentype.KwElse)
            self.e_00_expr()
        else:
            self.e_01_expr()
        return

    # e_01_expr - e_02_expr e_03_expr
    def e_01_expr(self):
        print("e_01_expr()")
        self.e_02_expr()
        self.e_03_expr()
        return

    # e_03_expr - or e_02_expr e_03_expr | eps
    def e_03_expr(self):
        print("e_03_expr()")
        if self.token.type == Tokentype.OpOr:
            self.match(Tokentype.OpOr)
            self.e_02_expr()
            self.e_03_expr()
        return

    # e_02_expr - e_04_expr e_05_expr
    def e_02_expr(self):
        print("e_02_expr()")
        self.e_04_expr()
        self.e_05_expr()
        return

    # e_05_expr - and e_04_expr e_05_expr | eps
    def e_05_expr(self):
        print("e_05_expr()")
        if self.token.type == Tokentype.OpAnd:
            self.match(Tokentype.OpAnd)
            self.e_04_expr()
            self.e_05_expr()
        return

    # e_04_expr - not e_04_expr | cexpr
    def e_04_expr(self):
        print("e_0_expr()")
        if self.token.type == Tokentype.OpNot:
            self.match(Tokentype.OpNot)
            self.e_04_expr()
        else:
            self.cexpr()

    # cexpr ::= fexpr c_0_expr
    # | - cexpr
    def cexpr(self):
        print("cexpr()")
        if self.token.type == Tokentype.OpMinus:
            self.cexpr()
        else:
            self.fexpr()
            self.c_0_expr()

    # c_0_expr ::= c_1_expr c_0_expr | eps
    def c_0_expr(self):
        print("c_0_expr()")
        if self.token.type in self.c_1_expr_tokens:
            self.c_1_expr()
            self.c_0_expr()
        return

    # c_1_expr ::= . ID c_2_expr
    # | [ expr ]
    # | bin op cexpr
    def c_1_expr(self):
        print("c_1_expr()")
        if self.token.type in self.bin_op_tokens:
            self.bin_op()
            self.cexpr()
        elif self.token.type == Tokentype.Period:
            self.match(Tokentype.Period)
            self.match(Tokentype.Identifier)
            self.c_2_expr()
        elif self.token.type == Tokentype.BracketL:
            self.expr()
            self.match(Tokentype.BracketR)
        else:
            raise(SyntaxErrorException("Invalid expression", self.token.location))

    # c_2_expr ::= ( {expr {, expr }∗}? ) | eps
    def c_2_expr(self):
        print("c_2_expr()")
        if self.token.type == Tokentype.ParenthesisL:
            self.match(Tokentype.ParenthesisL)
            if self.token.type in self.first_expr_tokens:
                self.expr()
                while self.token.type == Tokentype.Comma:
                    self.expr()
            self.match(Tokentype.ParenthesisR)
            return # node
        return # eps

    # bin op ::= + | - | * | // | % | == | != | <= | >= | < | > | is
    def bin_op(self):
        self.match(self.token.type)
        return # bin_op node

    # target ::= ID
    # | cexpr target_1
    def target(self):
        print("target()")
        if self.token.type == Tokentype.Identifier:
            self.match(Tokentype.Identifier)
            return # id
        elif self.token.type in self.first_cexpr_tokens:
            self.cexpr()
            self.target_1()
        else:
            raise(SyntaxErrorException("Invalid target", self.token.location))

    # target_1 ::= .D | [expr]
    def target_1(self):
        print("target_1()")
        if self.token.type == Tokentype.Period:
            self.match(Tokentype.Period)
            self.match(Tokentype.Identifier)
        elif self.token.type == Tokentype.BracketL:
            self.match(Tokentype.BracketL)
            self.expr()
            self.match(Tokentype.BracketR)
        else:
            raise(SyntaxErrorException("Invalid target", self.token.location))

    # fexpr ::= ID f_1_expr
    # | literal
    # | [ {expr {, expr }∗}? ]
    # | ( expr )
    def fexpr(self):
        print("fexpr()")
        if self.token.type == Tokentype.Identifier:
            self.match(Tokentype.Identifier)
            self.f_1_expr()
        elif self.token.type in self.first_literal_tokens:
            self.literal()
        elif self.token.type == Tokentype.BracketL:
            self.match(Tokentype.BracketL)
            if self.token.type in self.first_expr_tokens:
                self.expr()
                while self.token.type == Tokentype.Comma:
                    self.expr()
            self.match(Tokentype.BracketR)
        elif self.token.type == Tokentype.ParenthesisL:
            self.match(Tokentype.ParenthesisL)
            self.expr()
            self.match(Tokentype.ParenthesisR)
        else:
            raise(SyntaxErrorException("Invalid Expression", self.token.location))

        # f_1_expr ::= ( {expr {, expr }∗}? ) | eps

    def f_1_expr(self):
        print("f_1_expr()")
        if self.token.type == Tokentype.ParenthesisL:
            self.match(Tokentype.ParenthesisL)
            self.expr()
            while self.token.type == Tokentype.Comma:
                self.match(Tokentype.Comma)
                self.expr()
            self.match(Tokentype.ParenthesisR)
        return


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

        self.first_e_or0_expr_tokens = {Tokentype.KwIf, Tokentype.OpNot, Tokentype.OpMinus, Tokentype.Identifier,
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

        if self.token.type == type:

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
        #print('matching: ', self.token.type)
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
        node = self.program()
        self.match(Tokentype.EOI)
        return node

    # program ::= {var def | func def | class def }∗ stmt∗
    def program(self):
        #print("program()")
        decl = []
        stmt = []
        while self.token.type in {Tokentype.Identifier, Tokentype.KwDef, Tokentype.KwClass}:
            #print('here')
            if self.token.type == Tokentype.Identifier and self.peek().type == Tokentype.Colon:
                #print('here')
                decl.append(self.var_def())
            elif self.token.type == Tokentype.KwDef:
                #print('here')
                decl.append(self.func_def())
            elif self.token.type == Tokentype.KwClass:
                #print('here')
                decl.append(self.class_def())
            else:
                #print(decl)
                break
            #print(decl)
        while self.token.type in self.first_stmt_tokens:
            #print('waaat')
            singlestmt = self.stmt()
            stmt.append(singlestmt)

        return ast.ProgramNode(decl, stmt)

    # class def ::= class ID ( ID ) : NEWLINE INDENT class body DEDENT
    def class_def(self):
        #print("class_def()")
        decl = []
        self.match(Tokentype.KwClass)

        id_ = self.token.lexeme
        #print(id_)
        self.match(Tokentype.Identifier)
        node = ast.IdentifierNode(id_)

        self.match(Tokentype.ParenthesisL)
        id_2 = self.token.lexeme
        self.match(Tokentype.Identifier)
        node2 = ast.IdentifierNode(id_2)

        self.match(Tokentype.ParenthesisR)

        self.match(Tokentype.Colon)
        self.match(Tokentype.Newline)
        self.match(Tokentype.Indent)

        decl = self.class_body()

        self.match(Tokentype.Dedent)

        return ast.ClassDefNode(node, node2, decl)

    # class body ::= pass NEWLINE
    # | {var def | func def }+
    def class_body(self):
        #print("class_body()")
        decl = []
        if self.token.type == Tokentype.KwPass:
            self.match(Tokentype.KwPass)
            self.match(Tokentype.Newline)
            return decl
        if self.token.type in {Tokentype.Identifier, Tokentype.KwDef}:
            while self.token.type in {Tokentype.Identifier, Tokentype.KwDef}:
                if self.token.type == Tokentype.Identifier and self.peek().type == Tokentype.Colon:
                    decl.append(self.var_def())
                elif self.token.type == Tokentype.KwDef:
                    decl.append(self.func_def())
        else:
            raise(SyntaxErrorException("At least one member required", self.token.location))
        return decl

    # func def ::= def ID ( {typed var {, typed var }∗}? ) {-> type}? : NEWLINE INDENT func body DEDENT
    def func_def(self):
        #print("func_def()")
        self.match(Tokentype.KwDef)
        id = self.token.lexeme
        node_id = ast.IdentifierNode(id)
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
        return ast.FuncDefNode(node_id, params, return_type, fun_decl, fun_stmt)

    # func body ::= {global decl | nonlocal decl | var def | func def }∗ stmt+
    def func_body(self):
        #print("func_body()")

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
        #print("typed_var()")
        id_node = ast.IdentifierNode(self.token.lexeme)
        self.match(Tokentype.Identifier)
        self.match(Tokentype.Colon)
        id_type = self._type()
        return ast.TypedVarNode(id_node, id_type)

    # type ::= ID | IDSTRING | [ type ]
    def _type(self):
        #print("_type()")
        node = ast.TypeAnnotationNode()
        if self.token.type == Tokentype.Identifier:
            node = ast.ClassTypeAnnotationNode(self.token.lexeme)
            self.match(Tokentype.Identifier)
        elif self.token.type == Tokentype.StringLiteral:
            node = ast.ClassTypeAnnotationNode(self.token.lexeme)
            self.match(Tokentype.StringLiteral)
        elif self.token.type == Tokentype.BracketL:
            self.match(Tokentype.BracketL)
            node = ast.ListTypeAnnotationNode[self._type()]
            self.match(Tokentype.BracketR)
        return node

    # global decl ::= global ID NEWLINE
    def global_decl(self):
        #print("global_decl()")
        self.match(Tokentype.KwGlobal)
        ID = self.token.lexeme
        self.match(Tokentype.Identifier)
        self.match(Tokentype.Newline)
        return ast.GlobalDeclNode(ID)

    # nonlocal decl ::= nonlocal ID NEWLINE
    def nonlocal_decl(self):
        #print("nonlocal_def()")
        self.match(Tokentype.KwNonLocal)
        ID = self.token.lexeme
        self.match(Tokentype.Identifier)
        self.match(Tokentype.Newline)
        return ast.NonLocalDeclNode(ID)

    # var def ::= typed var = literal NEWLINE
    def var_def(self):
        #print("var_def()")
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
        #print("stmt()")
        node = ast.ExprNode()
        if self.token.type == Tokentype.KwIf:
            #print('token if')
            elifs = []

            self.match(Tokentype.KwIf)
            cond_ = self.expr()
            self.match(Tokentype.Colon)
            then_ = self.block()
            node = ast.IfStmtNode(cond_, then_, [], [])
            while self.token.type == Tokentype.KwElif:
                self.match(Tokentype.KwElif)
                elif_expr = self.expr()
                self.match(Tokentype.Colon)
                elif_body = self.block()
                elifs.append([elif_expr, elif_body])
                #print(elifs)
                node = ast.IfStmtNode(cond_, then_, elifs, None)
            if self.token.type == Tokentype.KwElse:
                self.match(Tokentype.KwElse)
                self.match(Tokentype.Colon)
                else_body = self.block()
                node = ast.IfStmtNode(cond_, then_, elifs, else_body)
        elif self.token.type == Tokentype.KwWhile:
            self.match(Tokentype.KwWhile)
            expr = self.expr()
            self.match(Tokentype.Colon)
            decl = self.block()
            node = ast.WhileStmtNode(expr, decl)
        elif self.token.type == Tokentype.KwFor:
            self.match(Tokentype.KwFor)
            id_ = self.token.lexeme
            self.match(Tokentype.Identifier)
            id_node = ast.IdentifierNode(id_)
            self.match(Tokentype.OpIn)
            iter_node = self.expr()
            self.match(Tokentype.Colon)
            body_list_node = self.block()
            node = ast.ForStmtNode(id_node, iter_node, body_list_node)
        elif self.token.type in self.first_simplestmt_tokens:
            #print('simple_stmt')
            node = self.simple_stmt()
            #print(node)
            targets = [node]
            #print(self.token.type)
            if self.token.type == Tokentype.OpAssign:
                self.match(Tokentype.OpAssign)
                node2 = self.expr()
                # QUESTION:
                # Using only one look ahead, how is the parser supposed to correctly identify the following cases:
                # x = y[0].n + 5   //  AssignStmtNode(), targets: x, node: ( y[0].n + 5 )
                # and
                # x = y[0].n = 5  //   AssignStmtNode(), targets: x, y[0].n, node: (5)
                node = ast.AssignStmtNode(targets, node2)
                self.match(Tokentype.Newline)
            else:
                self.match(Tokentype.Newline)

        else:
            return None #this is wrong

        return node

    # simple stmt ::= pass
    # | return {expr }?
    # | expr
    # | { target = }+ expr <--- do this with backtrack
    def simple_stmt(self):
        #print("simple_stmt()")
        node = ast.StmtNode()
        if self.token.type == Tokentype.KwPass:
            self.match(Tokentype.KwPass)
            node = ast.PassStmtNode()
        elif self.token.type == Tokentype.KwReturn:
            self.match(Tokentype.KwReturn)
            if self.token.type in self.first_expr_tokens:
                return_expr = self.expr()
                node = ast.ReturnStmtNode(return_expr)
            else:
                node = ast.ReturnStmtNode(None)
        else:
            #print("simple_stmt()")
            node = self.expr()
            #node = ast.ExprStmt(node) # ExprStmt CAN'T BE USED BY #PRINT VISITOR, so this is commented
        return node

    # block ::= NEWLINE INDENT stmt+ DEDENT
    def block(self):
        #print("block()")
        self.match(Tokentype.Newline)
        self.match(Tokentype.Indent)

        stmt = []
        stmt.append(self.stmt())

        while self.token.type in self.first_stmt_tokens:
            stmt.append(self.stmt())

        self.match(Tokentype.Dedent)
        return stmt

    # literal ::= None
    # | True
    # | False
    # | INTEGER
    # | IDSTRING | STRING
    def literal(self):
        #print("literal()")
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

    # expr ::= e_or0_expr e_if0_expr
    def expr(self):
        #print("expr()")
        node = self.e_or0_expr()
        node2 = self.e_if0_expr()
        if node2 is ast.ListExprNode:
            #print('expr_if_here')
            return ast.IfExprNode(node, node2[0], node2[1])
        else:
            return node

    # e_if0_expr ::= e_if_expr | eps
    def e_if0_expr(self):
        #print("e_if0_expr()")
        if self.token.type in self.first_e_or0_expr_tokens:
            node_or_list = self.e_if_expr()
            return node_or_list
        else:
            return None

    # e_if_expr ::= if e_if_expr else e_if_expr | eps
    def e_if_expr(self):
        #print("e_if_expr()")
        if self.token.type == Tokentype.KwIf:
            self.match(Tokentype.KwIf)
            node = self.e_if_expr()
            self.match(Tokentype.KwElse)
            node2 = self.e_if_expr()
            return ast.ListExprNode([node, node2])
        else:
            return None

    # e_or0_expr - e_and0_expr e_or_expr
    def e_or0_expr(self):
        #print("e_or0_expr()")
        node = self.e_and0_expr()
        node2 = self.e_or_expr()
        if node2 is None:
            return node
        else:
            return ast.BinaryOpExprNode(ast.Operator.Or, node, node2)

    # e_or_expr - or e_and0_expr e_or_expr | eps
    def e_or_expr(self):
        #print("e_or_expr()")
        if self.token.type == Tokentype.OpOr:
            self.match(Tokentype.OpOr)
            node = self.e_and0_expr()
            node2 = self.e_or_expr()
            if node2 is None:
                return node
            else:
                return ast.BinaryOpExprNode(ast.Operator.Or, node, node2)

        return

    # e_and0_expr - e_not_expr e_and_expr
    def e_and0_expr(self):
        #print("e_and0_expr()")
        node = self.e_not_expr()
        node2 = self.e_and_expr()
        if node2 is None:
            return node
        else:
            return ast.BinaryOpExprNode(ast.Operator.And, node, node2)

    # e_and_expr - and e_not_expr e_and_expr | eps
    def e_and_expr(self):
        #print("e_and_expr()")
        if self.token.type == Tokentype.OpAnd:
            self.match(Tokentype.OpAnd)
            node = self.e_not_expr()
            node2 = self.e_and_expr()
            if node2 is None:
                return node
            else:
                return ast.BinaryOpExprNode(ast.Operator.And, node, node2)
        return None

    # e_not_expr - not e_not_expr | cexpr
    def e_not_expr(self):
        #print("e_not_expr()")
        if self.token.type == Tokentype.OpNot:
            self.match(Tokentype.OpNot)
            node = self.e_not_expr()
            return ast.UnaryOpExprNode(ast.Operator.Not, node)
        else:
            node = self.cexpr()
            return node

    # cexpr ::= fexpr c_0_expr
    # | - cexpr
    def cexpr(self):
        #print("cexpr()")
        if self.token.type == Tokentype.OpMinus:
            self.match(Tokentype.OpMinus)
            node = self.cexpr()
            return ast.UnaryOpExprNode(ast.Operator.Minus, node)
        else:
            node = self.fexpr()
            if self.token.type in self.bin_op_tokens:
                node = ast.BinaryOpExprNode(self.token.type, node, self.c_0_expr())
            elif self.token.type == Tokentype.Period:
                a, b = self.c_0_expr()
                mem = ast.MemberExprNode(node, a)
                node = ast.MethodCallExprNode(node, b)
            elif self.token.type == Tokentype.BracketL:
                node = ast.IndexExprNode(node, self.c_0_expr())
            return node

    # c_0_expr ::= . ID c_2_expr c_0_expr | [ expr ] c_0_expr | bin op cexpr c_0_expr | eps


    def c_0_expr(self):
        #print("c_0_expr()")
        if self.token.type in self.c_1_expr_tokens:
            lists = []
            if self.token.type in self.bin_op_tokens:
                while self.token.type in self.bin_op_tokens:
                    self.bin_op()
                    node2 = self.cexpr()
                return node2
            elif self.token.type == Tokentype.Period:
                self.match(Tokentype.Period)
                id_ = self.token.lexeme
                self.match(Tokentype.Identifier)
                node_id = ast.IdentifierNode(id_)
                if self.token.type == Tokentype.ParenthesisL:
                    node2 = self.c_2_expr()
                    while self.token.type == Tokentype.Comma:
                        lists.append(self.c_2_expr())
                return [node_id, lists]
            elif self.token.type == Tokentype.BracketL:
                self.match(Tokentype.BracketL)
                node = self.expr()
                self.match(Tokentype.BracketR)
                return node
            else:
                raise (SyntaxErrorException("Invalid expression", self.token.location))

            return node
        return None


    # c_2_expr ::= ( {expr {, expr }∗}? ) | eps
    def c_2_expr(self):
        #print("c_2_expr()")

        if self.token.type == Tokentype.ParenthesisL:
            self.match(Tokentype.ParenthesisL)
            exs = []
            if self.token.type in self.first_expr_tokens:
                exs.append(self.expr())
                while self.token.type == Tokentype.Comma:
                    exs.append(self.expr())
            self.match(Tokentype.ParenthesisR)
            return exs
        return None

    # bin op ::= + | - | * | // | % | == | != | <= | >= | < | > | is
    def bin_op(self):
        type_ = self.token.type
        self.match(self.token.type)
        return type_

    # target ::= ID
    # | cexpr target_1
    def target(self):
        #print("target()")
        if self.token.type == Tokentype.Identifier and self.peek().type != Tokentype.BracketL and self.peek().type != Tokentype.Period:
            id_ = self.token.lexeme
            self.match(Tokentype.Identifier)
            return ast.IdentifierNode(id_)
        elif self.token.type in self.first_cexpr_tokens:
            node = self.cexpr()
            node2 = self.target_1()
            if node2 is ast.IdentifierNode:
                return ast.MemberExprNode(node, node2)
            else:
                return ast.IndexExprNode(node, node2)
        else:
            raise(SyntaxErrorException("Invalid target", self.token.location))

    # target_1 ::= .ID | [expr]
    def target_1(self):
        #print("target_1()")
        if self.token.type == Tokentype.Period:
            self.match(Tokentype.Period)
            id_ = self.token.lexeme
            self.match(Tokentype.Identifier)
            node = ast.IdentifierNode(id_)
            return node
        elif self.token.type == Tokentype.BracketL:
            self.match(Tokentype.BracketL)
            node = [self.expr()]
            self.match(Tokentype.BracketR)
            return node
        else:
            raise(SyntaxErrorException("Invalid target", self.token.location))

    # fexpr ::= ID f_1_expr
    # | literal
    # | [ {expr {, expr }∗}? ]
    # | ( expr )
    def fexpr(self):
        #print("fexpr()")
        node = ast.ExprNode()
        if self.token.type == Tokentype.Identifier:
            #print('identifier')
            id_ = self.token.lexeme
            self.match(Tokentype.Identifier)
            #print(self.token.type == Tokentype.ParenthesisL)
            #print(self.peek().type)
            if self.token.type == Tokentype.ParenthesisL and self.peek().type == Tokentype.ParenthesisR:
                self.match(Tokentype.ParenthesisL)
                self.match(Tokentype.ParenthesisR)
                return ast.FunctionCallExprNode(ast.IdentifierNode(id_), [])
            else:
                node_exprs = self.f_1_expr()
                if node_exprs is None:
                    return ast.IdentifierNode(id_)
                else:
                    return ast.FunctionCallExprNode(ast.IdentifierNode(id_), node_exprs)
        elif self.token.type in self.first_literal_tokens:
            node = self.literal()
            return node
        elif self.token.type == Tokentype.BracketL:
            self.match(Tokentype.BracketL)
            node_exprs = []
            if self.token.type in self.first_expr_tokens:
                node_exprs = [self.expr()]
                while self.token.type == Tokentype.Comma:
                    self.match(Tokentype.Comma)
                    node_exprs.append(self.expr())
            self.match(Tokentype.BracketR)
            return ast.ListExprNode(node_exprs)
        elif self.token.type == Tokentype.ParenthesisL:
            self.match(Tokentype.ParenthesisL)
            node = self.expr()
            self.match(Tokentype.ParenthesisR)
            return node
        else:
            raise(SyntaxErrorException("Invalid Expression", self.token.location))

        # f_1_expr ::= ( {expr {, expr }∗}? ) | eps

    def f_1_expr(self):
        #print("f_1_expr()")
        if self.token.type == Tokentype.ParenthesisL:
            self.match(Tokentype.ParenthesisL)
            nodes_ = []
            n = self.expr()
            if n is not None:
                nodes_.append(n)
                while self.token.type == Tokentype.Comma:
                    self.match(Tokentype.Comma)
                    nodes_.append(self.expr())
            self.match(Tokentype.ParenthesisR)
            return nodes_
        else:
            return None


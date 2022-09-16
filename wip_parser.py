from lexer import Lexer, Tokentype, SyntaxErrorException
import astree


class Parser:


    def __init__(self, f):
        self.lexer = Lexer(f)
        self.token = self.lexer.next()
        self.token_peek = None

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
        print(node)
        self.match(Tokentype.EOI)
        return node

    #def peek(self):

    # CHOCOPY FULL REFERENCE GRAMMAR

    stmt_keywords = {Tokentype.KwIf, Tokentype.KwFor, Tokentype.KwWhile, Tokentype.KwPass}
    simple_stmt_keywords = {Tokentype.KwPass, Tokentype.KwReturn}

<<<<<<< HEAD
    def program(self, stmt_keywords=stmt_keywords):
        dec = []
        stmt = []
=======
    # program ::= [[ var def | func def | class def ]]∗ stmt∗
    def program(self):
        print("program()")
>>>>>>> 1c3a8d0530483ef9715585c9b728a6def00eee8f
        while self.token.type in {Tokentype.KwDef, Tokentype.KwClass, Tokentype.Identifier}:
            if self.token.type == Tokentype.KwDef:
                dec.append(self.func_def())
            elif self.token.type == Tokentype.KwClass:
<<<<<<< HEAD
                dec.append(self.class_def())
            elif self.peek() == Tokentype.Colon:
                # implement peek()
                # The colon is used for type annotations after the identifier. i.e "var : int = ..."
                dec.append(self.var_def())
        while self.token.type in stmt_keywords:
            stmt.append(self.stmt())
        return astree.ProgramNode(dec, stmt)
=======
                self.class_def()
           #elif self.peek() == Tokentype.Colon:
            elif self.token.type == Tokentype.Identifier:
                # implement peek()
                # The colon is used for type annotations after the identifier. i.e "var : int = ..."
                self.var_def()
        while self.token.type in self.stmt_keywords:
            self.stmt()
>>>>>>> 1c3a8d0530483ef9715585c9b728a6def00eee8f

    # class def ::= class ID ( ID ) : NEWLINE INDENT class body DEDENT
    def class_def(self):
        print("class_def()")
        self.match(Tokentype.KwClass)
        self.match(Tokentype.Identifier)

        # self.match(Tokentype.ParenthesisL)
        # self.match(Tokentype.Identifier)
        # self.match(Tokentype.ParenthesisR)

        self.match(Tokentype.Colon)
        self.match(Tokentype.Newline)
        self.match(Tokentype.Indent)

        self.class_body()

        self.match(Tokentype.Dedent)

    # class body ::= pass NEWLINE
    # | [[var def | func def ]]+
    def class_body(self):
        print("class_body()")
        if self.token.type == Tokentype.KwPass:
            self.match(Tokentype.Newline)

        if self.token.type not in {Tokentype.Identifier, Tokentype.KwDef}: 
            raise(SyntaxErrorException) # need at least one
        
        while self.token.type in {Tokentype.Identifier, Tokentype.KwDef}:
            if self.token.type == Tokentype.Identifier:
                self.var_def()
            elif self.token.type == Tokentype.KwDef:
                self.func_def()
    
    # func def ::= def ID ( [[typed var [[, typed var ]]∗]]? ) [[-> type]]? : NEWLINE INDENT func body DEDENT
    def func_def(self):
        print("func_def()")
        self.match(Tokentype.KwDef)
        self.match(Tokentype.Identifier)

        self.match(Tokentype.ParenthesisL)
        if self.token.type != Tokentype.ParenthesisR:
            self.typed_var()
            while self.token.type == Tokentype.Comma:
                self.typed_var()

        self.match(Tokentype.ParenthesisR)
        
        if self.token.type == Tokentype.Arrow:
            self.type()
        
        self.match(Tokentype.Colon)
        self.match(Tokentype.Newline)
        self.match(Tokentype.Indent)

        self.func_body()

        self.match(Tokentype.Dedent)
        
    # func body ::= [[global decl | nonlocal decl | var def | func def ]]∗ stmt+
    def func_body(self):
        print("func_body()")
        while self.token.type in {Tokentype.KwGlobal, Tokentype.KwNonLocal, Tokentype.Identifier, Tokentype.KwDef}:
            if self.token.type == Tokentype.KwGlobal:
                self.global_decl()
            elif self.token.type == Tokentype.KwNonLocal:
                self.nonlocal_decl()
            elif self.token.type == Tokentype.Identifier:
                self.var_def()
            elif self.token.type == Tokentype.KwDef:
                self.func_def()

        if self.token.type not in self.stmt_keywords:
            raise(SyntaxError("Statement keyword missing"))
        while self.token.type in self.stmt_keywords:
            self.stmt()
        
    # typed var ::= ID : type
    def typed_var(self):
        print("typed_var()")
        self.match(Tokentype.Identifier)
        if self.token.type == Tokentype.Colon:
            self.match(Tokentype.Colon)
            self._type()
    
    # type ::= ID | IDSTRING | [ type ]
    def _type(self):
        print("_type()")
        # Need to define IDSTRING, and primitive types
        if self.token.type == Tokentype.Identifier:
            self.match(Tokentype.Identifier)
        elif self.token.type == Tokentype.StringLiteral:
            self.match(Tokentype.StringLiteral)
        else:
            raise(SyntaxError("Invalid type annotation"))
        
    # global decl ::= global ID NEWLINE
    def global_decl(self):
        print("global_decl()")
        self.match(Tokentype.KwGlobal)
        self.match(Tokentype.Identifier)
        self.match(Tokentype.Newline)

    # nonlocal decl ::= nonlocal ID NEWLINE
    def nonlocal_decl(self):
        print("nonlocal_decl()")
        self.match(Tokentype.KwNonLocal)
        self.match(Tokentype.Identifier)
        self.match(Tokentype.Newline)
    
    # var def ::= typed var = literal NEWLINE
    def var_def(self):
        print("var_def()")
        self.typed_var()
        self.match(Tokentype.OpAssign)
        self.literal()
        self.match(Tokentype.Newline)

    # stmt ::= simple stmt NEWLINE
    # | if expr : block [[elif expr : block ]]∗ [[else : block ]]?
    # | while expr : block
    # | for ID in expr : block
    def stmt(self):
        print("stmt()")
        if self.token.type in self.simple_stmt_keywords:
            self.simple_stmt()
            self.match(Tokentype.Newline)    
        elif self.token.type == Tokentype.KwIf:
            self.match(Tokentype.KwIf)
            self.expr()
            self.match(Tokentype.Colon)
            self.block()
            while self.token.type == Tokentype.KwElif:
                self.expr()
                self.match(Tokentype.Colon)
                self.block()
            if self.token.type == Tokentype.KwElse:
                self.block()
        elif self.token.type == Tokentype.KwWhile:
            self.match(Tokentype.KwWhile)
            self.expr()
            self.match(Tokentype.Colon)
            self.block()
        elif self.token.type == Tokentype.KwFor:
            self.match(Tokentype.KwFor)
            self.match(Tokentype.Identifier)
            self.match(Tokentype.OpIn)
            self.expr()
            self.match(Tokentype.Colon)
            self.block()
        else:
            raise(SyntaxError("Invalid statement"))

    # simple stmt ::= pass
    # | expr
    # | return [[expr]]?
    # | [[target = ]]+ expr
    def simple_stmt(self):
        print("simple_stmt()")
        if self.token.type == Tokentype.KwPass:
            self.match(Tokentype.KwPass)
        elif self.token.type == Tokentype.KwReturn:
            self.match(Tokentype.KwReturn)
            if self.token.type != Tokentype.Newline:
                self.expr()
        elif self.token.type == Tokentype.Identifier:
            while self.token.type == Tokentype.Identifier:
                self.target()
                self.match(Tokentype.OpAssign)
            self.expr()
        else:
            self.expr()

    # block ::= NEWLINE INDENT stmt+ DEDENT
    def block(self):
        print("block()")
        self.match(Tokentype.Newline)
        self.match(Tokentype.Indent)
        if self.token.type not in self.stmt_keywords:
            raise(SyntaxError("Invalid Block"))
        while self.token.type in self.stmt_keywords:
            self.stmt()
        self.match(Tokentype.Dedent)
    
    # literal ::= None
    # | True
    # | False
    # | INTEGER
    # | IDSTRING | STRING
    literal_tokens = {Tokentype.KwNone, Tokentype.BoolFalseLiteral, Tokentype.BoolTrueLiteral, 
                      Tokentype.StringLiteral, Tokentype.IntegerLiteral}
    def literal(self):
        print("literal()")
        if self.token.type == Tokentype.KwNone:
            self.match(Tokentype.KwNone)
        elif self.token.type == Tokentype.BoolTrueLiteral:
            self.match(Tokentype.BoolTrueLiteral)
        elif self.token.type == Tokentype.BoolFalseLiteral:
            self.match(Tokentype.BoolFalseLiteral)
        elif self.token.type == Tokentype.IntegerLiteral:
            self.match(Tokentype.IntegerLiteral)
        elif self.token.type == Tokentype.StringLiteral:
            # Define IDSTRING
            self.match(Tokentype.StringLiteral)
        
    
    # expr ::= cexpr
    # | not expr
    # | expr [[and | or]] expr
    # | expr if expr else expr
    def expr(self):
        print("expr()")
        if self.token.type == Tokentype.OpNot:
            self.match(Tokentype.OpNot)
            self.expr()
        elif self.token.type == Tokentype.Identifier:
            self.cexpr()
        else:
            self.expr()
            if self.token.type == Tokentype.OpAnd:
                self.match(Tokentype.OpAnd)
                self.expr()
            elif self.token.type == Tokentype.OpOr:
                self.match(Tokentype.OpOr)
                self.expr()

    
    # cexpr ::= ID
    # | literal
    # | [ [[expr [[, expr ]]∗]]? ]
    # | ( expr )
    # | member expr // cexpr . ID
    # | index expr // cexpr [expr]
    # | member expr ( [[expr [[, expr ]]∗]]? )
    # | ID ( [[expr [[, expr ]]∗]]? )
    # | cexpr bin op cexpr
    # | - cexpr
    def cexpr(self):
        print("cexpr()")
        if self.token.type == Tokentype.Identifier:
            self.match(Tokentype.Identifier)
            if self.token.type == Tokentype.ParenthesisL:
                self.match(Tokentype.ParenthesisL)
                if self.token.type != Tokentype.ParenthesisR:
                    self.expr()
                    while self.token.type == Tokentype.Comma:
                        self.expr()
                self.match(Tokentype.ParenthesisR)
            elif self.token.type in self.bin_ops:
                self.bin_op()
                self.cexpr()
            return

        elif self.token.type in self.literal_tokens:
            self.literal()
        
        elif self.token.type == Tokentype.BracketL:
            self.match(Tokentype.BracketL)
            if self.token.type != Tokentype.BracketR:
                self.expr()
                while self.token.type == Tokentype.Comma:
                    self.expr()
            self.match(Tokentype.BracketR)
            return

        elif self.token.type == Tokentype.ParenthesisL:
            self.match(Tokentype.ParenthesisL)
            self.expr()
            self.match(Tokentype.ParenthesisR)
            return
        
        elif self.token.type == Tokentype.Period:
            while self.token.type == Tokentype.Period:
                self.member_expr()
            return

        elif self.token.type == Tokentype.BracketL:
            while self.token.type == Tokentype.BracketL:
                self.index_expr()
            return

        # try:
        #     self.member_expr()
        # except:
        #     pass
        # else:
        #     if self.token.type == Tokentype.ParenthesisL:
        #         self.match(Tokentype.ParenthesisL)
        #         if self.token.type != Tokentype.ParenthesisR:
        #             self.expr()
        #             while self.token.type == Tokentype.Comma:
        #                 self.expr()
        #         self.match(Tokentype.ParenthesisR)
        #     return

        # try:
        #     self.index_expr()
        # except:
        #     pass
        # else:
        #     return

        # try:
        #     self.cexpr()
        # except:
        #     pass
        # else:
        #     self.bin_op()
        #     self.cexpr()
        #     return
        
        elif self.token.type == Tokentype.OpMinus:
            self.cexpr()
            return

        else:
            raise(SyntaxError(str(self.token.type) + "???"))

    # bin op ::= + | - | * | // | % | == | != | <= | >= | < | > | is
    bin_ops = {Tokentype.OpPlus, Tokentype.OpMinus, Tokentype.OpMultiply,
               Tokentype.OpIntDivide, Tokentype.OpModulus, Tokentype.OpEq,
               Tokentype.OpNotEq, Tokentype.OpLtEq, Tokentype.OpGtEq,
               Tokentype.OpLt, Tokentype.OpGt, Tokentype.OpIs}
    def bin_op(self):
        print("bin_op()")
        if self.token.type in self.bin_ops:
            ... # "Surely now we will both drown", said the frog.
                # "lol", said the scorpion, "lmao".
            if self.token.type == Tokentype.OpLt:
                self.match(Tokentype.OpLt)
            return
        
    # # member expr ::= cexpr . ID
    # member expr ::= [[. ID]]+
    def member_expr(self):
        print("member_expr()")
        if self.token.type != Tokentype.Period():
            return
        self.match(Tokentype.Period)
        self.match(Tokentype.Identifier)
        self.member_expr()



    # # index expr ::= cexpr [ expr ]

    def index_expr(self):
        print("index_expr()")
        self.match(Tokentype.BracketL)
        if self.token.type == Tokentype.BracketL:
            self.index_expr()
        self.expr()
        self.match(Tokentype.BracketR)

    # target ::= ID
    # | member expr
    # | index expr
    def target(self):
        print("target()")
        if self.token.type == Tokentype.Identifier:
            self.match(Tokentype.Identifier)
            return

        elif self.token.type == Tokentype.Period:
            self.member_expr()
            return
        
        elif self.token.type == Tokentype.BracketL:
            self.index_expr()
            return


        # try:
        #     self.member_expr()
        # except:
        #     pass
        # else:
        #     return

        # try:
        #     self.index_expr()
        # except:
        #     raise(SyntaxErrorException)
        # else:
        #     return

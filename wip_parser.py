from lexer import Lexer, Tokentype, SyntaxErrorException
import astree


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
        node = self.program()
        print(node)
        self.match(Tokentype.EOI)
        return node

    # CHOCOPY FULL REFERENCE GRAMMAR

    # program ::= [[ var def | func def | class def ]]∗ stmt∗
    stmt_keywords = {Tokentype.KwIf, Tokentype.KwFor, Tokentype.KwWhile}

    def program(self, stmt_keywords=stmt_keywords):
        dec = []
        stmt = []
        while self.token.type in {Tokentype.KwDef, Tokentype.KwClass, Tokentype.Identifier}:
            if self.token.type == Tokentype.KwDef:
                dec.append(self.func_def())
            elif self.token.type == Tokentype.KwClass:
                dec.append(self.class_def())
            elif self.peek() == Tokentype.Colon:
                # implement peek()
                # The colon is used for type annotations after the identifier. i.e "var : int = ..."
                dec.append(self.var_def())
        while self.token.type in stmt_keywords:
            stmt.append(self.stmt())
        return astree.ProgramNode(dec, stmt)

    # class def ::= class ID ( ID ) : NEWLINE INDENT class body DEDENT
    def class_def(self):
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
    # | [[var def | func def ]]+
    def class_body(self):
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
        self.match(Tokentype.KwDef)
        self.match(Tokentype.Identifier)

        self.match(Tokentype.ParenthesisL)
        if self.peek != Tokentype.ParenthesisR:
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
        while self.token.type in {Tokentype.KwGlobal, Tokentype.KwNonLocal, Tokentype.Identifier, Tokentype.KwDef}:
            if self.token.type == Tokentype.KwGlobal:
                self.global_decl()
            elif self.token.type == Tokentype.KwNonLocal:
                self.nonlocal_decl()
            elif self.token.type == Tokentype.Identifier:
                self.var_def()
            elif self.token.type == Tokentype.KwDef:
                self.func_def()

        if self.token.type not in stmt_keywords:
            raise(SyntaxErrorException)
        while self.token.type in stmt_keywords:
            self.stmt()
        
    # typed var ::= ID : type
    def typed_var(self):
        self.match(Tokentype.Identifier)
        self.match(Tokentype.Colon)
        self._type()
    
    # type ::= ID | IDSTRING | [ type ]
    def _type(self):
        # Need to define IDSTRING, and primitive types
        if self.token.type == Tokentype.Identifier:
            self.match(Tokentype.Identifier)
        elif self.token.type == Tokentype.StringLiteral:
            self.match(Tokentype.StringLiteral)
        else:
            raise(SyntaxErrorException)
        
    # global decl ::= global ID NEWLINE
    def global_decl(self):
        
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

cc : int = 12076463
c : str = "ciao"

class MyClass:
    def method(self: "MyClass"):
        pass

def wat(cx):
    while cx < 1:
        zx : str = "ciao\"eh"

if z == 0:
    x = 0
    if x == 0:
        z = 1
        d = 0
        z = 0
    c = 'h'

print(c)

import lexer

filename = 'test.py'
with open(filename) as f:
    lex = lexer.Lexer(f)
    token = lex.next()
    while token.type != lexer.Tokentype.EOI:
        print(token.type, token.lexeme if token.type != lexer.Tokentype.Newline else "\\n", token.location.line)
        token = lex.next()

n = 0
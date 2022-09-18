import wip_parser
import print_visitor
import ast

filename = 'test_expr.txt'

import lexer

with open(filename) as f:
    lex = lexer.Lexer(f)
    token = lex.next()
    while token.type != lexer.Tokentype.EOI:
        print(token.type, token.lexeme if token.type != lexer.Tokentype.Newline else "\\n", token.location.line)
        token = lex.next()

with open(filename) as f:
    p = wip_parser.Parser(f)
    a = p.parse()
    pv = print_visitor.PrintVisitor()
    pv.do_visit(a)

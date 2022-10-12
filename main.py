import parser
import print_visitor
import ast

filename = 'test/test03.cpy'

import lexer


with open(filename) as f:

    p = parser.Parser(f)
    a = p.parse()
    pv = print_visitor.PrintVisitor()
    pv.do_visit(a)

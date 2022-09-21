import parser
import print_visitor
import ast

filename = 'test_expr.txt'

import lexer



with open(filename) as f:
    p = parser.Parser(f)
    a = p.parse()
    pv = print_visitor.PrintVisitor()
    pv.do_visit(a)

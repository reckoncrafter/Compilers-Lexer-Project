import wip_parser
import print_visitor
filename = 'test.py'
with open(filename) as f:
    p = wip_parser.Parser(f)
    a = p.parse()
    pv = print_visitor.PrintVisitor()
    pv.do_visit(a)

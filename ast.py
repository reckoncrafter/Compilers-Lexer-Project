from enum import Enum


class Operator(Enum):
    Plus = 0
    Minus = 1
    Gt = 2
    Lt = 3
    Eq = 4
    GtEq = 5
    LtEq = 6
    Multiply = 7
    Divide = 8
    Modulus = 9


class Node:
    pass


class ExprNode:
    pass


class StmtNode:
    pass


class OpNode(ExprNode):
    pass


class BinaryOpNode(ExprNode):

    def __init__(self, op, lhs, rhs):
        self.op = op
        self.lhs = lhs
        self.rhs = rhs


class UnaryOpNode(ExprNode):

    def __init__(self, op, child):
        self.op = op
        self.child = child


class Literal(ExprNode):
    pass


class IntegerLiteral(Literal):
    def __init__(self, value):
        self.value = value

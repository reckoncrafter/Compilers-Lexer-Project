#
# Symbol table construction visitor. Version 1.0
# Yours to implement.
#
# I've suggested some member variables to use in the constructor, but you are free to implement
# it differently, as long as the interface does not change.
#

import functools
import astree as ast
import visitor
import symbol_table
from symbol_table import Symbol
import semantic_error


class SymbolTableVisitor(visitor.Visitor):

    def __init__(self):
        # Built-in functions and their return types.
        self.built_ins = {'print': '<None>', 'len': "int", 'input': 'str', 'object': 'object'}
        self.root_sym_table = None
        self.curr_sym_table = None
        ...  # add more member variables as needed.
        pass

    def do_visit(self, node):
        if node:
            return self.visit(node)

    @functools.singledispatchmethod
    def visit(self, node):
        print("Visitor support missing for", type(node))
        exit()

    @visit.register
    def _(self, node: ast.IdentifierNode):
        return node.name

    @visit.register
    def _(self, node: ast.NoneLiteralExprNode):
        return '<None>'

    @visit.register
    def _(self, node: ast.StringLiteralExprNode):
        return node.value

    @visit.register
    def _(self, node: ast.IntegerLiteralExprNode):
        return node.value

    @visit.register
    def _(self, node: ast.BooleanLiteralExprNode):
        return node.value

    @visit.register
    def _(self, node: ast.IdentifierExprNode):
        return self.do_visit(node.identifier)

    @visit.register
    def _(self, node: ast.BinaryOpExprNode):
        self.do_visit(node.lhs)
        self.do_visit(node.rhs)

    @visit.register
    def _(self, node: ast.UnaryOpExprNode):
        self.do_visit(node.operand)

    @visit.register
    def _(self, node: ast.IfExprNode):
        self.do_visit(node.condition)
        self.do_visit(node.then_expr)
        self.do_visit(node.else_expr)

    @visit.register
    def _(self, node: ast.IndexExprNode):
        self.do_visit(node.list_expr)
        self.do_visit(node.index)

    @visit.register
    def _(self, node: ast.MemberExprNode):
        self.do_visit(node.expr_object)
        self.do_visit(node.member)

    @visit.register
    def _(self, node: ast.FunctionCallExprNode):
        name = self.do_visit(node.identifier)
        sym_tab = self.curr_sym_table
        prev = sym_tab
        type_ = ''
        called_already = False

        # if the function was in the built ins, we check if it was already called in the current scope
        if name in self.built_ins:
            type_ = self.built_ins[name]
            for sym in sym_tab.get_symbols():
                if name == sym.get_name():
                    called_already = True
            # in case it hasn't been called we add it to the current symbol table
            if not called_already:
                self.curr_sym_table.add_symbol(Symbol(name, Symbol.Is.Global, type_))
        else:
            # we look for the function in the previous symbol tables to get the return type
            while sym_tab is not None:
                for sym in sym_tab.get_symbols():
                    if name == sym.get_name():
                        type_ = sym.get_type_str()
                        if sym_tab == self.curr_sym_table:
                            called_already = True
                        break
                sym_tab = sym_tab.get_parent()
            if not called_already:
                self.curr_sym_table.add_symbol(Symbol(name, 0, type_))

        for a in node.args:
            i = self.do_visit(a)
            type_ == ''
            sym_tab = self.curr_sym_table
            prev = sym_tab
            while sym_tab is not None:
                for sym in sym_tab.get_symbols():
                    if i == sym.get_name():
                        type_ = sym.get_type_str()
                prev = sym_tab
                sym_tab = sym_tab.get_parent()
            if self.curr_sym_table != prev:
                self.curr_sym_table.add_symbol(Symbol(i, Symbol.Is.ReadOnly, type_))

    @visit.register
    def _(self, node: ast.MethodCallExprNode):
        self.do_visit(node.member)
        for a in node.args:
            self.do_visit(a)

    @visit.register
    def _(self, node: ast.ListExprNode):
        for e in node.elements:
            self.do_visit(e)

    @visit.register
    def _(self, node: ast.PassStmtNode):
        pass

    @visit.register
    def _(self, node: ast.ReturnStmtNode):
        self.do_visit(node.expr)

    @visit.register
    def _(self, node: ast.AssignStmtNode):
        for t in node.targets:
            self.do_visit(t)
        self.do_visit(node.expr)

    @visit.register
    def _(self, node: ast.IfStmtNode):
        self.do_visit(node.condition)
        for s in node.then_body:
            self.do_visit(s)
        for e in node.elifs:
            self.do_visit(e[0])
            for s in e[1]:
                self.do_visit(s)
        for s in node.else_body:
            self.do_visit(s)

    @visit.register
    def _(self, node: ast.WhileStmtNode):
        self.do_visit(node.condition)
        for s in node.body:
            self.do_visit(s)

    @visit.register
    def _(self, node: ast.ForStmtNode):
        self.do_visit(node.identifier)
        self.do_visit(node.iterable)
        for s in node.body:
            self.do_visit(s)

    @visit.register
    def _(self, node: ast.ClassTypeAnnotationNode):
        return node.__str__()

    @visit.register
    def _(self, node: ast.ListTypeAnnotationNode):
        str_ = f"[{self.do_visit(node.elem_type)}]"
        return str_

    @visit.register
    def _(self, node: ast.TypedVarNode):
        name = self.do_visit(node.identifier)
        type_ = self.do_visit(node.id_type)
        return name, type_

    @visit.register
    def _(self, node: ast.VarDefNode):
        name, type_ = self.do_visit(node.var)
        parent = self.curr_sym_table
        value = self.do_visit(node.value)
        if parent.get_name() == 'top':
            self.curr_sym_table.add_symbol(Symbol(name, Symbol.Is.Global | Symbol.Is.Local, type_str=type_))
        else:
            self.curr_sym_table.add_symbol(Symbol(name, Symbol.Is.Local, type_str=type_))

    @visit.register
    def _(self, node: ast.GlobalDeclNode):
        name = self.do_visit(node.variable)
        # value = self.do_visit(node.value)

        type_ = ''
        for n in self.root_sym_table.get_symbols():
            if name == n.get_name():
                type_ = n.get_type_str()

        # self.curr_sym_table.add_symbol(Symbol(name, Symbol.Is.Global))
        self.curr_sym_table.add_symbol(Symbol(name, Symbol.Is.Global, type_str=type_))

    @visit.register
    def _(self, node: ast.NonLocalDeclNode):
        name = self.do_visit(node.variable)

        type_ = ''
        parent = self.curr_sym_table.get_parent()
        while parent != None:
            for n in parent.get_symbols():
                if name == n.get_name():
                    type_ = n.get_type_str()
                    break
            parent = parent.get_parent()

        # self.curr_sym_table.add_symbol(Symbol(name, 0))
        self.curr_sym_table.add_symbol(Symbol(name, 0, type_str=type_))

    @visit.register
    def _(self, node: ast.ClassDefNode):
        # adding superclass to symbol table
        # name = self.do_visit(node.super_class)
        # self.curr_sym_table.add_symbol(Symbol(name, Symbol.Is.Local, type_str=name))
        parent = self.curr_sym_table
        name = self.do_visit(node.name)
        self.curr_sym_table.add_symbol(Symbol(name, Symbol.Is.Global, type_str=name))

        name2 = self.do_visit(node.super_class)
        p_sym_tab = parent
        already_there = False
        while p_sym_tab is not None:
            for n in p_sym_tab.get_symbols():
                if name2 == n.get_name():
                    already_there = True
            p_sym_tab = p_sym_tab.get_parent()
        if not already_there:
            self.curr_sym_table.add_symbol(Symbol(name2, Symbol.Is.Global, type_str=name2))

        child = symbol_table.Class(name, name2)
        parent.add_child(child)
        self.curr_sym_table = child

        for d in node.declarations:
            self.do_visit(d)
        self.curr_sym_table = parent

    @visit.register
    def _(self, node: ast.FuncDefNode):
        # a function has a new symbol table, we add a child to te current one
        parent = self.curr_sym_table
        name = self.do_visit(node.name)
        bool_nested = True
        if node.return_type is not None:
            type_ = self.do_visit(node.return_type)
        else:
            type_ = '<None>'
        if parent.get_name() == 'top':
            self.curr_sym_table.add_symbol(Symbol(name, Symbol.Is.Global, type_str=type_))
            bool_nested = False
        else:
            self.curr_sym_table.add_symbol(Symbol(name, Symbol.Is.Local, type_str=type_))
            bool_nested = True
        child = symbol_table.Function(name, is_nested=bool_nested)
        parent.add_child(child)
        self.curr_sym_table = child
        self.do_visit(node.name)
        for p in node.params:
            name, ty = self.do_visit(p)
            self.curr_sym_table.add_symbol(Symbol(name, Symbol.Is.Parameter | Symbol.Is.Local, type_str=ty))
        self.do_visit(node.return_type)
        for d in node.declarations:
            self.do_visit(d)
        for s in node.statements:
            self.do_visit(s)

        self.curr_sym_table = parent

    @visit.register
    def _(self, node: ast.ProgramNode):
        self.root_sym_table = symbol_table.SymbolTable('top')
        self.curr_sym_table = self.root_sym_table
        for d in node.declarations:
            self.do_visit(d)
        for s in node.statements:
            self.do_visit(s)
        self.curr_sym_table = self.root_sym_table

    def get_symbol_table(self) -> symbol_table.SymbolTable:
        return self.root_sym_table

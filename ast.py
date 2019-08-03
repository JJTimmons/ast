import sys
from typing import Union

INT, MUL, DIV, ADD, MIN, EOF, LP, RP = (
    "INT",
    "MUL",
    "DIV",
    "ADD",
    "MIN",
    "EOF",
    "LP",
    "RP",
)


class Token:
    def __init__(self, type: str, value: Union[str, int]):
        self.type = type
        self.value = value

    def __str__(self):
        return f"Token({self.type},{self.value})"


class Lexer:
    def __init__(self, text: str):
        self.text = text.strip()

        self.curr_index = 0
        self.curr_token = self.text[self.curr_index]

    def more(self):
        return self.curr_index < len(self.text) - 1

    def inc(self):
        if self.more():
            self.curr_index += 1
            self.curr_token = self.text[self.curr_index]
        else:
            self.curr_index += 1
            self.curr_token = ""

    def skip_white(self):  # skip the white space between tokens
        while self.curr_token == " ":
            self.inc()

    def get_digit(self) -> Token:
        digit = ""
        while self.curr_token.isdigit():
            digit += self.curr_token
            self.inc()
        return Token(INT, int(digit))

    def get_next_token(self) -> Token:
        self.skip_white()

        if self.curr_token.isdigit():
            return self.get_digit()
        elif self.curr_token == "+":
            self.inc()
            return Token(ADD, "+")
        elif self.curr_token == "-":
            self.inc()
            return Token(MIN, "-")
        elif self.curr_token == "*":
            self.inc()
            return Token(MUL, "*")
        elif self.curr_token == "/":
            self.inc()
            return Token(DIV, "/")
        elif self.curr_token == "(":
            self.inc()
            return Token(LP, "(")
        elif self.curr_token == ")":
            self.inc()
            return Token(RP, ")")
        return Token(EOF, "")


class AST:
    pass


class Num(AST):
    def __init__(self, token: Token):
        assert token
        self.token = token
        self.value = token.value


class Unary(AST):  # +/-
    def __init__(self, op: Token, right: AST):
        assert op
        self.op = op
        self.right = right


class BinOp(AST):
    def __init__(self, left: AST, op: Token, right: AST):
        assert left and right
        self.left = left
        self.op = op
        self.right = right


class Parser:
    """Take a lexer and build an AST."""

    def __init__(self, lexer):
        self.lexer = lexer
        self.token = lexer.get_next_token()

    def eat(self, type: str):
        assert self.token.type == type
        self.token = self.lexer.get_next_token()

    def factor(self) -> AST:
        if self.token.type == INT:
            node = Num(self.token)
            self.eat(INT)
            return node

        if self.token.type in (ADD, MIN):
            token = self.token
            if self.token.type == ADD:
                self.eat(ADD)
            elif self.token.type == MIN:
                self.eat(MIN)
            return Unary(token, self.factor())

        if self.token.type == LP:
            self.eat(LP)
            node = self.expr()
            self.eat(RP)
            return node
        raise ValueError(self.token)

    def term(self) -> AST:
        node = self.factor()

        while self.token.type in (MUL, DIV):
            token = self.token
            if self.token.type == MUL:
                self.eat(MUL)
            elif self.token.type == DIV:
                self.eat(DIV)

            node = BinOp(node, token, self.factor())

        return node

    def expr(self) -> AST:
        """
            expr   : term ((ADD | MIN) term)*
            term   : factor ((MUL | DIV) factor)*
            factor : (ADD | MIN) factor | Num | LP expr RP
        """
        node = self.term()

        while self.token.type in (ADD, MIN):
            token = self.token
            if self.token.type == ADD:
                self.eat(ADD)
            elif self.token.type == MIN:
                self.eat(MIN)

            node = BinOp(node, token, self.term())

        return node

    def parse(self) -> AST:
        return self.expr()


class NodeVisitor:
    def __init__(self, parser: Parser):
        self.parser = parser

    def interpret(self):
        return self.visit(self.parser.parse())

    def visit(self, node: AST):
        visit_method_name = "visit_" + type(node).__name__
        visit_method = getattr(self, visit_method_name, self.generic_visit)
        return visit_method(node)

    def generic_visit(self, node: AST):
        raise NotImplementedError("visit_" + type(node).__name__)


class Interpreter(NodeVisitor):
    def visit_Num(self, node: Num):
        return node.value

    def visit_Unary(self, node: Unary):
        if node.op.type == MIN:
            return -(self.visit(node.right))
        if node.op.type == ADD:
            return +(self.visit(node.right))
        raise ValueError(node)

    def visit_BinOp(self, node: BinOp):
        if node.op.type == ADD:
            return self.visit(node.left) + self.visit(node.right)
        if node.op.type == MIN:
            return self.visit(node.left) - self.visit(node.right)
        if node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        if node.op.type == DIV:
            return self.visit(node.left) / self.visit(node.right)
        raise ValueError(node.left, node.op.type, node.right)


class LISPInterpreter(NodeVisitor):
    """Write output in LISP syntax.
    
    Ex: 7 + 3 * (9 * 9) -> (+ 7 (* 3 (* 9 9))
    """

    def visit_Num(self, node: Num):
        return node.value

    def visit_BinOp(self, node: BinOp):
        if node.op.type == ADD:
            return f"(+ {self.visit(node.left)} {self.visit(node.right)})"
        if node.op.type == MIN:
            return f"(- {self.visit(node.left)} {self.visit(node.right)})"
        if node.op.type == MUL:
            return f"(* {self.visit(node.left)} {self.visit(node.right)})"
        if node.op.type == DIV:
            return f"(/ {self.visit(node.left)} {self.visit(node.right)})"
        raise ValueError(node.left, node.op.type, node.right)


if __name__ == "__main__":
    test_input = "7 + (-3 * -9) * ---9"

    lexer = Lexer(test_input)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)

    print(interpreter.interpret())

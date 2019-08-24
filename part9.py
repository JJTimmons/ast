"""From part 9: https://ruslanspivak.com/lsbasi-part9/

GRAMMER:

    program : compound_statement DOT

    compound_statement : BEGIN statement_list END

    statement_list : statement
                    | statement SEMI statement_list

    statement : compound_statement
                | assignment_statement
                | empty

    assignment_statement : variable ASSIGN expr

    empty :

    expr: term ((PLUS | MINUS) term)*

    term: factor ((MUL | DIV) factor)*

    factor : PLUS factor
            | MINUS factor
            | INTEGER
            | LPAREN expr RPAREN
            | variable

    variable: ID

EXAMPLE:
    BEGIN
        BEGIN
            number := 2;
            a := number;
            b := 10 * a + 10 * number / 4;
            c := a - - b
        END;
        x := 11;
    END.
"""

from typing import Any, Dict, Union, List

INT, MUL, DIV, PLUS, MINUS, EOF, LP, RP, BEGIN, END, DOT, ASSIGN, SEMI, ID = (
    "INT",
    "MUL",
    "DIV",
    "PLUS",
    "MINUS",
    "EOF",
    "LP",
    "RP",
    "BEGIN",
    "END",
    "DOT",
    "ASSIGN",
    "SEMI",
    "ID",
)


class Token:
    """Pascal Token."""

    def __init__(self, token_type: str, value: Union[str, int]):
        self.type = token_type
        self.value = value

    def __str__(self):
        return f"Token({self.type},{self.value})"


SYMBOLS = {
    "*": Token(MUL, "*"),
    "/": Token(DIV, "/"),
    "+": Token(PLUS, "+"),
    "-": Token(MINUS, "-"),
    "(": Token(LP, "("),
    ")": Token(RP, ")"),
    "BEGIN": Token(BEGIN, "BEGIN"),
    "END": Token(END, "END"),
    ".": Token(DOT, "."),
    ":=": Token(ASSIGN, ":="),
    ";": Token(SEMI, ";"),
}


class Lexer:
    """Pascal Lexer."""

    def __init__(self, text: str):
        self.text = text.strip()

        self.curr_index = 0
        self.curr_token = self.text[self.curr_index]

    def _id(self) -> Token:
        result = ""
        while self.curr_token.isalnum() and self.more():
            result += self.curr_token
            self.inc()
        return SYMBOLS.get(result, Token(ID, result))

    def get_next_token(self) -> Token:
        self.skip_white()

        if self.curr_token.isdigit():
            return self.get_digit()
        if self.curr_token.isalnum():
            return self._id()
        if self.curr_token in SYMBOLS:
            token = SYMBOLS[self.curr_token]
            self.inc()
            return token
        if self.curr_token + self.peek() in SYMBOLS:
            token = SYMBOLS[self.curr_token + self.peek()]
            self.inc()
            self.inc()
            return token
        return Token(EOF, "")

    def peek(self):
        if self.more():
            return self.text[self.curr_index + 1]
        return ""

    def skip_white(self):  # skip the white space between tokens
        while self.curr_token.isspace():
            self.inc()

    def inc(self):
        if self.more():
            self.curr_index += 1
            self.curr_token = self.text[self.curr_index]
        else:
            self.curr_index += 1
            self.curr_token = ""

    def more(self):
        return self.curr_index < len(self.text) - 1

    def get_digit(self) -> Token:
        digit = ""
        while self.curr_token.isdigit():
            digit += self.curr_token
            self.inc()
        return Token(INT, int(digit))


class AST:
    pass


class NoOp(AST):
    pass


class Num(AST):
    def __init__(self, token: Token):
        assert token
        self.token = token
        self.value = token.value


class Var(AST):
    """The Var node is constructed out of ID token."""

    def __init__(self, token):
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


class Compound(AST):
    """Represents a 'BEGIN ... END' block"""

    def __init__(self):
        self.children = []


class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Parser:
    """Take a lexer and build an AST."""

    def __init__(self, lexer):
        self.lexer = lexer
        self.token = lexer.get_next_token()

    def eat(self, token_type: str):
        assert token_type == self.token.type, f"{token_type} {self.token.type}"
        self.token = self.lexer.get_next_token()

    def parse(self) -> AST:
        node = self.program()
        if self.token.type != EOF:
            raise ValueError(self.token)

        return node

    def program(self) -> AST:
        """program : compound_statement DOT"""
        node = self.compound_statement()
        self.eat(DOT)
        return node

    def compound_statement(self) -> AST:
        """compound_statement : BEGIN statement_list END"""
        self.eat(BEGIN)
        nodes = self.statement_list()
        self.eat(END)

        node = Compound()
        for n in nodes:
            node.children.append(n)
        return node

    def statement_list(self) -> List[AST]:
        """statement_list : statement
                          | statement SEMI statement_list
        """
        node = self.statement()
        results = [node]

        while self.token.type == SEMI:
            self.eat(SEMI)
            results.append(self.statement())

        if self.token.type == ID:
            raise RuntimeError(self.token)

        return results

    def statement(self) -> AST:
        """ statement : compound_statement
                        | assignment_statement
                        | empty
        """

        if self.token.type == BEGIN:
            return self.compound_statement()
        if self.token.type == ID:
            return self.assignment_statement()
        return self.empty()

    def assignment_statement(self) -> AST:
        """ assignment_statement : variable ASSIGN expr """

        left = self.variable()
        token = self.token
        self.eat(ASSIGN)
        right = self.expr()
        return Assign(left, token, right)

    def variable(self):
        """ variable : ID """

        node = Var(self.token)
        self.eat(ID)
        return node

    def empty(self):
        """An empty production"""
        return NoOp()

    def expr(self) -> AST:
        """
            expr   : term ((PLUS | MINUS) term)*
            term   : factor ((MUL | DIV) factor)*
            factor : (PLUS | MINUS) factor | Num | LP expr RP
        """
        node = self.term()

        while self.token.type in (PLUS, MINUS):
            token = self.token
            if self.token.type == PLUS:
                self.eat(PLUS)
            elif self.token.type == MINUS:
                self.eat(MINUS)

            node = BinOp(node, token, self.term())

        return node

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

    def factor(self) -> AST:
        """factor : PLUS factor
                    | MINUS factor
                    | INTEGER
                    | LPAREN expr RPAREN
                    | variable
        """

        if self.token.type == INT:
            node = Num(self.token)
            self.eat(INT)
            return node

        if self.token.type in (PLUS, MINUS):
            token = self.token
            if self.token.type == PLUS:
                self.eat(PLUS)
            elif self.token.type == MINUS:
                self.eat(MINUS)
            return Unary(token, self.factor())

        if self.token.type == LP:
            self.eat(LP)
            node = self.expr()
            self.eat(RP)
            return node

        return self.variable()


class NodeVisitor:
    def __init__(self, parser: Parser):
        self.parser = parser
        self.symbols: Dict[str, Any] = {}

    def interpret(self):
        self.visit(self.parser.parse())

        for key in self.symbols.keys():
            print(key, self.symbols[key])

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
        if node.op.type == MINUS:
            return -(self.visit(node.right))
        if node.op.type == PLUS:
            return +(self.visit(node.right))
        raise ValueError(node)

    def visit_BinOp(self, node: BinOp):
        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)
        if node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        if node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        if node.op.type == DIV:
            return self.visit(node.left) / self.visit(node.right)
        raise ValueError(node.left, node.op.type, node.right)

    def visit_Compound(self, node: Compound):
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node: Assign):
        self.symbols[node.left.value] = self.visit(node.right)

    def visit_Var(self, node: Var):
        var = node.value
        return self.symbols[var]

    def visit_NoOp(self, node: NoOp):
        pass


if __name__ == "__main__":
    test = """\
BEGIN
    BEGIN
        number := 2;
        a := number;
        b := 10 * a + 10 * number / 4;
        c := a - - b
    END;
    x := 11;
END.
"""

    lexer = Lexer(test)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)

    interpreter.interpret()

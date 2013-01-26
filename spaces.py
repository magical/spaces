"""

body = stmt*
stmt = assignment | ifstmt | whilestmt
assignment = name '=' expr '\n'
ifstmt = 'if' expr block
       | 'if' expr block 'else' block
       | 'if' expr block 'else' ifstmt
whilestmt = 'while' expr block
block = '{' stmt* '}'

expr = binexpr

binexpr = unexpr
        | binexpr binop unexpr
unexpr = value
       | unop value
value = name | number
      | '(' expr ')'

binop = boolop | cmpop | addop | mulop | bitop
boolop = '&&' | '||'
cmpop = '==' | '!=' | '<' | '>' | '<=' | '>='
addop = '+' | '-'
mulop = '*' | '/' | '%'
bitop = '^' | '&' | '|'

unop = '!' | '^'

# precedence:
#  boolop < cmpop < addop < mulop
#  boolop < cmpop < bitop

name = letter+ (' ' letter+)*

number = digit+

"""

punctuation = "!&|^=<>+-*/%~"

class SyntaxError(Exception): pass
class InvalidChar(SyntaxError): pass

class Token:
    def __init__(self, type, value=None):
        self.type = type
        self.value = value
    def __eq__(self, other):
        return self.type == other.type and self.value == other.value
    def __str__(self):
        if self.value is not None:
            return '%s: %s' % (self.type, self.value)
        return self.type

def tokenize(s):
    # I hate checking if we're past the end of the input all the time, so lets
    # pad the input with an invalid character
    s = s + '\0'
    i = 0
    tokens = []
    done = False
    while not done:
        token = None
        if s[i].isspace():
            # probably meaningless whitespace
            if s[i] == '\n':
                token = Token('newline')
            i += 1
        elif s[i].isdigit():
            # a number
            start = i
            while s[i].isdigit():
                i += 1
            if s[i].isalpha():
                raise SyntaxError("numbers and name must be separated by a space")
            token = Token('number', s[start:i])
        elif s[i].isalpha():
            start = end = i
            while s[i].isalpha():
                while s[i].isalpha():
                    i += 1
                end = i
                if s[i].isdigit():
                    raise SyntaxError("names cannot contain numbers")
                if s[start:end] in ('if', 'else', 'while'):
                    token = Token(s[start:end])
                    break
                if s[i] == ' ':
                    i += 1
            else:
                token = Token('name', s[start:end])
        elif s[i] in "(){}":
            token = Token(s[i])
            i += 1
        elif s[i] in punctuation:
            start = i
            while s[i] in punctuation:
                i += 1
            token = Token('op', s[start:i])
        elif s[i] == '\0' and i == len(s) - 1:
            token = Token('end')
            done = True
        else:
            raise InvalidChar(s[i])

        if token is not None:
            tokens.append(token)
    return tokens

def test_tokenize():
    END = Token('end')
    assert tokenize("1") == [Token('number', '1'), END]
    assert tokenize("a") == [Token('name', 'a'), END]
    assert tokenize("this is a test") == [Token('name', 'this is a test'), END]
    assert tokenize("this = a test") == [Token('name', 'this'), Token('op', '='), Token('name', 'a test'), END]
    assert tokenize("if") == [Token('if'), END]
    assert tokenize("else") == [Token('else'), END]
    assert tokenize("else if") == [Token('else'), Token('if'), END]
    assert tokenize("while") == [Token('while'), END]
    assert tokenize("if this == a test {\n is a test = 1 \n} else {\n is a test = 0 \n}") == [
        Token('if'),
        Token('name', 'this'),
        Token('op', '=='),
        Token('name', 'a test'),
        Token('{'),
        Token('newline'),
        Token('name', 'is a test'),
        Token('op', '='),
        Token('number', '1'),
        Token('newline'),
        Token('}'),
        Token('else'),
        Token('{'),
        Token('newline'),
        Token('name', 'is a test'),
        Token('op', '='),
        Token('number', '0'),
        Token('newline'),
        Token('}'),
        END,
    ]

_builtin_type = type

class Node:
    def __init__(self, type, *args):
        for x in args:
            if _builtin_type(x) is Token:
                raise TypeError(x)
        self.type = type
        self.args = args
    def __str__(self):
        def strarg(a):
            if type(a) is list:
                if len(a) == 1:
                    return str(a[0])
                return '[' + '; '.join(str(x) for x in a) + ']'
            return str(a)
        if len(self.args) > 1:
            return '%s:<%s>' % (self.type, ', '.join(map(strarg, self.args)))
        elif len(self.args) == 1:
            return '%s:%s' % (self.type, strarg(self.args[0]))
        return self.type

def parse(tokens):
    stmts = []
    i = 0
    while tokens[i].type != 'end':
        if tokens[i].type == 'newline':
            # empty statement
            i += 1
        else:
            stmt, i = parse_stmt(tokens, i)
            stmts.append(stmt)
    return stmts

def parse_expression(tokens):
    i = 0
    expr, i = parse_expr(tokens, i)
    if tokens[i].type != 'end':
        raise SyntaxError("unexpected %s" % tokens[i])
    return expr

def parse_stmt(tokens, i):
    t = tokens[i]
    i += 1
    if t.type == 'if':
        cond, i = parse_expr(tokens, i)
        then, i = parse_block(tokens, i)
        if tokens[i].type == 'else':
            i += 1
            if tokens[i].type == 'if':
                alt, i = parse_stmt(tokens, i)
            elif tokens[i].type == '{':
                alt, i = parse_block(tokens, i)
            else:
                raise SyntaxError('expected "{" or "if" after "else"')
            return Node('ifelse', cond, then, alt), i
        return Node('if', cond, then), i
    elif t.type == 'else':
        raise SyntaxError('unexpected "else"')
    elif t.type == 'while':
        cond, i = parse_expr(tokens, i)
        body, i = parse_block(tokens, i)
        return Node('while', cond, body), i
    elif t.type == 'name':
        # assignment
        name = Node('name', t.value)
        if tokens[i] != Token('op', '='):
            raise SyntaxError('expected "="')
        i += 1
        expr, i = parse_expr(tokens, i)
        if tokens[i].type == 'newline':
            i += 1
        return Node('assign', name, expr), i
    else:
        raise SyntaxError('unexpected token:', str(t))

def parse_block(tokens, i):
    stmts = []
    if tokens[i].type != '{':
        raise SyntaxError('expected "{"')
    i += 1
    while tokens[i].type != '}':
        if tokens[i].type == 'newline':
            # empty statement
            i += 1
        else:
            stmt, i = parse_stmt(tokens, i)
            stmts.append(stmt)
    if tokens[i].type != '}':
        raise SyntaxError('expected "}"')
    i += 1
    return stmts, i

def parse_expr(tokens, i, eat_newlines=False):
    return parse_binexpr(tokens, i, 1, eat_newlines)

_precedence = {}
for op in "! && ||".split():
    _precedence[op] = 1
for op in "== != < > <= >=".split():
    _precedence[op] = 2
for op in "+ -".split():
    _precedence[op] = 3
for op in "* / %".split():
    _precedence[op] = 4
for op in "^ & |".split():
    _precedence[op] = 3
del op

_binops = "&& || == != < > <= >= + - * / % ^ & |".split()
_unops = "! ^".split()

def parse_binexpr(tokens, i, lowprec, eat_newlines):
    # Pratt parser
    left, i = parse_unexpr(tokens, i, eat_newlines)
    while True:
        if tokens[i].type != 'op':
            break
        if tokens[i].value not in _binops:
            raise SyntaxError("unknown operator %s" % op)
        op = tokens[i].value
        prec = _precedence[op]
        if prec < lowprec:
            break
        i += 1
        right, i = parse_binexpr(tokens, i, prec+1, eat_newlines)
        left = Node('binexpr', op, left, right)
    return left, i

def parse_unexpr(tokens, i, eat_newlines):
    if eat_newlines:
        while tokens[i].type == 'newline':
            i += 1
    if tokens[i].type == 'op':
        op = tokens[i].value
        i += 1
        if op not in _unops:
            raise SyntaxError("unknown unary operator %s" % op)
        expr, i = parse_value(tokens, i)
        return Node('unexpr', op, expr), i
    expr, i = parse_value(tokens, i)
    return expr, i

def parse_value(tokens, i):
    t = tokens[i]
    i += 1

    if t.type == '(':
        expr, i = parse_expr(tokens, i, eat_newlines=True)
        if tokens[i].type != ')':
            raise SyntaxError('expected ")"')
        i += 1
        return expr, i
    elif t.type == 'name':
        return Node('name', t.value), i
    elif t.type == 'number':
        return Node('number', t.value), i
    raise SyntaxError('unexpected %s' % t)

def test_parse():
    def pexpr(s):
        return str(parse_expression(tokenize(s)))
    assert pexpr('1 + 1') == 'binexpr:<+, number:1, number:1>'
    assert pexpr('(1 + 1)') == 'binexpr:<+, number:1, number:1>'
    assert pexpr('a + b + c') == 'binexpr:<+, binexpr:<+, name:a, name:b>, name:c>'
    assert pexpr('a + b * c + d') == \
        'binexpr:<+, binexpr:<+, name:a, binexpr:<*, name:b, name:c>>, name:d>'
    assert pexpr('(!a || !b) == !(a && b)') == \
        'binexpr:<==, binexpr:<||, unexpr:<!, name:a>, unexpr:<!, name:b>>'\
                   ', unexpr:<!, binexpr:<&&, name:a, name:b>>>'
    assert pexpr('a <= b < c') == 'binexpr:<<, binexpr:<<=, name:a, name:b>, name:c>'
    assert pexpr('a > b >= c') == 'binexpr:<>=, binexpr:<>, name:a, name:b>, name:c>'

    def p(s):
        return list(map(str, parse(tokenize(s))))
    assert p('a = b') == ['assign:<name:a, name:b>']
    assert p('a = a + 1') == ['assign:<name:a, binexpr:<+, name:a, number:1>>']
    assert p('a = 1\nb = 2') == ['assign:<name:a, number:1>',
                                 'assign:<name:b, number:2>']
    assert p('if a == b {\n c = d \n}') == \
        ['if:<binexpr:<==, name:a, name:b>, assign:<name:c, name:d>>']
    assert p('if a {} else {}') == ['ifelse:<name:a, [], []>']
    assert p('if a {} else if b {}') == ['ifelse:<name:a, [], if:<name:b, []>>']
    assert p('if a {} else if b {} else {}') == \
        ['ifelse:<name:a, [], ifelse:<name:b, [], []>>']
    assert p('while a { a = 0 }') == ['while:<name:a, assign:<name:a, number:0>>']

test_tokenize()
test_parse()

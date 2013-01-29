#!/usr/bin/env python
"""spaces - an experimental mini-language which allows spaces in identifiers

Syntax
------

..

    program = stmt*

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

    name = LETTER+ ( ' ' LETTER+ )*

    number = DIGIT+

TODO
----

* indent/dedent instead of { }
* functions
* make the REPL understand multiline statements
* use whitespace to disambiguate operator precedence

"""

from __future__ import print_function
import operator

_punctuation = "!&|^=<>+-*/%~"

class SyntaxError(Exception): pass
class InvalidChar(SyntaxError):
    def __str__(self):
        return "invalid character: %s" % self.args[0]
class EvalError(Exception): pass
class RuntimeError(Exception): pass

try:
    input = raw_input
except NameError:
    pass

__metaclass__ = type

class Token:
    def __init__(self, type, value=None):
        self.type = type
        self.value = value
    def __eq__(self, other):
        return self.type == other.type and self.value == other.value
    def __ne__(self, other):
        return not self.__eq__(other)
    def __str__(self):
        if self.value is not None:
            return '%s:%s' % (self.type, self.value)
        return self.type
    def __repr__(self):
        return "Token(%r, %r)" % (self.type, self.value)

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
        elif s[i] in _punctuation:
            start = i
            while s[i] in _punctuation:
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
                stmt, i = parse_stmt(tokens, i)
                alt = [stmt]
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
        op = tokens[i].value
        if op not in _binops:
            raise SyntaxError("unknown operator %s" % op)
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
            if op in _binops:
                raise SyntaxError("missing lhs of binary operator %s" % op)
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
        n = int(t.value)
        return Node('number', n), i
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

def eval(body, env=None):
    if env is None:
        env = {}
    eval_body(body, env)
    return env

def eval_body(body, env):
    for node in body:
        eval_stmt(node, env)
    return

def eval_stmt(node, env):
    if node.type == 'assign':
        lhs, rhs = node.args
        if lhs.type != 'name':
            raise EvalError("non-name on lhs of assignment: %s", lhs)
        name = lhs.args[0]
        value = eval_expr(rhs, env)
        env[name] = value
    elif node.type == 'if':
        cond, then = node.args
        if eval_expr(cond, env):
            eval_body(then, env)
    elif node.type == 'ifelse':
        cond, then, alt = node.args
        if eval_expr(cond, env):
            eval_body(then, env)
        else:
            eval_body(alt, env)
    elif node.type == 'while':
        cond, body = node.args
        while eval_expr(cond, env):
            eval_body(body, env)
    else:
        raise EvalError("not a statement: %s" % node)
    return

_eval_ops = {
    '==': operator.eq,
    '!=': operator.ne,
    '<': operator.lt,
    '>': operator.gt,
    '<=': operator.le,
    '>=': operator.ge,

    '+': operator.add,
    '-': operator.sub,

    '*': operator.mul,
    '/': operator.floordiv,
    '%': operator.mod,

    '&': operator.and_,
    '|': operator.or_,
    '^': operator.xor,
}

def eval_expr(node, env):
    if node.type == 'name':
        name = node.args[0]
        try:
            return env[name]
        except KeyError:
            raise RuntimeError("no such variable: %s" % name)
    elif node.type == 'number':
        return node.args[0]
    elif node.type == 'binexpr':
        op, lexpr, rexpr = node.args
        if op == '&&':
            lval = eval_expr(lexpr, env)
            if lval:
                rval = eval_expr(rexpr, env)
                return rval
            else:
                return lval
        elif op == '||':
            lval = eval_expr(lexpr, env)
            if lval:
                return lval
            else:
                rval = eval_expr(rexpr, env)
                return rval
        elif op in _eval_ops:
            f = _eval_ops[op]
            lval = eval_expr(lexpr, env)
            rval = eval_expr(rexpr, env)
            return f(lval, rval)
        raise EvalError("unknown binary operator: %s" % op)
    elif node.type == 'unexpr':
        op, expr = node.args
        value = eval_expr(expr, env)
        if op == '!':
            return int(not value)
        elif op == '^':
            return ~value
        raise EvalError("unknown unary operator: %s" % op)
    raise EvalError("not an expression: %s" % node)

def test_eval():
    def e(_e, **env):
        return eval_expr(parse_expression(tokenize(_e)), env)
    assert e('1') == 1
    assert e('a', a=1) == 1
    assert e('2 + 2') == 4
    assert e('a + a', a=2) == 4
    assert e('!a', a=0) == 1
    assert e('!a', a=100) == 0
    assert e('1+2*3+4') == 11
    assert e('a && b', a=1, b=1) == 1
    assert e('a == 1', a=1) == 1
    assert e('a != 1', a=1) == 0
    assert e('a < b < c', a=4, b=5, c=100) == 1
    #assert e('a < b < c', a=4, b=0, c=100) == 0

    def s(_s, **env):
        return eval(parse(tokenize(_s)), env)
    assert s('a = 1')['a'] == 1
    assert s('a = 2', a=1)['a'] == 2
    assert s('a = a+1', a=1)['a'] == 2
    assert s('if a == b { c = 1 } else { c = 2 }', a=1, b=1)['c'] == 1
    assert s('if a == b { c = 1 } else { c = 2 }', a=1, b=0)['c'] == 2
    assert s('c = a == b', a=1, b=1)['c'] == 1
    assert s('while a > 0 { a = a - 1 }', a=100)['a'] == 0
    assert s('while a > 0 {'
                'if b > a { t = a\n a = b\n b = t }'
                'a = a - b'
             '}', a=65535, b=170)['b'] == 85

def repl():
    env = {}
    env['env'] = env
    while True:
        print("> ", end="")
        try:
            line = input()
            tokens = tokenize(line)
            try:
                val = eval_expr(parse_expression(tokens), env)
                print(val)
            except SyntaxError:
                eval(parse(tokens), env)
        except EOFError as e:
            print()
            break
        except (SyntaxError, EvalError, RuntimeError) as e:
            print(e)

if __name__ == '__main__':
    test_tokenize()
    test_parse()
    test_eval()
    repl()

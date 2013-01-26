"""

body = stmt*
stmt = assignment | ifstmt | whilestmt
assignment = name '=' expr
ifstmt = 'if' expr '{' stmt* '}' elsestmt?
elsestmt = 'else' ifstmt
         | 'else' '{' stmt* '}'
whilestmt = 'while' expr '{' stmt* '}'

expr = (name | number | op | '(' expr ')')*

op = parens | boolop | cmpop | mathop | bitop
boolop = '!' | '&&' | '||'
cmpop = '==' | '!=' | '<' | '>' | '<=' | '>='
mathop = '+' | '-' | '*' | '/' | '%'
bitop = '^' | '&' | '|'

name = letter+ (' ' letter+)*

number = digit+

"""

punctuation = "!&|^=<>+-*/%~"

class SyntaxError(Exception): pass
class InvalidChar(SyntaxError): pass

class Token:
    def __init__(self, type, *args):
        self.type = type
        self.args = args
    def __eq__(self, other):
        return self.type == other.type and self.args == other.args

def tokenize(s):
    # I hate checking if we're past the end of the input all the time, so lets
    # pad the input with an invalid character
    s = s + '\0'
    i = 0
    tokens = []
    while True:
        token = None
        if s[i].isspace():
            # meaningless whitespace
            i += 1
        elif s[i].isdigit():
            # a number
            start = i
            while s[i].isdigit():
                i += 1
            if s[i].isalpha():
                raise SyntaxError("numbers and name must be separated by a space")
            token = Token('number', int(s[start:i]))
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
            break
        else:
            raise InvalidChar(s[i])

        if token is not None:
            tokens.append(token)
    return tokens


def test_tokenize():
    assert tokenize("1") == [Token('number', 1)]
    assert tokenize("a") == [Token('name', 'a')]
    assert tokenize("this is a test") == [Token('name', 'this is a test')]
    assert tokenize("this = a test") == [Token('name', 'this'), Token('op', '='), Token('name', 'a test')]
    assert tokenize("if") == [Token('if')]
    assert tokenize("else") == [Token('else')]
    assert tokenize("else if") == [Token('else'), Token('if')]
    assert tokenize("while") == [Token('while')]
    assert tokenize("if this == a test { is a test = 1 } else { is a test = 0 }") == [
        Token('if'),
        Token('name', 'this'),
        Token('op', '=='),
        Token('name', 'a test'),
        Token('{'),
        Token('name', 'is a test'),
        Token('op', '='),
        Token('number', 1),
        Token('}'),
        Token('else'),
        Token('{'),
        Token('name', 'is a test'),
        Token('op', '='),
        Token('number', 0),
        Token('}'),
    ]

test_tokenize()

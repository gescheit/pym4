#!/usr/bin/env python

import sys
from string import Template
from collections import defaultdict, deque


class DlrTemplate(Template):
    delimiter = '$'
    idpattern = r'[0-9]+'


class ParseError(Exception):
    def __init__(self, message):
        self.message = message

    def __str__(self):
        return 'ParseError(%s)' % self.message


class Token(object):
    def __init__(self, name, value=None):
        self.type = name
        self.value = name if value is None else value

    def __eq__(self, other):
        if isinstance(other, Token):
            return self.type == other.type and self.value == other.value
        return False

    def __repr__(self):
        return "<Token: %r %r>" % (self.type, self.value)


class eof(str):
    def __repr__(self):
        return '<EOF>'


EOF = eof()


def endswith(l, e):
    return l[-len(e):] == e


def name(x):
    return x.__name__ if hasattr(x, '__name__') else x


class peek_insert_iter:
    def __init__(self, iter):
        self.iter = iter
        self.inserted = deque()
        self.peeked = deque()

    def __iter__(self):
        return self

    def __next__(self):
        if self.inserted:
            return self.inserted.popleft()
        if self.peeked:
            return self.peeked.popleft()
        return next(self.iter)

    next = __next__

    def insert(self, iterable):
        iterable = list(iterable)
        iterable.reverse()
        self.inserted.extendleft(iterable)

    def _peek(self):
        if not self.peeked:
            try:
                self.peeked.append(next(self.iter))
            except StopIteration:
                pass

    def peek(self):
        if self.inserted:
            return self.inserted[0]
        self._peek()
        if self.peeked:
            return self.peeked[0]
        return EOF


class Lexer:
    def __init__(self, text):
        self.text = text
        self.state = None
        self.chars = []
        self.nesting_level = 0
        self.start_quote = ['`']
        self.end_quote = ["'"]
        self.iter = None
        self.quote_buffer = deque(maxlen=len(self.start_quote[0]))

    def _finish_token(self, name):
        t = Token(name, ''.join(self.chars))
        self.chars = []
        return t

    def insert_text(self, text):
        self.iter.insert(text)

    def changequote(self, start_quote='`', end_quote='\''):
        self.start_quote = list(start_quote)
        self.end_quote = list(end_quote)
        if len(start_quote) != self.quote_buffer.maxlen:
            self.quote_buffer = deque(self.quote_buffer, maxlen=len(start_quote))

    def ifelse(self, string1, string2, equal, notequal=None):
        if string1 == string2 and equal:
            return equal
        elif string1 != string2 and notequal:
            return notequal

    def parse(self):
        '''
        Return an iterator that produces tokens. The iterator
        has one extra method: peek_char, that allows consumers
        to peek at the next character before it is lexed.
        '''
        lexer = self

        class peekthrough_iter:
            def __init__(self, iter):
                self.iter = iter

            def __iter__(self):
                return self.iter

            def __next__(self):
                return next(self.iter)
            next = __next__

            def peek_char(self):
                return lexer.iter.peek()
        self.iter = peek_insert_iter(iter(self.text))
        return peekthrough_iter(self._parse_internal())

    def _parse_internal(self):
        while True:
            c = self.iter.peek()
            # print 'CHAR: %s (state: %s)' % (repr(c), name(self.state))
            if self.state is not None:
                tokens = self.state(c)
            else:
                tokens = self._generic(c)
            for tok in tokens:
                yield tok
            if c is EOF and self.iter.peek() is EOF:
                break
        if self.chars:
            if self.state is None:
                for c in self.chars:
                    yield Token(c, c)
            else:
                raise ParseError('Error, unterminated %s' % name(self.state))

    def _generic(self, c):
        if c is not EOF:
            self.chars.append(next(self.iter))
        self.quote_buffer.append(c)
        if c.isalpha() or c == '_':
            self.state = self._identifier
        elif c == '#':
            self.state = self._comment

        d = len(self.start_quote) - len(self.chars)
        borrow = []
        if d > 0:
            for _ in range(d):
                b = next(self.iter)
                self.chars.append(b)
                borrow.append(b)
        if self.chars == self.start_quote:
            self.state = self._string
            self.nesting_level = 1

        if self.state is None:
            for _ in borrow:
                b = self.chars.pop()
                self.insert_text(b)
            tokens = [Token(c, c) for c in self.chars]
            self.chars = []
            return tokens
        return []

    def _string(self, c):
        self.chars.append(next(self.iter))
        if (
                self.start_quote != self.end_quote and
                endswith(self.chars, self.start_quote)
        ):
            self.nesting_level += 1
        elif endswith(self.chars, self.end_quote):
            self.nesting_level -= 1
            if self.nesting_level == 0:
                # strip start/end quote out of the token value
                self.chars = \
                    self.chars[len(self.start_quote):-len(self.end_quote)]
                self.state = None
                return [self._finish_token('STRING')]
        return []

    def _identifier(self, c):
        if not (c.isalnum() or c == '_'):
            self.state = None
            return [self._finish_token('IDENTIFIER')]

        self.chars.append(next(self.iter))
        return []

    def _comment(self, c):
        if c != '\n' and c is not EOF:
            self.chars.append(next(self.iter))
            return []

        self.state = None
        return [self._finish_token('COMMENT')]


def substmacro(name, body, args):
    kwargs = {str(x + 1): args[x] for x in range(len(args))}
    kwargs["0"] = name
    res = DlrTemplate(body).safe_substitute(kwargs)
    return res


class Parser:
    def __init__(self, text):
        self.macros = {
            'define': self._builtin_define,
            'dnl': self._builtin_dnl,
            'changequote': self._builtin_changequote,
            'divert': self._builtin_divert,
            'ifelse': self._builtin_ifelse,
            'ifdef': self._builtin_ifdef,
        }
        self.lexer = Lexer(text)
        self.token_iter = self.lexer.parse()
        self.diversions = defaultdict(list)
        self.current_diversion = 0

    def _builtin_define(self, args):
        if args:
            self.define(*args[:2])
        return None

    def _builtin_dnl(self, args):
        # Eat tokens till newline
        for char in self.lexer.iter:
            if char == '\n':
                break
        return None

    def _builtin_changequote(self, args):
        self.changequote(*args[:2])
        return None

    def _builtin_ifelse(self, args):
        return self.ifelse(*args)

    def _builtin_ifdef(self, args):
        if len(args) == 3:
            macro, string1, string2 = args
        elif len(args) == 2:
            macro, string1 = args
            string2 = None
        else:
            raise Exception()

        if macro in self.macros and string1:
            self.lexer.insert_text(string1)
        elif macro not in self.macros and string2:
            self.lexer.insert_text(string2)
        return None

    def _builtin_divert(self, args):
        args = args or [0]
        try:
            self.current_diversion = int(args[0])
        except ValueError:
            # GNU m4 prints a warning here:
            # m4:stdin:1: non-numeric argument to builtin `divert'
            return
        return None

    def _parse_args(self):
        args = []
        current_arg = []
        if self.token_iter.peek_char() == '(':
            # drop that token
            tok = next(self.token_iter)
            if tok.value != '(':
                raise ParseError('Expected open parenthesis but got %s'
                                 % tok.value)
            nesting_level = 1
            for tok in self._expand_tokens():
                if tok.value == '(':
                    nesting_level += 1
                elif tok.value == ',' or tok.value == ')':
                    args.append(''.join(current_arg))
                    current_arg = []
                elif current_arg or not tok.value.isspace():
                    current_arg.append(tok.value)
                if tok.value == ')':
                    nesting_level -= 1
                    if nesting_level == 0:
                        break
            # TODO: handle EOF without closing paren
        return args

    def _expand_tokens(self):
        for tok in self.token_iter:
            if (
                    isinstance(tok, Token)
                    and tok.type == 'IDENTIFIER'
                    and tok.value in self.macros
            ):
                args = self._parse_args()
                result = self.macros[tok.value](args)
                if result:
                    self.lexer.insert_text(result)
            else:
                yield tok

    def define(self, name, body=''):
        self.macros[name] = lambda x: substmacro(name, body, x)

    def changequote(self, start_quote='`', end_quote='\''):
        self.lexer.changequote(start_quote, end_quote)

    def ifelse(self, *args):
        if len(args) == 3:
            return self.lexer.ifelse(args[0], args[1], args[2])
        elif len(args) == 4:
            return self.lexer.ifelse(args[0], args[1], args[2], args[3])
        elif len(args) > 4:
            arg_chunks = [list(args[i:i + 3]) for i in range(0, len(args), 3)]
            if len(arg_chunks[-1]) == 1:
                arg_chunks[-2].append(arg_chunks[-1][0])
                arg_chunks.pop(-1)
            for arg_chunk in arg_chunks:
                res = self.lexer.ifelse(arg_chunk[0], arg_chunk[1], arg_chunk[2])
                if res:
                    return res

    def parse(self, stream=sys.stdout):
        for tok in self._expand_tokens():
            if self.current_diversion == 0:
                stream.write(tok.value)
            elif self.current_diversion > 0:
                self.diversions[self.current_diversion].append(tok.value)
        for diversion in sorted(self.diversions.keys()):
            if diversion < 1:
                continue
            stream.write(''.join(self.diversions[diversion]))
            self.diversions[diversion] = []


if __name__ == '__main__':
    Parser(sys.stdin.read()).parse()

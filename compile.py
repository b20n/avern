import re
import sys

## Parsing elements largely drawn from Norvig:
## http://norvig.com/lispy.html

BIFs = {
    '+': ('erlang', '+'),
    '-': ('erlang', '-'),
    '*': ('erlang', '*')
}

class Module:
    def __init__(self):
        self.name = None
        self.exports = []
        self.context = set()
        self.funs = []

def translate_symbol(symbol):
    return '_%s' % symbol

def is_valid_symbol(token):
    return re.match(r'^[a-z][a-z0-9]+', token)

def is_integer(token):
    return re.match(r'[0-9]+', token)

def tokenize(f):
    with open(f) as raw:
        program = ''.join(raw.readlines())
    program = program.strip()
    program = program.replace('\n', ' ')
    program = re.sub(r'\s+', ' ', program)
    return program.replace('(',' ( ').replace(')',' ) ').split()

def ns(tokens):
    name = tokens.pop(0)
    if not is_valid_symbol(name):
        raise SyntaxError('invalid symbol: %s' % name)
    tokens.pop(0) # Trailing )
    return name

def fn(tokens, context):
    # Parse argument spec
    token = tokens.pop(0)
    if token != '(':
        raise SyntaxError('unexpected token: %s' % token)
    body = ['fun', '(']
    args = []
    token = tokens.pop(0)
    while token != ')':
        context.add(translate_symbol(token))
        args.append(translate_symbol(token))
        token = tokens.pop(0)
    body.append(', '.join(args))
    body.append(') ->')
    tokens, foo = read_form(tokens, context)
    body.extend(foo)
    tokens.pop(0) # Trailing )
    return tokens, len(args), ' '.join(body)

def read_quoted_form(tokens):
    token = tokens.pop(0)
    form = []
    if token == '(':
        conses = 1
        form.append('[')
        while tokens[0] != ')':
            tokens, inner = read_quoted_form(tokens)
            form.extend(inner)
            form.append('|[')
            conses += 1
        for i in range(conses):
            form.append(']')
        tokens.pop(0) # Trailing )
    elif is_integer(token):
        form.append(token)
    else:
        form.append('\'%s\'' % token)
    return tokens, form

def read_form(tokens, context):
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    form = []
    if token == '(':
        while tokens[0] != ')':
            tokens, inner = read_form(tokens, context)
            form.extend(inner)
        tokens.pop(0) # Trailing )
    elif token == ')':
        raise SyntaxError('unexpected )')
    elif token == 'quote':
        tokens, quoted = read_quoted_form(tokens)
        form.extend(quoted)
    elif token in BIFs:
        form.extend(['call', "'%s':'%s'" % BIFs[token], '('])
        while tokens[0] != ')':
            tokens, inner = read_form(tokens, context)
            form.extend(inner)
            form.append(',')
        form.pop() # Remove trailing comma
        form.append(')')
    elif translate_symbol(token) in context:
        form.append(translate_symbol(token))
    elif is_integer(token):
        form.append(token)
    else:
        raise SyntaxError('unexpected token: %s' % token)
    return tokens, form

def read_top_level(tokens, mod):
    token = tokens.pop(0)
    if token != '(':
        raise SyntaxError('unexpected token: %s' % token)
    token = tokens.pop(0)
    if token == 'ns':
        if mod.name:
            raise SyntaxError('multiple ns forms found')
        mod.name = ns(tokens)
    elif token == 'defn+':
        name = tokens.pop(0)
        if not is_valid_symbol(name):
            raise SyntaxError('invalid symbol: %s' % name)
        tokens, arity, body = fn(tokens, mod.context.copy())
        mod.exports.append((name, arity))
        mod.context.add(name)
        mod.funs.append("'%s'/%s = %s" % (name, arity, body))
    else:
        raise SyntaxError('unexpected token: %s' % token)
    return tokens, mod

def add_module_info(mod):
    mod.exports.extend([('module_info', 0), ('module_info', 1)])
    mod.funs.extend([
        ("'module_info'/0 = fun () -> "
         "call 'erlang':'get_module_info' ('%s')" % mod.name),
        ("'module_info'/1 = fun (_cor0) -> "
         "call 'erlang':'get_module_info' ('%s', _cor0)" % mod.name)
    ])
    return mod

def compile(mod):
    output = []
    exports = ', '.join(["'%s'/%s" % (n, a) for n, a in mod.exports])
    output.append(' '.join([
        "module",
        "'%s'" % mod.name,
        "[%s]" % exports
    ]))
    output.append('attributes []')
    output.extend(mod.funs)
    output.append('end')
    return '\n'.join(output)

def main(f):
    mod = Module()
    tokens = tokenize(f)
    while tokens:
        tokens, mod = read_top_level(tokens, mod)
    if tokens:
        raise SyntaxError('leftover tokens: %s' % tokens)
    mod = add_module_info(mod)
    print compile(mod)

if __name__ == '__main__':
    main(sys.argv[1])

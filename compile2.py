import re
import sys

BIFs = {
    '+': ('erlang', '+'),
    '-': ('erlang', '-'),
    '*': ('erlang', '*')
}

class Module:
    def __init__(self):
        self.text = None
        self.tokens = None
        self.ast = []
        self.name = None
        self.exports = []
        self.funs = {}
        self.context = set()
        self.compiled = []

def add_module_info(mod):
    mod.exports.extend([('module_info', 0), ('module_info', 1)])
    mod.compiled.extend([
        ("'module_info'/0 = fun () -> "
         "call 'erlang':'get_module_info' ('%s')" % mod.name),
        ("'module_info'/1 = fun (_cor0) -> "
         "call 'erlang':'get_module_info' ('%s', _cor0)" % mod.name)
    ])

def translate_symbol(symbol):
    return '_%s' % symbol

def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

def compile_form(f, funs, vars):
    if type(f) is not list:
        if is_number(f):
            return [f]
        elif translate_symbol(f) in vars:
            return [translate_symbol(f)]
        else:
            raise NotImplementedError
    else:
        compiled = []
        call = f.pop(0)
        if call == 'let':
            arg = f.pop(0)
            bindings = zip(arg[::2], arg[1::2])
            for binding, form in bindings:
                vars.add(translate_symbol(binding))
                compiled.extend([
                    'let',  '<%s>' % translate_symbol(binding), '='
                ])
                compiled.extend(compile_form(form, funs, vars))
                compiled.append('in')
            for form in f:
                compiled.extend(compile_form(form, funs, vars))
        elif call == 'quote':
            pass
        else:
            if '/' in call:
                mod, fun = call.split('/')
                compiled.extend(['call', "'%s':'%s'" % (mod, fun), '('])
            elif call in BIFs:
                compiled.extend(['call', "'%s':'%s'" % BIFs[call], '('])
            elif call in funs:
                compiled.extend(['apply', funs[call], '('])
            else:
                raise SyntaxError('unknown call: %s' % call)
            for arg in f:
                compiled.extend(compile_form(arg, funs, vars))
                compiled.append(',')
            compiled.pop() # Remove trailing comma
            compiled.append(')')
        return compiled

def fn(args, body, funs, vars):
    compiled = ['fun', '(']
    for arg in args:
        symbol = translate_symbol(arg)
        vars.add(symbol)
        compiled.append(symbol)
        compiled.append(',')
    compiled.pop() # Remove trailing comma
    compiled.append(') ->')
    compiled.extend(compile_form(body, funs, vars))
    return compiled

def ns(form, mod):
    [_ns, name] = form
    if mod.name:
        raise SyntaxError('multiple ns forms')
    mod.name = name

def defn(form, mod):
    [declaration, name, args, body] = form
    if declaration == 'defn+':
        mod.exports.append((name, len(args)))
    compiled = ["'%s'/%s" % (name, len(args)), '=']
    mod.funs[name] = "'%s'/%s" % (name, len(args))
    mod.context.add(name)
    compiled.extend(fn(args, body, mod.funs, mod.context))
    mod.compiled.append(' '.join(compiled))

def compile(mod):
    for form in mod.ast:
        if form[0] == 'ns':
            ns(form, mod)
        elif form[0].startswith('defn'):
            defn(form, mod)
    add_module_info(mod)

    exports = ', '.join(["'%s'/%s" % e for e in mod.exports])
    print "module '%s' [%s]" % (mod.name, exports)
    print "attributes []"
    for fun in mod.compiled:
        print fun
    print "end"

def read_form(tokens):
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    if token == '(':
        L = []
        while tokens[0] != ')':
            L.append(read_form(tokens))
        tokens.pop(0) # pop off ')'
        return L
    elif token == ')':
        raise SyntaxError('unexpected )')
    else:
        return token

def parse(mod):
    tokens = mod.tokens
    while tokens:
        mod.ast.append(read_form(tokens))

def tokenize(mod):
    program = mod.text.strip()
    program = program.replace('\n', ' ')
    program = re.sub(r'\s+', ' ', program)
    program = program.replace('(',' ( ')
    program = program.replace(')',' ) ')
    mod.tokens = program.split()

def main(f):
    mod = Module()
    with open(f) as raw:
        mod.text = ''.join(raw.readlines())
    tokenize(mod)
    parse(mod)
    compile(mod)

if __name__ == '__main__':
    main(sys.argv[1])

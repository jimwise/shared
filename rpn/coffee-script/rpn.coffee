stdin = process.openStdin()
stdout = process.stdout

stack = []
dict = {}

trim = (s) -> s.replace /\n$/g, ""

put = (s) -> stdout.write s + "\n"

keys = (d) -> k for k of d

make_op = (name, d, a, f) ->
        dict[name] =
                doc: d
                arity: a
                action: f

signal_error = (s) ->
        put s

make_op ".", "display top value on the stack", 1, (stack, x) ->
        put x
        [x]

make_op "#", "display number of values on the stack", 0, (stack) ->
        put stack.length
        []

make_op "+", "replace top two values on the stack with their sum", 2, (stack, x, y) -> [x + y]
make_op "-", "replace top two values on the stack with their difference", 2, (stack, x, y) -> [x - y]
make_op "*", "replace top two values on the stack with their product", 2, (stack, x, y) -> [x * y]
make_op "/", "replace top two values on the stack with their quotient", 2, (stack, x, y) -> [x / y]
make_op "^", "replace top two values on the stack, x and y, with x to the yth power", 2, (stack, x, y) -> [ Math.pow(x, y) ]

make_op "drop", "remove top value from the stack", 1, []
make_op "dup", "duplicate top value on the stack", 1, (stack, x) -> [ x, x ]
make_op "swap", "swap top two values on the stack", 2, (stack, x, y) -> [ y, x ]

make_op "help", "display this help", 0, (stack) ->
        put keys(dict).length + " Commands:"
        put key + " -- " + dict[key].doc for key in keys(dict).sort()
        []

do_op = (op) ->
        if stack.length < op.arity
                signal_error "Stack underflow"
        else
                args = []
                args.unshift stack.pop() for x in [1..op.arity] unless op.arity == 0
                res = op.action(stack, args...)
                stack = stack.concat res if res.length

act = (s) ->
        if s of dict
                do_op dict[s]
        else
                n = parseFloat(s)
                if isNaN n
                        signal_error("Unknown operation: " + s)
                else
                        stack.push(n)

stdout.write("> ")
stdin.on 'data', (input) ->
        s = trim(""+input)
        act s
        stdout.write("> ")

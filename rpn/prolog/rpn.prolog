%% stack ops

%% op(Name, Arity, Doc)
rpn_op('.', 1, 'display the top value on the stack').
rpn_op('+', 2, 'replace top two values on the stack with their sum').
rpn_op('-', 2, 'replace top two values on the stack with their difference').
rpn_op('*', 2, 'replace top two values on the stack with their product').
rpn_op('/', 2, 'replace top two values on the stack with their quotient').
rpn_op('^', 2, 'replace top two values on the stack, x and y, with x to the yth power').
rpn_op('drop', 1, 'remove the top value from the stack').
rpn_op('dup', 1, 'duplicate the top value on the stack').
rpn_op('swap', 2, 'swap the top two values on the stack').
rpn_op('help', 0, 'show this help').

op_arity(O, N) :- rpn_op(O, N, _).
op_doc(O, D) :- rpn_op(O, _, D).
op_names(Names) :- findall(N, rpn_op(N, _, _), Names).

act('.', [X|Stack], [X|Stack]) :- !, writeln(X).
act('+', [Y,X|Stack], [Z|Stack]) :- !, Z is X + Y.
act('-', [Y,X|Stack], [Z|Stack]) :- !, Z is X - Y.
act('*', [Y,X|Stack], [Z|Stack]) :- !, Z is X * Y.
act('/', [Y,X|Stack], [Z|Stack]) :- !, Z is X / Y.
act('^', [Y,X|Stack], [Z|Stack]) :- !, Z is X ** Y.
act('drop', [_|Stack], Stack) :- !.
act('dup', [X|Stack], [X,X|Stack]) :- !.
act('swap', [Y,X|Stack], [X,Y|Stack]) :- !.

act('help', X, X) :- 
	op_names(O),
	length(O, N),
	write(N),
	writeln(' Commands:'),
	op_help(O).

%% deal with list-ified commands (from readln), empty commands, stack underflow,
%% numerical values to push to stack, and unknown commands
act([X], S1, S2) :- !, act(X, S1, S2).
act([], S, S).
act(Op, S, S) :- bad_arity(Op, S), !.
act(N, Stack, [N|Stack]) :- number(N), !.
act(_, X, X) :- signal_error('unknown operation').

%% support for the help command
op_help([]).
op_help([O|Ops]) :-
	op_doc(O, H),
	write(O), write(' -- '), writeln(H),
	op_help(Ops).

%% check arity, and signal error if so (and succeed, for use in rule above)
bad_arity(Op, S) :- op_arity(Op, N), length(S, L), N > L, signal_error('stack underflow').

signal_error(S) :- atom_concat('*** ERROR: ', S, T), writeln(T).

repl1(Stack, NewStack) :- readln(S), !, act(S, Stack, NewStack).
repl(_, _) :- at_end_of_stream, !, writeln(''), halt.
repl(Stack, NewStack) :- !, repl1(Stack, TmpStack), repl(TmpStack, NewStack).
repl :- prompt(_, '> '), repl([], _).

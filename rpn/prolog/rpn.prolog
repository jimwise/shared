%% stack ops


op_names(X) :- X = ['.', '+', '-', '*', '/', '^', 'drop', 'dup', 'swap', 'quit', 'help'].

op_doc('.', 'display the top value on the stack').
op_doc('+', 'replace top two values on the stack with their sum').
op_doc('-', 'replace top two values on the stack with their difference').
op_doc('*', 'replace top two values on the stack with their product').
op_doc('/', 'replace top two values on the stack with their quotient').
op_doc('^', 'replace top two values on the stack, x and y, with x to the yth power').
op_doc('drop', 'remove the top value from the stack').
op_doc('dup', 'duplicate the top value on the stack').
op_doc('swap', 'swap the top two values on the stack').
op_doc('quit', 'quit the calculator').
op_doc('help', 'show this help').

arity('.', 1).
arity('+', 2).
arity('-', 2).
arity('*', 2).
arity('/', 2).
arity('^', 2).
arity('drop', 1).
arity('dup', 1).
arity('swap', 2).


act('.', [X|Stack], [X|Stack]) :- !, writeln(X).
act('+', [Y,X|Stack], [Z|Stack]) :- !, Z is X + Y.
act('-', [Y,X|Stack], [Z|Stack]) :- !, Z is X - Y.
act('*', [Y,X|Stack], [Z|Stack]) :- !, Z is X * Y.
act('/', [Y,X|Stack], [Z|Stack]) :- !, Z is X / Y.
act('^', [Y,X|Stack], [Z|Stack]) :- !, Z is X ** Y.
act('drop', [_|Stack], Stack) :- !.
act('dup', [X|Stack], [X,X|Stack]) :- !.
act('swap', [Y,X|Stack], [X,Y|Stack]) :- !.

%% no arity, please -- we're nullary
act('quit', _, _) :- halt.
act('help', X, X) :- 
	op_names(O),
	length(O, N),
	write(N),
	writeln(' Commands:'),
	op_help(O).
	
act([X], S1, S2) :- !, act(X, S1, S2).
act([], S, S).
act(Op, S, S) :- bad_arity(Op, S), !.
act(N, Stack, [N|Stack]) :- number(N), !.
act(_, X, X) :- signal_error('unknown operation').

op_help([]).
op_help([O|Ops]) :-
	op_doc(O, H),
	write(O), write(' -- '), writeln(H),
	op_help(Ops).

bad_arity(Op, S) :- arity(Op, N), length(S, L), N > L, signal_error('stack underflow').

signal_error(S) :- atom_concat('*** ERROR: ', S, T), writeln(T).

repl1(Stack, NewStack) :- readln(S), !, act(S, Stack, NewStack).
repl(Stack, NewStack) :- !, repl1(Stack, TmpStack), repl(TmpStack, NewStack).
repl :- repl([], _).

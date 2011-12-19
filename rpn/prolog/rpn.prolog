%% stack ops

bad_arity(Op, S) :- arity(Op, N), length(S, L), N > L, signal_error('stack underflow').

act([X], S1, S2) :- !, act(X, S1, S2).

act(Op, S, S) :- bad_arity(Op, S), !.
 
arity('.', 1).
act('.', [X|Stack], [X|Stack]) :- !, writeln(X).

arity('+', 2).
act('+', [Y,X|Stack], [Z|Stack]) :- !, Z is X + Y.

arity('-', 2).
act('-', [Y,X|Stack], [Z|Stack]) :- !, Z is X - Y.

arity('*', 2).
act('*', [Y,X|Stack], [Z|Stack]) :- !, Z is X * Y.

arity('/', 2).
act('/', [Y,X|Stack], [Z|Stack]) :- !, Z is X / Y.

arity('drop', 1).
act('drop', [X|Stack], Stack) :- !.

arity('dup', 1).
act('dup', [X|Stack], [X,X|Stack]) :- !.

arity('swap', 2).
act('swap', [Y,X|Stack], [X,Y|Stack]) :- !.

act(N, Stack, [N|Stack]) :- number(N), !.
act(_, X, X) :- signal_error('unknown operation').

signal_error(S) :- atom_concat('*** ERROR: ', S, T), writeln(T).

repl1(Stack, NewStack) :- readln(S), !, act(S, Stack, NewStack).
repl(Stack, NewStack) :- !, repl1(Stack, TmpStack), repl(TmpStack, NewStack).
repl :- repl([], NewStack).

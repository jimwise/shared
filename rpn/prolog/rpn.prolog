%% stack ops

bad_arity(Op, S) :- arity(Op, N), length(S, L), N > L, signal_error('stack underflow').

 
arity('.', 1).
arity('+', 2).
arity('-', 2).
arity('*', 2).
arity('/', 2).
arity('drop', 1).
arity('dup', 1).
arity('swap', 2).

act('.', [X|Stack], [X|Stack]) :- !, writeln(X).
act('+', [Y,X|Stack], [Z|Stack]) :- !, Z is X + Y.
act('-', [Y,X|Stack], [Z|Stack]) :- !, Z is X - Y.
act('*', [Y,X|Stack], [Z|Stack]) :- !, Z is X * Y.
act('/', [Y,X|Stack], [Z|Stack]) :- !, Z is X / Y.
act('drop', [_|Stack], Stack) :- !.
act('dup', [X|Stack], [X,X|Stack]) :- !.
act('swap', [Y,X|Stack], [X,Y|Stack]) :- !.

act('quit', _, _) :- halt.

act([X], S1, S2) :- !, act(X, S1, S2).
act([], S, S).
act(Op, S, S) :- bad_arity(Op, S), !.
act(N, Stack, [N|Stack]) :- number(N), !.
act(_, X, X) :- signal_error('unknown operation').

signal_error(S) :- atom_concat('*** ERROR: ', S, T), writeln(T).

repl1(Stack, NewStack) :- readln(S), !, act(S, Stack, NewStack).
repl(Stack, NewStack) :- !, repl1(Stack, TmpStack), repl(TmpStack, NewStack).
repl :- repl([], _).

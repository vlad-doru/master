% Ion Vlad-Doru
% Grupa 408(SD)
% FMI UNIBUC

% Functii ajutatoare.
in(X, [X | _]).
in(X, [_ | T ]) :- in(X, T).
notin(_, []).
notin(X, [H | T]) :- X\=H, notin(X, T).
remove(_, [], []).
remove(X, [X | T], L) :- remove(X, T, L).
remove(X, [H | T], [H | To]) :- X\=H, remove(X, T, To).

% Ex 1
replaceall(_, _, [], []).
replaceall(X, Y, [H | T1], [H | T2]) :- X\=H, replaceall(X, Y, T1, T2).
replaceall(X, Y, [X | T1], [Y | T2]) :- replaceall(X, Y, T1, T2).
replacefirst(_, _, [], []).
replacefirst(X, Y, [X | T1], [Y | T1]).
replacefirst(X, Y, [H | T1], [H | T2]) :- X\=H, replacefirst(X, Y, T1, T2).
% Ex 2
% Ex 3
minimum([M], M).
minimum([E1, E2 | T], M) :- E1 > E2, minimum([E2 | T], M).
minimum([E1, E2 | T], M) :- E2 > E1, minimum([E1 | T], M).

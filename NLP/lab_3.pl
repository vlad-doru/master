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
concat([], L, L).
concat([H | T], L2, [H | Lc] ) :- concat(T, L2, Lc).

% Ex 1
replaceall(_, _, [], []).
replaceall(X, Y, [H | T1], [H | T2]) :- X\=H, replaceall(X, Y, T1, T2).
replaceall(X, Y, [X | T1], [Y | T2]) :- replaceall(X, Y, T1, T2).
replacefirst(_, _, [], []).
replacefirst(X, Y, [X | T1], [Y | T1]).
replacefirst(X, Y, [H | T1], [H | T2]) :- X\=H, replacefirst(X, Y, T1, T2).
% Ex 2
reverse(L,R):-  accRev(L,[],R).
accRev([H|T],A,R):-  accRev(T,[H|A],R). 
accRev([],A,A).
% Ex 3
minimum([M], M).
minimum([E1, E2 | T], M) :- E1 > E2, minimum([E2 | T], M).
minimum([E1, E2 | T], M) :- E2 > E1, minimum([E1 | T], M).
% Ex 4
findbyindex(0, [H | _], H).
findbyindex(X, [_ | T], R) :- X > 0, S is X - 1, findbyindex(S, T, R).
% Ex 5
insertatindex(0, C, L, [C | L]).
insertatindex(X, C, [H | T], [H | R]) :- X > 0, S is X -1, insertatindex(S, C, T, R) .
% Ex 6
merge([], L, L).
merge(L, [], L).
merge([H1 | T1], [H2 | T2], [R | T]) :- H1 < H2, R is H1, merge(T1, [H2 | T2], T).
merge([H1 | T1], [H2 | T2], [R | T]) :- H1 >= H2, R is H2, merge([H1 | T1], T2, T).
% Ex 7
splitby(_, [], [], []).
splitby(X, [H | T], [S | L], R) :- X >= H, S is H, splitby(X, T, L, R).
splitby(X, [H | T], L, [S | R]) :- X < H, S is H, splitby(X, T, L, R).

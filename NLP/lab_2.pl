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

% Ex 1.
concat([], L, L).
concat([H | T], L2, [H | Lc] ) :- concat(T, L2, Lc).
% Ex 2.
checkset([]).
checkset([H | T]) :- notin(H, T) , checkset(T).
% Ex 3.
listtoset([], []).
listtoset([H | T], [H | T1]) :- remove(H, T, T2), listtoset(T2, T1).
% Ex 4.
  % Intersectia
intersection([], _, []).
intersection([H | T], L, [H | Ti]) :- in(H, L), intersection(T, L, Ti).
intersection([H | T], L, I) :- notin(H, L), intersection(T, L, I).
  % Reuniunea
join(X, Y, J) :- concat(X, Y, Concat), listtoset(Concat, J).
  % Diferenta
difference([], _, []).
difference([H | T], Y, [H | Td]) :- notin(H, Y), difference(T, Y, Td).
difference([H | T], Y, D) :- in(H, Y), difference(T, Y, D).
% Ex 5
pushfront(X, [], X).
pushfront(X, Y, Y) :- in(X, Y).
pushfront(X, Y, J) :- notin(X, Y), concat([X], Y, J).
% Ex 6
erase(X, Y, J) :- remove(X, Y, J).
% Ex 7
  % Factorial
fact(0, 1).
fact(X, Y) :-  X > 0, S is X - 1, fact(S, F), Y is X * F.
  % Fibonacci
fib(0, 0).
fib(1, 1).
fib(X, F) :- X > 1, T1 is X -1, T2 is X - 2, fib(T1, F1), fib(T2, F2), F is F1 + F2.
  % Cmmdc
cmmdc(X, X, Z) :- Z is X.
cmmdc(X, Y, Z) :- X > Y, D is X - Y, cmmdc(D, Y, Z).
cmmdc(X, Y, Z) :- X < Y, cmmdc(Y, X, Z).
% Ex 8.
len([], 0).
len([_ | T], X) :- length(T, Y), X is Y + 1.
% Ex 9.
sumpos([], 0).
sumpos([H | T], X) :- sumpos(T, Y), H > 0, X is Y + H.
sumpos([H | T], X) :- sumpos(T, X), H =< 0.

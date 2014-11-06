% Ion Vlad-Doru
% Problema 3

% Regula pentru convenienta.
parse(X, TP) :- parse(X, [s], [], TP).

% Vom folosi o stiva de numere

% Avem repl_red(sir_ramas, stiva, rezultat).
parse(S, Rezultat, Numere, TP) :- depl_red(S, [], Rezultat, Numere, [], TP).

% Dorim sa nu mai existe nimic si sa ajungem la stiva dorita.
depl_red([], Rezultat, Rezultat, _, [PT | _], PT).
depl_red(S, Stiva, Rezultat, Numere, SPT, PT) :-
  deplasare(Stiva, S, StivaNoua, S1),
  reducere(StivaNoua, StivaRedusa, Numere, NumereReduse, SPT, RPT),
  depl_red(S1, StivaRedusa, Rezultat, NumereReduse, RPT, PT).

deplasare(Stiva, [H | Y], [H | Stiva], Y).

% Nu reducem nimic
reducere(Stiva, Stiva, Numere, Numere, SPT, SPT).
% Reducem daca exista regula, apoi incercam sa aplicam o reducere succesiva.
reducere(Stiva, StivaRedusa, Numere, NumereReduse, SPT, PT) :-
  iregula(Stiva, Stiva1, Numere, Numere1, SPT, SPT1),
  reducere(Stiva1, StivaRedusa, Numere1, NumereReduse, SPT1, PT).
% NP si VP se acorda in verb in timp ce s nu are numar.
iregula([vp, np | X], [s | X], [Numar, Numar | Y], Y, [S, T | SPT], [[s, T, S] | SPT]).
iregula([np, vp | X], [vp | X], [_, Numar | Y], [Numar | Y], [S, T | SPT], [[vp, T, S] | SPT]).
iregula([v | X], [vp | X], [Numar | Y], [Numar | Y], [S | SPT], [[vp, S] | SPT]).
iregula([n | X], [np | X], [Numar | Y], [Numar | Y], [S | SPT], [[np, S] | SPT]).

iregula([Cuvant | X], [Cat | X], Y, [Numar | Y], SPT, [[Cat, [Cuvant]] | SPT]) :- cuvant(Cat, Numar, Cuvant).

cuvant(n, singular, publicul).
cuvant(n, plural, artistii).
cuvant(v, singular, iubeste).
cuvant(v, plural, iubesc).

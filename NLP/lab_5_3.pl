% Vlad-Doru Ion
% Sisteme distribuite

% Problema 3.

% Regula pentru parsarea mai usoara.
parse(X, Arbore) :- parse(X, [s], [], Arbore).

% Vom folosi o stiva de numere
parse(Sir, Rezultat, Numere, Arbore) :-
    depl_red(Sir, [], Rezultat, Numere, [], Arbore).
% depl_red(Sir, StivaInitiala, Rezultat, Numere, StivaNumere, Arbore).

% Dorim sa parsam complet sirul si sa ajungem la rezultatul dorit.
depl_red([], Rezultat, Rezultat, _, [Arbore], Arbore).
% Regula cand nu am parsat complet intreg sirul.
depl_red(Sir, Stiva, Rezultat, Numere, Arbore, ArboreFinal) :-
  deplasare(Stiva, Sir, StivaNoua, SirDeplasat),
  reducere(StivaNoua, StivaRedusa, Numere, NumereReduse, Arbore, ArboreInter),
  depl_red(SirDeplasat, StivaRedusa, Rezultat, NumereReduse, ArboreInter, ArboreFinal).

deplasare(Stiva, [Termen | SirDeplasat], [Termen | Stiva], SirDeplasat).

% Nu reducem nimic
reducere(Stiva, Stiva, Numere, Numere, Arbore, Arbore).
% Reducem daca exista regula, apoi incercam sa aplicam o reducere succesiva.
reducere(Stiva, StivaRedusa, Numere, NumereReduse, Arbore, ArboreFinal) :-
  iregula(Stiva, Stiva1, Numere, Numere1, Arbore, ArboreInter),
  reducere(Stiva1, StivaRedusa, Numere1, NumereReduse, ArboreInter, ArboreFinal).
% NP si VP se acorda in verb in timp ce s nu are numar.
iregula([vp, np | X], [s | X], [Numar, Numar | Y], [_ | Y], [S, T | SPT], [[s, T, S] | SPT]).
iregula([np, vp | X], [vp | X], [_, Numar | Y], [Numar | Y], [S, T | SPT], [[vp, T, S] | SPT]).
iregula([v | X], [vp | X], [Numar | Y], [Numar | Y], [S | SPT], [[vp, S] | SPT]).
iregula([n | X], [np | X], [Numar | Y], [Numar | Y], [S | SPT], [[np, S] | SPT]).

iregula([Cuvant | X], [Cat | X], Y, [Numar | Y], Arbore, [[Cat, [Cuvant]] | Arbore]) :-
    cuvant(Cat, Numar, Cuvant).

cuvant(n, singular, publicul).
cuvant(n, plural, artistii).
cuvant(v, singular, iubeste).
cuvant(v, plural, iubesc).

% Ion Vlad-Doru
% Problema 3

% Regula pentru convenienta.
parse(X) :- parse(X, [s], []).

% Vom folosi o stiva de numere

% Avem repl_red(sir_ramas, stiva, rezultat).
parse(S, Rezultat, Numere) :- depl_red(S, [], Rezultat, Numere).

% Dorim sa nu mai existe nimic si sa ajungem la stiva dorita.
depl_red([], Rezultat, Rezultat, _).
depl_red(S, Stiva, Rezultat, Numere) :- deplasare(Stiva, S, StivaNoua, S1),
                                        reducere(StivaNoua, StivaRedusa, Numere, NumereReduse),
                                        depl_red(S1, StivaRedusa, Rezultat, NumereReduse).

deplasare(Stiva, [H | Y], [H | Stiva], Y).

% Nu reducem nimic
reducere(Stiva, Stiva, Numere, Numere).
% Reducem daca exista regula, apoi incercam sa aplicam o reducere succesiva.
reducere(Stiva, StivaRedusa, Numere, NumereReduse) :- iregula(Stiva, Stiva1, Numere, Numere1),
                                                      reducere(Stiva1, StivaRedusa, Numere1, NumereReduse).
% NP si VP se acorda in verb in timp ce s nu are numar.
iregula([vp, np | X], [s | X], [Numar, Numar | Y], Y).
iregula([np, vp | X], [vp | X], [_, Numar | Y], [Numar | Y]).
iregula([v | X], [vp | X], [Numar | Y], [Numar | Y]).
iregula([n | X], [np | X], [Numar | Y], [Numar | Y]).

iregula([Cuvant | X], [Cat | X], Y, [Numar | Y]) :- cuvant(Cat, Numar, Cuvant).

cuvant(n, singular, publicul).
cuvant(n, plural, artistii).
cuvant(v, singular, iubeste).
cuvant(v, plural, iubesc).

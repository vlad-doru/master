% Vlad-Doru Ion
% Sisteme distribuite

% Problema 5.

parse(Sir, Arbore) :- parse(s, Sir, [], Arbore, _).

parse(SimbolFinal, [Cuvant | RestSir], SirFinal, Arbore, Numar) :-
    cuvant(Simbol, Numar, Cuvant),
    completeaza(Simbol, SimbolFinal, RestSir, SirFinal, [Simbol, [Cuvant]], Arbore, Numar).

% Arborele pe care l-am obtinut trebuie sa fie egal cu arborele final
completeaza(SimbolFinal, SimbolFinal, SirFinal, SirFinal, ArboreFinal, ArboreFinal, _).
% Arborele obtinut din stanga se compune cu arborele obtinut prin parsare
% top-down si se asigneaza regulei curente.
completeaza(Simbol, SimbolFinal, Sir, SirFinal, ArboreStanga, ArboreFinal, Numar) :-
    regula(Parinte, NumarParinte, [Numar | Numere], [Simbol | Rest]),
    trim(Rest, RestTrimmed),
    parse_lista(RestTrimmed, Sir, SirInter, ArboreInter, Numere),
    completeaza(Parinte, SimbolFinal, SirInter, SirFinal, [Parinte, [ArboreStanga | ArboreInter]], ArboreFinal, NumarParinte).

trim([], []).
trim([P | Y], Z) :- regula(P, _, [], []), trim(Y, Z).
trim([P | Y], [P | Z]) :- trim(Y, Z).

parse_lista([Simbol | Simboluri], Sir, SirFinal, [Arbore1 | Arbore2], [Numar | Numere]) :-
    parse(Simbol, Sir, SirInter, Arbore1, Numar),
    parse_lista(Simboluri, SirInter, SirFinal, Arbore2, Numere).
parse_lista([], SirFinal, SirFinal, [], _).

% Regulile gramaticii
regula(s, Numar, [Numar, Numar], [np, vp]).
regula(np, Numar, [Numar, Numar, _], [det, n, pp]).
regula(vp, Numar, [Numar, _], [v, np]).
regula(pp, Numar, [_, Numar], [p, np]).
regula(pp, _, [], []).
% Cuvinte
cuvant(det, _, the).
cuvant(det, plural, all).
cuvant(det, singular, every).
cuvant(p, _, near).
cuvant(conj, _, and).
cuvant(n, singular, dog).
cuvant(n, plural, dogs).
cuvant(n, singular, cat).
cuvant(n, plural, cats).
cuvant(n, singular, elephant).
cuvant(n, plural, elephants).
cuvant(v, plural, chase).
cuvant(v, singular, chases).
cuvant(v, plural, see).
cuvant(v, singular, sees).
cuvant(v, plural, amuse).
cuvant(v, singular, amuses).

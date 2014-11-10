% Vlad-Doru Ion
% Sisteme distribuite

% Problema 5.

parse(Sir, Arbore) :- parse(s, Sir, [], Arbore).

parse(SimbolFinal, [Cuvant | RestSir], SirFinal, Arbore) :-
    cuvant(Simbol, Cuvant),
    completeaza(Simbol, SimbolFinal, RestSir, SirFinal, [Simbol, [Cuvant]], Arbore).

% Arborele pe care l-am obtinut trebuie sa fie egal cu arborele final
completeaza(SimbolFinal, SimbolFinal, SirFinal, SirFinal, ArboreFinal, ArboreFinal).
% Arborele obtinut din stanga se compune cu arborele obtinut prin parsare
% top-down si se asigneaza regulei curente.
completeaza(Simbol, SimbolFinal, Sir, SirFinal, ArboreStanga, ArboreFinal) :-
    regula(Parinte, [Simbol | Rest]),
    parse_lista(Rest, Sir, SirInter, ArboreInter),
    completeaza(Parinte, SimbolFinal, SirInter, SirFinal, [Parinte, [ArboreStanga | ArboreInter]], ArboreFinal).

parse_lista([Simbol | Simboluri], Sir, SirFinal, [Arbore1 | Arbore2]) :-
    parse(Simbol, Sir, SirInter, Arbore1),
    parse_lista(Simboluri, SirInter, SirFinal, Arbore2).
parse_lista([], SirFinal, SirFinal, []).

% Regulile gramaticii
regula(s, [np, vp]).
regula(np, [det, n]).
regula(np, [np, conj, np]).
regula(vp, [v, np]).
regula(vp, [v, np, pp]).
regula(pp, [p, np]).
% Cuvinte
cuvant(det, the).
cuvant(det, all).
cuvant(det, every).
cuvant(p, near).
cuvant(conj, and).
cuvant(n, dog).
cuvant(n, dogs).
cuvant(n, cat).
cuvant(n, cats).
cuvant(n, elephant).
cuvant(n, elephants).
cuvant(v, chase).
cuvant(v, chases).
cuvant(v, see).
cuvant(v, sees).
cuvant(v, amuse).
cuvant(v, amuses).

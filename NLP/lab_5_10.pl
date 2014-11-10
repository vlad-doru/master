% Vlad-Doru Ion
% Sisteme distribuite

% Problema 10.

parse(Sir, Arbore) :- parse(s, Sir, [], Arbore).

parse(SimbolFinal, [Cuvant | RestSir], SirFinal, Arbore) :-
    cuvant(Simbol, Cuvant),
    legatura(Simbol, SimbolFinal),
    completeaza(Simbol, SimbolFinal, RestSir, SirFinal, [Simbol, [Cuvant]], Arbore).

parse(SimbolFinal, Sir, SirFinal, Arbore) :-
    regula(Simbol, []),
    legatura(Simbol, SimbolFinal),
    completeaza(Simbol, SimbolFinal, Sir, SirFinal, [Simbol, []], Arbore).

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
regula(det, []).
% Predicat generate links.
:-dynamic legatura/2.
generate_links(X) :-  assert(legatura(X, X)).
generate_links(X) :-
    regula(Y, [X | _]), X\=Y,
    generate_links(Y),
    legatura(Y, Z), \+legatura(X, Z),
    assert(legatura(X, Z)),
    print(X), print(' '), print(Z), nl.
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

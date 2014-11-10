% Vlad-Doru Ion
% Sisteme distribuite

% Problema 4.

parse(S) :- parse(s, S, []).

parse(Simbol, [Cuvant | RestSir], SirFinal) :-
    cuvant(Categorie, Cuvant),
    completeaza(Categorie, Simbol, RestSir, SirFinal).

% Regula care spune cand se opreste procesul de completare.
completeaza(SimbolFinal, SimbolFinal, SirFinal, SirFinal).

completeaza(Colt, SimbolFinal, Sir, SirFinal) :-
    regula(Parinte, [Colt | Simboluri]),
    parse_lista(Simboluri, Sir, SirInter),
    completeaza(Parinte, SimbolFinal, SirInter, SirFinal).

parse_lista([Simbol | Simboluri], Sir, SirFinal) :-
    parse(Simbol, Sir, SirInter),
    parse_lista(Simboluri, SirInter, SirFinal).
parse_lista([], SirFinal, SirFinal).

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

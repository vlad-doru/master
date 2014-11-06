% Ion Vlad-Doru
% Sisteme Distribuite

% Ex 4

parse(S) :- parse(s, S, []).

parse(C, [Cuvant | S2], S) :- cuvant(W, Cuvant),
                              completeaza(W, C, S2, S).

completeaza(C, C, S, S).
completeaza(W, C, S1, S) :- regula(P, [W | Rest]),
                            parse_lista(Rest, S1, S2),
                            completeaza(P, C, S2, S).

parse_lista([C|Cs], S1, S) :- parse(C, S1, S2),
                              parse_lista(Cs, S2, S).
parse_lista([], S, S).

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

% Ion Vlad-Doru
% Sisteme Distribuite

% Ex 6

parse(S, T) :- parse(s, S, [], T).

% Arborele care a fost obtinut pana acum in stanga e [W, Cuvant].
parse(C, [Cuvant | S2], S, T) :- cuvant(W, Cuvant),
                                 completeaza(W, C, S2, S, [W, [Cuvant]], T).

% Arborele pe care l-am obtinut trebuie sa fie egal cu arborele final
completeaza(C, C, S, S, T, T).
% Arborele obtinut din stanga se compune cu arborele obtinut prin parsare
% top-down si se asigneaza regulei curente.
completeaza(W, C, S1, S, TLeft, T) :- regula(P, [W | Rest]),
                            trim(Rest, RestTrimmed),
                            parse_lista(RestTrimmed, S1, S2, T1),
                            completeaza(P, C, S2, S, [P, [TLeft | T1]], T).
trim([], []).
trim([P | Y], Z) :- regula(P, []), trim(Y, Z).
trim([P | Y], [P | Z]) :- trim(Y, Z).

parse_lista([C|Cs], S1, S, [Y1 | Y2]) :- parse(C, S1, S2, Y1),
                              parse_lista(Cs, S2, S, Y2).
parse_lista([], S, S, []).

% Regulile gramaticii
regula(s, [np, vp]).
regula(np, [det, n, pp]).
regula(vp, [v, np]).
regula(pp, []).
% Cuvinte
cuvant(det, the).
cuvant(det, all).
cuvant(det, every).
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

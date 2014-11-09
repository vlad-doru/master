% Ion Vlad-Doru
% Sisteme Distribuite

% Ex 5

parse(S, T) :- parse(s, S, [], T, _).

% Arborele care a fost obtinut pana acum in stanga e [W, Cuvant].
parse(C, [Cuvant | S2], S, T, Numar) :-
                                 cuvant(W, Numar, Cuvant),
                                 completeaza(W, C, S2, S, [W, [Cuvant]], T, Numar).

% Arborele pe care l-am obtinut trebuie sa fie egal cu arborele final
completeaza(C, C, S, S, T, T, _).
% Arborele obtinut din stanga se compune cu arborele obtinut prin parsare
% top-down si se asigneaza regulei curente.
completeaza(W, C, S1, S, TLeft, T, Numar) :-
                            regula(P, NumarP, [Numar | Numere], [W | Rest]),
                            parse_lista(Rest, S1, S2, T1, Numere),
                            completeaza(P, C, S2, S, [P, [TLeft | T1]], T, NumarP).

parse_lista([C|Cs], S1, S, [Y1 | Y2], [Numar | Numere]) :- parse(C, S1, S2, Y1, Numar),
                                                          parse_lista(Cs, S2, S, Y2, Numere).
parse_lista([], S, S, [], []).

% Regulile gramaticii
regula(s, Numar, [Numar, Numar], [np, vp]).
regula(np, Numar, [Numar, Numar], [det, n]).
regula(np, Numar, [Numar, Numar, Numar], [np, conj, np]).
regula(vp, Numar, [Numar, _], [v, np]).
regula(vp, Numar, [Numar, _, _], [v, np, pp]).
regula(pp, Numar, [_, Numar], [p, np]).
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

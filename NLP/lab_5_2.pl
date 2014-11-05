% Vlad-Doru Ion grupa 331
% Sisteme distribuite

% Problema 2

% Vlad-Doru Ion grupa 331
% Sisteme distribuite

% Atunci cand parsam un constituent trebuie sa fim siguri ca numarul
% acestuia este cel corect.
parse(C, [Cuvant | S], S, [C, [Cuvant]]) :-  cuvant(C, Cuvant).
% Atunci cand aplicam o regula, fiecare din constituentii ce vor fi adaugati
% vor genera la randul lor un numar. Este important sa ii acordam pe fiecare dintre
% acesteia la randul lor in numar.
parse(C, S1, S, [C, Y]) :- regula(C, Cs), parse_lista(Cs, S1, S, Y).
parse_lista([C|Cs], S1, S, [Y1 | Y2]) :- parse(C, S1, S2, Y1),
                                         parse_lista(Cs, S2, S, Y2).
parse_lista([], S, S, []).

regula(s, [np, vp]).
regula(vp, [v, np]).
regula(np, [n]).
regula(vp, [v]).

cuvant(n, publicul).
cuvant(n, artistii).
cuvant(v, iubeste).
cuvant(v, iubesc).

parse(X) :- parse(s, X, [], Y), write(Y).

generate_all(L) :- findall(X, parse(s, X, []), L).

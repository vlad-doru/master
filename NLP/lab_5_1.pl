% Vlad-Doru Ion grupa 331
% Sisteme distribuite

% Problema 1.

% Atunci cand parsam un constituent trebuie sa fim siguri ca numarul
% acestuia este cel corect.
parse(C, Numar, [Cuvant | S], S, [C, [Cuvant]]) :-  cuvant(C, Numar, Cuvant).
% Atunci cand aplicam o regula, fiecare din constituentii ce vor fi adaugati
% vor genera la randul lor un numar. Este important sa ii acordam pe fiecare dintre
% acesteia la randul lor in numar.
parse(C, Numar, S1, S, [C, Y]) :- regula(C, Numar, Numere, Cs), parse_lista(Cs, Numere, S1, S, Y).
parse_lista([C|Cs], [Numar | N], S1, S, [Y1 | Y2]) :- parse(C, Numar, S1, S2, Y1), parse_lista(Cs, N, S2, S, Y2).
parse_lista([], [], S, S, []).

regula(s, _, [Numar, Numar], [np, vp]).
regula(vp, Numar, [Numar, _], [v, np]).
regula(np, Numar, [Numar], [n]).
regula(vp, Numar, [Numar], [v]).

cuvant(n, singular, publicul).
cuvant(n, plural, artistii).
cuvant(v, singular, iubeste).
cuvant(v, plural, iubesc).

parse(X, ParseTree) :- parse(s, _, X, [], ParseTree).

generate_all(L) :- findall(X, parse(s, _, X, []), L).

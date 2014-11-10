% Vlad-Doru Ion grupa 331
% Sisteme distribuite

% Problema 2.

% Regula pentru a putea parsa mai usor fraze si a genera arborele.
parse(X, Arbore) :- parse(s, X, [], Arbore).
% Cand trebuie sa parsam un consituent.
parse(Simbol, [Cuvant | S], S, [Simbol, [Cuvant]]) :-
    cuvant(Simbol, Cuvant).
% Atunci cand aplicam o regula, fiecare din constituentii ce vor fi adaugati
% vor genera la randul lor un numar.
parse(Simbol, Sir, SirFinal, [Simbol, Arbore]) :-
    regula(Simbol, Simboluri),
    parse_lista(Simboluri, Sir, SirFinal, Arbore).

parse_lista([Simbol | Simboluri], Sir, SirFinal, [Arbore1 | Arbore2]) :-
    parse(Simbol, Sir, SirIntermediar, Arbore1),
    parse_lista(Simboluri, SirIntermediar, SirFinal, Arbore2).
% Regula de oprire atunci cand parsam lista vida.
parse_lista([], S, S, []).

regula(s, [np, vp]).
regula(vp, [v, np]).
regula(np, [n]).
regula(vp, [v]).

cuvant(n, publicul).
cuvant(n, artistii).
cuvant(v, iubeste).
cuvant(v, iubesc).

generate_all(L) :- findall(X, parse(X, _), L).

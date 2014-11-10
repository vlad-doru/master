% Vlad-Doru Ion grupa 331
% Sisteme distribuite

% Problema 1.

% Regula pentru a putea parsa mai usor fraze si a genera arborele.
parse(X, Arbore) :- parse(s, _, X, [], Arbore).
% Atunci cand parsam un constituent trebuie sa fim siguri ca numarul
% acestuia este cel corect.
parse(Simbol, Numar, [Cuvant | S], S, [Simbol, [Cuvant]]) :-
    cuvant(Simbol, Numar, Cuvant).
% Atunci cand aplicam o regula, fiecare din constituentii ce vor fi adaugati
% vor genera la randul lor un numar. Este important sa ii acordam pe fiecare dintre
% acesteia la randul lor in numar.
parse(Simbol, Numar, Sir, SirFinal, [Simbol, Arbore]) :-
    regula(Simbol, Numar, Numere, Simboluri),
    parse_lista(Simboluri, Numere, Sir, SirFinal, Arbore).

parse_lista([Simbol | Simboluri], [Numar | Numere], Sir, SirFinal, [Arbore1 | Arbore2]) :-
    parse(Simbol, Numar, Sir, SirIntermediar, Arbore1),
    parse_lista(Simboluri, Numere, SirIntermediar, SirFinal, Arbore2).
% Regula de oprire atunci cand parsam lista vida.
parse_lista([], [], S, S, []).

regula(s, _, [Numar, Numar], [np, vp]).
regula(vp, Numar, [Numar, _], [v, np]).
regula(np, Numar, [Numar], [n]).
regula(vp, Numar, [Numar], [v]).

cuvant(n, singular, publicul).
cuvant(n, plural, artistii).
cuvant(v, singular, iubeste).
cuvant(v, plural, iubesc).

generate_all(L) :- findall(X, parse(X, _), L).

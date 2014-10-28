recunoaste(Categorie, S1, S2) :- regula(Categorie, Fii),
                                 imperecheaza(Fii, S1, S2).
% Prin definitie  o lista de categorii se imperecheaza cu
% un sir daca ambele sunt vide.
% Sau ambele constau dintr-un singur acelasi cuvant.
% Sau o portiune initiala a sirului poate fi recunoscuta
% ca reprezentand prima categorie din lista, iar restul
% listei de categorii se imperecheaza cu restul sirului.

% regula(s, [np, vp]).
% regula(v, [angajeaza]).

imperecheaza([], S, S).
imperecheaza([Cuvant], [Cuvant | S], S).
imperecheaza([Categorie | Categorii], S1, S3) :-
    recunoaste(Categorie, S1, S2), imperecheaza(Categorii, S2, S3).

% Cum se interogheaza?
% recunoaste(s, [Col, ang, surori], []).

% Parser
regula(v, angajeaza).
cuvant(v, angajeaza).
parse(c, [Cuvant | S], S) :- cuvant(C, Cuvant).
parse(C, S1, S) :- regula(C, Cs), parse_list(Cs, S1, S).
parse_lista([C | Cs], S1, S) :- parse(C, S1, S2),
    parse_list(Cs, S2, S).
parse_list([], S, S).
regula(s, [np, vp]).

% Nu trateaza regula recursive la stanga NP->Np conj Np

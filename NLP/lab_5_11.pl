% Vlad-Doru Ion
% Sisteme distribuite

% Problema 11

parse(Sir) :- parse(s, Sir, []).

parse(SimbolFinal, [Cuvant | SirRest], SirFinal) :-
    cuvant(Categorie, Cuvant),
    call(Categorie, SimbolFinal, SirRest, SirFinal).

% Regulile gramaticii + reguli oprire
s(s, Sir, Sir).

np(np, Sir, Sir).
% NP -> NP Conj NP
np(C, Sir, SirFinal) :-
    parse(conj, Sir, Sir1),
    parse(np, Sir1, Sir2),
    np(C, Sir2, SirFinal).
% SirFinal -> NP VP
np(C, Sir, SirFinal) :-
    parse(vp, Sir, Sir1),
    s(C, Sir1, SirFinal).

det(det, Sir, Sir).
% NP -> Det N
det(C, Sir, SirFinal) :-
    parse(n, Sir, Sir1),
    np(C, Sir1, SirFinal).

v(v, Sir, Sir).
% VP -> V NP
v(C, Sir, SirFinal) :-
    parse(np, Sir, Sir1),
    vp(C, Sir1, SirFinal).
% VP -> V NP PP
v(C, Sir, SirFinal) :-
    parse(np, Sir, Sir1),
    parse(pp, Sir1, Sir2),
    vp(C, Sir2, SirFinal).

p(p, Sir, Sir).
% PP -> P NP
p(C, Sir, SirFinal) :-
    parse(np, Sir, Sir1),
    pp(C, Sir1, SirFinal).

vp(vp, Sir, Sir).
n(n, Sir, Sir).
pp(pp, Sir, Sir).
conj(conj, Sir, Sir).

% Lexicon
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

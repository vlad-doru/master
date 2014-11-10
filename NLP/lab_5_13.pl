% Ion Vlad-Doru
% Sisteme distribuite

% Problema 13

parse(Sir, Arbore) :- parse(s, _, Sir, [], Arbore).

parse(SimbolFinal, Numar, [Cuvant | RestSir], SirFinal, Arbore) :-
    cuvant(Categorie, Numar, Cuvant),
    legatura(Categorie, SimbolFinal),
    call(Categorie, SimbolFinal, Numar, RestSir, SirFinal, [Categorie, [Cuvant]], Arbore).

% Regulile gramaticii + reguli oprire
s(s, _, SirFinal, SirFinal, ArboreFinal, ArboreFinal).

np(np, _, SirFinal, SirFinal, ArboreFinal, ArboreFinal).
% NP -> NP Conj NP
np(SimbolFinal, Numar, Sir, SirFInal, Arbore, ArboreFinal) :-
    parse(conj, Numar, Sir, Sir1, Arbore1),
    parse(np, Numar, Sir1, Sir2, Arbore2),
    np(SimbolFinal, plural, Sir2, SirFInal, [np, Arbore, Arbore1, Arbore2], ArboreFinal).
% SirFInal -> NP VP
np(SimbolFinal, Numar, Sir, SirFInal, Arbore, ArboreFinal) :-
    parse(vp, Numar, Sir, Sir1, Arbore1),
    s(SimbolFinal, Numar, Sir1, SirFInal, [s,  Arbore, Arbore1], ArboreFinal).

det(det, _, SirFinal, SirFinal, ArboreFinal, ArboreFinal).
% NP -> Det N
det(SimbolFinal, Numar, Sir, SirFInal, Arbore, ArboreFinal) :-
    parse(n, Numar, Sir, Sir1, Arbore1),
    np(SimbolFinal, Numar, Sir1, SirFInal, [np, Arbore, Arbore1], ArboreFinal).

v(v, _, SirFinal, SirFinal, ArboreFinal, ArboreFinal).
% VP -> V NP
v(SimbolFinal, Numar, Sir, SirFInal, Arbore, ArboreFinal) :-
    parse(np, _, Sir, Sir1, Arbore1),
    vp(SimbolFinal, Numar, Sir1, SirFInal, [vp, Arbore, Arbore1], ArboreFinal).
% VP -> V NP PP
v(SimbolFinal, Numar, Sir, SirFInal, Arbore, ArboreFinal) :-
    parse(np, _, Sir, Sir1, Arbore1),
    parse(pp, _, Sir1, Sir2, Arbore2),
    vp(SimbolFinal, Numar, Sir2, SirFInal, [vp, Arbore, Arbore1, Arbore2], ArboreFinal).

p(p, _, SirFinal, SirFinal, ArboreFinal, ArboreFinal).
% PP -> P NP
p(SimbolFinal, _, Sir, SirFInal, Arbore, ArboreFinal) :-
    parse(np, Numar, Sir, Sir1, Arbore1),
    pp(SimbolFinal, Numar, Sir1, SirFInal, [pp, Arbore, Arbore1], ArboreFinal).

vp(vp, _, SirFinal, SirFinal, ArboreFinal, ArboreFinal).
n(n, _, SirFinal, SirFinal, ArboreFinal, ArboreFinal).
pp(pp, _, SirFinal, SirFinal, ArboreFinal, ArboreFinal).
conj(conj, _, SirFinal, SirFinal, ArboreFinal, ArboreFinal).

% Tabela de legaturi.
legatura(np, s).
legatura(det, np).
legatura(det, s).
legatura(v, vp).
legatura(p, pp).
legatura(X, X).
% Lexicon
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

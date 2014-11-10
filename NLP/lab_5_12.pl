% Ion Vlad-Doru
% Sisteme distribuite

% Problema 11

parse(S, T) :- parse(s, _, S, [], T).

parse(C, Numar, [X | S1], S, T) :- cuvant(W, Numar, X),
                                   call(W, C, Numar, S1, S, [W, [X]], T).

% Regulile gramaticii + reguli oprire
s(s, _, X, X, T, T).

np(np, _, X, X, T, T).
% NP -> NP Conj NP
np(C, Numar, S1, S, Tup, T) :- parse(conj, Numar, S1, S2, T1), parse(np, Numar, S2, S3, T2), np(C, plural, S3, S, [np, Tup, T1, T2], T).
% S -> NP VP
np(C, Numar, S1, S, Tup, T) :- parse(vp, Numar, S1, S2, T1), s(C, Numar, S2, S, [s,  Tup, T1], T).

det(det, _, X, X, T, T).
% NP -> Det N
det(C, Numar, S1, S, Tup, T) :-
    parse(n, Numar, S1, S2, T1),
    np(C, Numar, S2, S, [np, Tup, T1], T).

v(v, _, X, X, T, T).
% VP -> V NP
v(C, Numar, S1, S, Tup, T) :- parse(np, _, S1, S2, T1), vp(C, Numar, S2, S, [vp, Tup, T1], T).
% VP -> V NP PP
v(C, Numar, S1, S, Tup, T) :- parse(np, _, S1, S2, T1), parse(pp, _, S2, S3, T2), vp(C, Numar, S3, S, [vp, Tup, T1, T2], T).

p(p, _, X, X, T, T).
% PP -> P NP
p(C, _, S1, S, Tup, T) :- parse(np, Numar, S1, S2, T1), pp(C, Numar, S2, S, [pp, Tup, T1], T).

vp(vp, _, X, X, T, T).
n(n, _, X, X, T, T).
pp(pp, _, X, X, T, T).
conj(conj, _, X, X, T, T).

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

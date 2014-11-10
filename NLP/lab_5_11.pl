% Ion Vlad-Doru
% Sisteme distribuite

% Problema 11

parse(S) :- parse(s, S, []).

parse(C, [X | S1], S) :- cuvant(W, X),
                   call(W, C, S1, S).

% Regulile gramaticii + reguli oprire
s(s, X, X).

np(np, X, X).
% NP -> NP Conj NP
np(C, S1, S) :- parse(conj, S1, S2), parse(np, S2, S3), np(C, S3, S).
% S -> NP VP
np(C, S1, S) :- parse(vp, S1, S2), s(C, S2, S).

det(det, X, X).
% NP -> Det N
det(C, S1, S) :- parse(n, S1, S2), np(C, S2, S).

v(v, X, X).
% VP -> V NP
v(C, S1, S) :- parse(np, S1, S2), vp(C, S2, S).
% VP -> V NP PP
v(C, S1, S) :- parse(np, S1, S2), parse(pp, S2, S3), vp(C, S3, S).

p(p, X, X).
% PP -> P NP
p(C, S1, S) :- parse(np, S1, S2), pp(C, S2, S).

vp(vp, X, X).
n(n, X, X).
pp(pp, X, X).
conj(conj, X, X).

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

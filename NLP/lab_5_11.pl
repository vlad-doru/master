% Ion Vlad-Doru
% Sisteme distribuite

% Problema 11

parse(S) :- parse(s, S, []).

parse(C, S1, S) :- cuvant(W, S1, S2),
                   call(W, C, S2, S).

% Regulile gramaticii
% S -> NP VP
np(C, S1, S) :- parse(vp, S1, S2), s(C, S2, S).
% NP -> Det N
det(C, S1, S) :- parse(n, S1, S2), np(C, S2, S).
% NP -> NP Conj NP
np(C, S1, S) :- parse(conj, S1, S2), parse(np, S2, S3), np(C, S3, S).
% VP -> V NP
v(C, S1, S) :- parse(np, S1, S2), vp(C, S2, S).
% VP -> V NP PP
v(C, S1, S) :- parse(np, S1, S2), parse(pp, S2, S3), vp(C, S3, S).
% PP -> P NP
p(C, S1, S) :- parse(np, S1, S2), pp(C, S2, S).

% Regulile de oprire.
s(s, X, X).
np(np, X, X).
vp(vp, X, X).
det(det, X, X).
n(n, X, X).
v(v, X, X).
pp(pp, X, X).
p(p, X, X).
conj(conj, X, X).

% Lexicon
cuvant(det, [the|X], X).
cuvant(det, [all|X], X).
cuvant(det, [every|X], X).
cuvant(p, [near|X], X).
cuvant(conj, [and|X], X).
cuvant(n, [dog|X], X).
cuvant(n, [dogs|X], X).
cuvant(n, [cat|X], X).
cuvant(n, [cats|X], X).
cuvant(n, [elephant|X], X).
cuvant(n, [elephants|X], X).
cuvant(v, [chase|X], X).
cuvant(v, [chases|X], X).
cuvant(v, [see|X], X).
cuvant(v, [sees|X], X).
cuvant(v, [amuse|X], X).
cuvant(v, [amuses|X], X).

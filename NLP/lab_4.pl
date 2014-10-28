% Ion Vlad-Doru
% Grupa 408 (SD)
% Problema celor N dame.

% Functii ajutatoare.
in(X, [X | _]).
in(X, [_ | T ]) :- in(X, T).
notin(_, []).
notin(X, [H | T]) :- X\=H, notin(X, T).
remove(X, [X | T], T).
remove(X, [H | T], [H | To]) :- X\=H, remove(X, T, To).
len([], 0).
len([_ | T], X) :- length(T, Y), X is Y + 1.

% Verificam daca o lista e [1..N]
is_range(N, N, [N]).
% Trebuie sa punem conditia ca elementul curent sa fie mai mic ca N.
is_range(N, H, [H | T]) :- H < N, S is H + 1, is_range(N, S, T).
% Genereaza rangeul cu o interfata mai familiara.
range(N, X) :- is_range(N, 1, X).

% Permuta elementele unei multimi.
permute([], []).
permute(L, [H | T]) :- in(H, L), remove(H, L, S), permute(S, T).
% O lista este permutare daca este reprezinta o permutare a listei [1..N]
is_permutation(N, X) :- range(N, S), permute(S, X).

% Verifica daca damele asezate in aceasta pozitie sunt in siguranta in ceea
% ce priveste diagonalele secundare.
secondary_diag_safe(_, [], _).
secondary_diag_safe(Poz, [H | T], Diags) :- Next is Poz + 1,    % Urmarim urmatoarea pozitie
                                            Diag is Poz + H,    % Codul diagonalei curente
                                            notin(Diag, Diags), % Trebuie sa nu mai existe alta regina pe acea diagonala
                                            secondary_diag_safe(Next, T, [Diag | Diags]).
% Analog pentru diagonalele principale.
primary_diag_safe(_, [], _).
primary_diag_safe(Poz, [H | T], Diags) :- Next is Poz + 1,
                                          Diag is Poz - H,
                                          notin(Diag, Diags),
                                          primary_diag_safe(Next, T, [Diag | Diags]).

queens_solution(N, X) :- is_permutation(N, X),
                         secondary_diag_safe(1, X, []),
                         primary_diag_safe(1, X, []).
list_queens_solutions(N, LIST) :- findall(Y, queens_solution(N, Y), LIST).
count_queens_solutions(N, X) :- list_queens_solutions(N, Solutions), len(Solutions, X).

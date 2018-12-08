maphead([], []).
maphead([[H|_]|T], [H|R]) :- maphead(T, R).

inc(N, R) :- R is N+1.

square(N, R) :- R is N*N.

app([], L2, L2).
app([H|T], L2, [H|TR]) :- app(T, L2, TR).

mymap(_, [], []).
mymap(F, [H|T], [HR|TR]) :- call(F, H, HR), mymap(F, T, TR).

take(0, _, []).
take(N, [H|T], [H|TR]) :- Q is N - 1, take(Q, T, TR).

member(X, [_|T]) :- member(X, T).
member(X, [X|_]).

union([H|T1], [H|T2], [H|TR]) :- union(T1, T2, TR).
union([], L2, L2).
union([H1|T1], [H2|T2], [H1, H2 | TR]) :- union(T1, T2, TR).
union(L1, [], L1).

evenlist([], []).
evenlist([H|T], [H|TR]) :- 0 is mod(H, 2), evenlist(T, TR).
evenlist([_|T], TR) :- evenlist(T, TR).

lreverse([], []).
lreverse(L, R) :- rhelper(L, [], R).
rhelper([H|T], A, R) :- rhelper(T, [H|A], R).
rhelper([], A, A).

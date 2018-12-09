inner([], _, 0).
inner([H1|T1], [H2|T2], R) :- inner(T1, T2, A), R is (H1*H2) + A.

outer([], _, []).
outer([H|T], L2, [HR|TR]) :- ohelper(H, L2, HR), outer(T, L2, TR).
ohelper(_, [], []).
ohelper(H, [H2|T], [HR|TR]) :- HR is H*H2, ohelper(H, T, TR).

scan([], []).
scan(L, R) :- scanhelp(0, L, R). 
scanhelp(N, [], [N|[]]).
scanhelp(N, [H|T], [N|TR]) :- M is N+H, scanhelp(M, T, TR).

not(A) :- \+A.

prime(1).
prime(N) :- primecheck(N, 2). 
primecheck(N, N). 
primecheck(N, K) :- K<N, not(0 is mod(N, K)), J is K+1, primecheck(N, J). 


selection_sort([], []).
selection_sort(L, [H|T]) :- minimum(L, H), remove(H, L, X), selection_sort(X, T). 

minimum([X], X). 
minimum([X, Y|Z], M) :- X<Y, minimum([X|Z], M). 
minimum([X, Y|Z], M) :- X>=Y, minimum([Y|Z], M). 

remove(X, [H|T], [H|TR]) :- remove(X, T, TR).
remove(X, [X|T], T). 

insertion_sort(L, R) :- ihelper(L, [], R).
ihelper([], A, A).
ihelper([H|T], A, R) :- insert(H, A, Q), ihelper(T, Q, R).
insert(X, [Y|T], [Y|TR]) :- X>Y, insert(X, T, TR).
insert(X, [Y|T], [X,Y|T]) :- X=<Y.
insert(X, [], [X]).

intersect(A, B) :- mymember(Z, A), mymember(Z, B).

mymember(H, [H|_]).
mymember(X, [_|T]) :- mymember(X, T).

memberGen([H|_], [H|_]).
memberGen([_|T], [_|T1]) :- memberGen(T, T1).

disjoint(A, B) :- not(intersect(A,B)).

diagonal([], []).
diagonal([[H|_]|M], [H|TR]) :- removeHeads(M, A), diagonal(A, TR).
removeHeads([], []).
removeHeads([[_|T]|M], [T|TR]) :- removeHeads(M, TR).

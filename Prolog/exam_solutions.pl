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

calc(num(A), A).
calc(add(A, B), R) :- calc(A, X), calc(B, Y), R is X+Y.
calc(subt(A, B), R) :- calc(A, X), calc(B, Y), R is X-Y.
calc(mul(A, B), R) :- calc(A, X), calc(B, Y), R is X*Y.
calc(div(A, B), R) :- calc(A, X), calc(B, Y), R is X/Y.

isdot(A, B) :- isdhelper(A, 1, B).
isdhelper(1, K, K).
isdhelper(A, K, R) :- M is K*A, L is A-1, J is K+1, isdhelper(L, J, Q), R is M+Q.

merge_sort([], []).
merge_sort([X], [X]).
merge_sort(L, Z) :- length(L, N), N>1, split(L, P, Q), merge_sort(P, X), merge_sort(Q, Y), merge(X, Y, Z).

split([], [], []).
split([A], [A], []).
split([A,B|T], [A|X], [B|Y]) :- split(T, X, Y).

merge([], L, L).
merge(L, [], L).
merge([H|T], [H2|T2], [H|T3]) :- H<H2, merge(T, [H2|T2], T3).
merge([H|T], [H2|T2], [H2|T3]) :- H>=H2, merge([H|T], T2, T3).

/* reverse list and all the nested sublists */
twist(X, X) :- atomic(X).
twist([H|T], R) :- twist(H, Q), twist(T, Z), append(Z, [Q], R).

transpose([[]|_], []).
transpose(M, [HR|TR]) :- transHelp(M, TL, HR), transpose(TL, TR).
transHelp([], [], []).
transHelp([[H|T]|R], [T|TL], [H|TR]) :- transHelp(R, TL, TR).

mylast([], []).
mylast([X|[]], X) :- !.
mylast([_|T], R) :- mylast(T, R).

init([H|[]], []).
init([H|T], [H|TR]) :- init(T, TR).

antitranspose([[]|_], []) :- !.
antitranspose(M, [X|Z]) :- atransHelp(M, J, A), reverse(J, X), antitranspose(A, Z).
atransHelp([], [], []).
atransHelp([H|T], [K|T1], [J|T2]) :- mylast(H, K), init(H, U), reverse(U, J), atransHelp(T, T1, T2).

mysqrt(N, R) :- sqrt_help(N, 0, N, R).
sqrt_help(N, L, H, M) :- M is div((L+H), 2), N is M*M.
sqrt_help(N, L, H, R) :- M is div((L+H), 2), K is M*M, N>K, Z is M+1, sqrt_help(N, Z, H, R).
sqrt_help(N, L, H, R) :- M is div((L+H), 2), K is M*M, N<K, Z is M-1, sqrt_help(N, L, Z, R).

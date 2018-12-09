male(bob).
male(sam).
female(lily).
female(jane).
female(mary).
female(kate).
female(brittney).

parent(john, alex).
parent(lily, alex).
parent(george, john).
parent(rick, george).
parent(george, jane).
parent(jane, kate).
parent(jane, brittney).
parent(jane, mary).

child(A, B) :- parent(B, A).
grandchild(A, B) :- parent(Z, A), parent(B, Z).
ancestor(A, B) :- parent(A, B).
ancestor(A, B) :- parent(A, Z), ancestor(Z, B).
descendant(A, B) :- child(A, B).
descendant(A, B) :- child(A, Z), descendant(Z, B).
sibling(A, B) :- parent(Z, A), parent(Z, B), not(A=B).
cousin(A, B) :- parent(Z, A), parent(Y, B), sibling(Z, Y).

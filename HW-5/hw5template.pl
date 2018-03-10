% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X, Y) :- parent(Y, X).


% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- parent(X, _), female(X).
isFather(X) :- parent(X, _), male(X).


% 3. Define a predicate `grandparent/2`.
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).


% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X, Y) :- parent(Z, X), parent(Z, Y).


% 5. Define two predicates `brother/2` and `sister/2`.
sister(X, Y) :- sibling(X, Y), female(X).
brother(X, Y) :- sibling(X, Y), male(X).




% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X, Y) :- sibling(X, Z), married(Y, Z).
siblingInLaw(X, Y) :- married(X, Z), sibling(Y, Z).


% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X, Y) :- child(Y, Z), sibling(X, Z), female(X).
aunt(X, Y) :- child(Y, Z), siblingInLaw(X, Z), female(X).

uncle(X, Y) :- child(Y, Z), sibling(X, Z), male(X).
uncle(X, Y) :- child(Y, Z), siblingInLaw(X, Z), male(X).





% 8. Define the predicate `cousin/2`.
cousin(X, Y) :- child(X, A), child(Y, B), sibling(A, B).



% 9. Define the predicate `ancestor/2`.
ancestor(X, Y) :- child(Y, X).
ancestor(X, Y) :- child(Y, Z), ancestor(X, Z).


% Extra credit: Define the predicate `related/2`.
related(X, Y) :- ancestor(X, Y).
related(X, Y) :- ancestor(Y, X).
related(X, Y) :- cousin(X, Y).
related(X, Y) :- cousin(Y, X).
related(X, Y) :- aunt(X, Y).
related(X, Y) :- aunt(Y, X).
related(X, Y) :- uncle(X, Y).
related(X, Y) :- uncle(Y, X).
related(X, Y) :- siblingInLaw(X, Y).
related(X, Y) :- siblingInLaw(Y, X).
related(X, Y) :- sibling(X, Y).
related(X, Y) :- sibling(Y, X).
related(X, Y) :- married(X, Y).
related(X, Y) :- married(Y, X).

%%
% Part 2. Language implementation
%%

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.


% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.



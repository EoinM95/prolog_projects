arc(N,M,Seed) :- M is N*Seed +1.
arc(N,M,Seed) :- M is N*Seed.

goal(N,Target) :- 0 is N mod Target.

search([Node|_],_,Target,Node) :- goal(Node,Target).
search([Node|FRest],Seed,Target,Found) :-
  findall(X,arc(Node,X,Seed),Children),
  add2frontier(Children,FRest,FNew),
  search(FNew,Seed,Target,Found).

/*add2frontier([],F,F).
add2frontier([N|Children],F,[N|FNew]) :-
  add2frontier(Children,F,FNew).*/

add2frontier(C,[],C).
add2frontier(C,[N|Rest],[N|FNew]) :-
  add2frontier(C,Rest,FNew).

/*
  Questions: https://www.scss.tcd.ie/Tim.Fernando/4AI/bS.pdf
  Breadth-first:
    1. x = 2x
  Depth-first:
    1. Finds 13 as it now goes, 1 -> [4,3], 4 -> [13,12,3], Finds 13
    2. x = x+1
    3. There'll always be only N solutions, where N is the arity of the tree/graph, in this case two.
        We should always go to the next deepest Node in the tree anyway though and
        the predicate arc/3 should give us that.
*/

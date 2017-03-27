% Last Modified: Mon Mar 14 09:08:42 2016 (vogel)
%
% It may be useful to increase the arity of some
% of the predicates here, aligning them with
% the arity of the test predicates, and non-terminal
% symbols in the actual grammar.  It doesn't hurt
% to have a few instances of the same essential
% function around, differing in arity, depending
% on use.  Of course, that is not the only solution.
% One may prefer a single instance, of highest
% arity, but with reduced arity needs met by
% using _ values for unnecessary argument positions.

:- unknown(_,trace).

doone(N) :- test(N,X),drawtree(X).
doone(N,t) :- trace,test(N,X),drawtree(X),notrace.
doone(N,nt) :- notrace,test(N,X),drawtree(X).

% these presuppose that there is a parse argument
pass([]).
pass([H|T]) :-
        test(H,P),%drawtree(P),
        write('successfully passed '),write(H),!,nl,
        pass(T).
pass([H|T]) :-
        write('unsuccessfully failed '),write(H),!,nl,
        pass(T).

fail([]).
fail([H|T]) :-
        test(H,P),!,%drawtree(P),
        write('unsuccessfully passed '),write(H),nl,
        fail(T).
fail([H|T]) :-
        write('successfully failed '),write(H),nl,
        fail(T).
drawtree(Tree):-
        d_tree(Tree, 0).

d_tree([],_).
d_tree(Atom,Indent) :-
	atomic(Atom),
	tab(Indent),
	write(Atom),nl.
d_tree([Q|UOTED],Indent) :-
	integer(Q),
	name(Word,[Q|UOTED]),!,
	tab(Indent),
	write(Word),nl.
d_tree([Mother|Daughters], Indent):-
        nonlist(Mother),!,
        tab(Indent),                            % .. leave a space ..
        write(Mother),                          % .. write mother node
        calcindent(Indent, NewIndent),          % .. calculate indent
        nl,d_daughters(Daughters, NewIndent).   % handle daughters

d_tree([Mother|Daughters], Indent):-
    d_tree(Mother,Indent),
    d_tree(Daughters,Indent).

nonlist(Item) :- functor(Item,X,Y),X \== '.'.

d_daughters([],_).
d_daughters([First|Rest], Indent):-             % .. otherwise ..
        nonvar(First),
        d_tree(First, Indent),          % .. handle first
        d_daughters(Rest, Indent).              % .. and handles rest

calcindent(N, N1):-
        N1 is N + 2.

complement_structured(Subcat,Complement,np(Complement,N,P,C,GI-GO),GI-GO) :-
	member(np(Complement,N,P,C,GI-GO),Subcat).
complement_structured(Subcat,Complement,Term,GI-GO) :-
	member(Element,Subcat),
	Element =.. [Category|Arguments],
	Category \== np,
	append([Category|Arguments,GI-GO],[Complement],Object),
	Term =.. Object.

tab(0).
tab(N) :-
        N > 0,
        M is N - 1,
        put_char(' '),
        tab(M).



% note -- built in in current sicstus
%append([],L,L).
%append([H|T],L,[H|L1]):-
%	append(T,L,L1).

% note -- built in in current sicstus
%member(X,[X|_]).
%member(X,[_|Y]) :-
%	member(X,Y).

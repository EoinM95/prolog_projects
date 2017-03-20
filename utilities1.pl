% Last Modified: Mon Jan 16 08:59:15 2017 (vogel)  
% Initial Prolog Utilities Lab

%-1)  Load the Prolog Debugger:
% a. Esc-X load-file
%     /users/Public/CSLL/4CSLL5/pltrace.el
% b. Esc-X pltrace-on
% c. trace.
% [note -- the first window one gets is less useless than what you
% get the second time you trace something, after reloading the file].

%0)  Within the Prolog buffer, verify that you understand the
%operation of the Universal Functor.
% a) What is the outcome of:
% ?- np([the,dog],sg) =.. X.
% ?- green(kermit) =.. X.
% b) What is the outcome of:
% X =.. [a, b, c].
% X =.. [1, 2, 3].

%1) Define a generalized append relation as a two place relation
%true of a list of lists, and the list that is the append of
%the list of lists.  E.g.:
%
%| ?- append([[a,b,c],[c,d,e],[f,g]],X).
%
%X = [a,b,c,c,d,e,f,g] ? ;

% Verify that the relation is fully reversible.

length([],0).
length([H|T],N) :-
        length(T,M),
        N is M + 1.

lesslength([],0,Limit) :-
        0 > Limit * 2,!,fail. 
lesslength([],0,Limit) :-
        0 =< Limit * 2. 
lesslength([H|T],N,Limit) :-
        L is Limit - 1,
        lesslength(T,M,L),
        N is M + 1,
        N =< L + 1.

ll([],N,N,L) :-
	N =< (L * 2).
ll([H|T],M,N,Limit) :-
	O is M + 1,
	ll(T,O,N,Limit).

append([],[]).
append([First|Rest],Total) :-
        nonvar(First),nonvar(Rest),
        append(Rest,MidTotal),
        append(First,MidTotal,Total).
append([First|Rest],Total) :-
        nonvar(Total),
        length(Total,M),
        ll([First|Rest],0,N,M),
        append(First,MidTotal,Total),
        append(Rest,MidTotal).

append([],L,L).
append([H|L1],L2,[H|L3]) :-
        append(L1,L2,L3).

%2) Define a series of mappend relations of increasing arity,
%such that the last argument of each is the append of the
%first N - 1 arguments, for each arity 2 < N < 7.
%

% Verify that the relation is fully reversible.

% N = 3
mappend([],L,L).
mappend([H|L1],L2,[H|L3]) :-
        mappend(L1,L2,L3).

% N = 4
mappend([],L2,L3,L4) :- 
        mappend(L2,L3,L4).
mappend([H|L1],L2,L3,[H|L4]) :-
        mappend(L1,L2,L3,L4).

% N = 5
mappend([],L2,L3,L4,L5) :- 
        mappend(L2,L3,L4,L5).
mappend([H|L1],L2,L3,L4,[H|L5]) :-
        mappend(L1,L2,L3,L4,L5).

% N = 6
mappend([],L2,L3,L4,L5,L6) :- 
        mappend(L2,L3,L4,L5,L6).
mappend([H|L1],L2,L3,L4,L5,[H|L6]) :-
        mappend(L1,L2,L3,L4,L5,L6).


%3) Define a member/3 relation which is true of an element of
%a list, the list containing it, and a list just like the containing
%list, except that it is missing an instance of the element.
%E.g.:	
%| ?- member(1,[2,1,3,1],X).
%
%X = [2,3,1] ? ;
%X = [2,1,3] ? ;
%no
        
member(E,[E|L],L).
member(E,[H|L],[H|L1]) :-
        member(E,L,L1).


%4) Define a relation insertarga/3 which is true of its three arguments
%just if the first and third arguments are terms that could suffice as
%Prolog goals, and where the third argument is just like the first, except
%that it has the second argument as its first argument (hence, the arity
%of the third argument is one greater than that of the first argument)

%yes
%| ?- insertarga(term,1,X).
%X = term(1) ? 
%yes
%| ?- insertarga(term(a,b),1,X).
%X = term(1,a,b) ? 

insertarga(InTerm,Arg,OutTerm) :-
        InTerm =.. [Functor | Arguments],
        OutTerm =.. [Functor, Arg | Arguments].


%5) Define a relation insertargz/3 which is true of its three arguments
%just if the first and third arguments are terms that could suffice as
%Prolog goals, and where the third argument is just like the first, except
%that it has the second argument as its final argument (hence, the arity
%of the third argument is one greater than that of the first argument)

%| ?- insertargz(term,1,X).
%X = term(1) ? 
%yes
%| ?- insertargz(term(a,b),1,X).
%X = term(a,b,1) ? 
%yes


insertargz(InTerm,Arg,OutTerm) :-
        InTerm =.. [Functor | Arguments],
        append([Functor | Arguments],[Arg],FAA),
        OutTerm =.. FAA.



diff([],TL,TL).
diff([H|RG],TL,Diff) :-
	member(H,TL,Diff1),
	diff(RG,Diff1,Diff).

% additional utilities

valence(Subcat,String,Type) :-
	member(Category,Subcat,Rest),
	type(Category,Type),
	insertargz(Category,String,Cat),
	Cat.

type(Term,Type) :-
	Term =.. [Functor | [Type | Args]].
type(Term,[]) :-
	Term =.. [Functor].


pass([]).
pass([H|T]) :-
        test(H),
        write('successfully passed '),write(H),!,nl,
        pass(T).
pass([H|T]) :-
        write('unsuccessfully failed '),write(H),!,nl,
        pass(T).

fail([]).
fail([H|T]) :-
        test(H),!,
        write('unsuccessfully passed '),write(H),nl,
        fail(T).
fail([H|T]) :-
        write('successfully failed '),write(H),nl,
        fail(T).





% cmv testsuite1
% Last Modified: Mon Feb  6 09:13:58 2017 (vogel) 
% starting point at end of the lecture
% A DCG with Case, Person, Speaker and Number Agreement
% (use utilities1.pl)



test(1,X) :- s(X,[the,man,sleeps,on,the,couch],[]).
test(2,X) :- s(X,[the,men,sleep,on,the,couch],[]).
test(3,X) :- s(X,[the,man,sleep,on,the,couches],[]).
test(4,X) :- s(X,[the,men,sleeps,on,the,couches],[]).
test(5,X) :- s(X,[the,man,sleeps],[]).
test(6,X) :- s(X,[she,gives,the, couch,to,i],[]).
test(7,X) :- s(X,[she,gives,the, couch,to,me],[]).
test(8,X) :- s(X,[her,gives,the, couch,to,me],[]).
test(9,X) :- s(X,[i,give,the, couch,to,her],[]).
test(10,X) :- s(X,[i,gives,the, couch,to,her],[]).
test(11,X) :- s(X,[i,sleeps],[]).
test(12,X) :- s(X,[i,sleep],[]).
test(13,X) :- s(X,[she,sleeps],[]).
test(14,X) :- s(X,[she,sleep],[]).
test(15,X) :- s(X,[her,sleeps],[]).


testem :-
     pass([1,2,5,7,9,12,13]),
     fail([3,4,6,8,10,11,14,15]).

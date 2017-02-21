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
%adverbs
test(16,X) :- s(X,[the,man,sleeps,peacefully,on,the,couch],[]).
test(17,X) :- s(X,[the,men,sleep,quietly,on,the,couch],[]).
test(18,X) :- s(X,[the,man,quietly,sleeps,on,the,couch],[]).
test(19,X) :- s(X,[the,men,sleep,on,the,couches,quietly],[]).
test(20,X) :- s(X,[the,man,sleeps,peacefully],[]).
test(20,X) :- s(X,[peacefully,the,man,sleeps],[]).
test(21,X) :- s(X,[the,man,sleeps,peacefully],[]).
test(22,X) :- s(X,[the,peacefully,man,sleeps],[]). %fail
%adjectives
test(23,X) :- s(X,[the,tall,man,sleeps],[]).
test(24,X) :- s(X,[the,quiet,man,sleeps],[]).
test(25,X) :- s(X,[the,man,quiet,sleeps],[]). %fail
test(26,X) :- s(X,[the,man,sleeps,quiet],[]). %fail
%relative clauses
test(27,X) :- s(X,[the,man,i,give,the,book,to,sleeps,quietly],[]).
test(28,X) :- s(X,[the,man,that,sleeps,quietly,gives,the,book,to,me],[]).
%tri-transitive
test(29,X) :- s(X,[the,man,swapped,me,an,apple,for,an,orange],[]).
%declarative
test(30,X) :- s(X,[dublin,is,the,capital,of,ireland],[]).
test(31,X) :- s(X,[this,is,an,apple],[]).
%questions
test(32,X) :- s(X,[is,this,an,apple],[]).
test(33,X) :- s(X,[did,you,give,the,book,to,her],[]).
test(34,X) :- s(X,[when,did,you,give,the,book,to,her],[]).
test(35,X) :- s(X,[how,did,you,give,the,book,to,her],[]).
test(36,X) :- s(X,[how,quickly,did,you,give,the,book,to,her],[]).
test(37,X) :- s(X,[who,swapped,the,apple,for,the,orange],[]).
%sentence embedding

%question embedding
testem :-
     pass([1,2,5,7,9,12,13,16,17,18,19,20,21,23,24,27,28,29,30,31,32,33,34,35,36,37]),
     fail([3,4,6,8,10,11,14,15,22,25,26]).

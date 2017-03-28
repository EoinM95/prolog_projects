% Last Modified: Mon Mar  6 10:19:05 2017 (vogel)

% Arity may require adjusting in the items below.
% The embedding predicates rquire addition, and
% classification in the testem predicate.

:- unknown(_,trace).
:- use_module(library(terms)).

% note placeholder arguments of S.
% note that X is shared with test, and test is used
% in the utilities.

%s(Type,X,B,G-G,[the,man,sleeps,on,the,couch],[]).
%vp(Vtype,VP,Per,Num,Form,nogap-nogap,[sleeps,on,the,couch],[]).
%np(NP,Per,Num,_,nogap-nogap,[cats],[]).
%nom(Y,X,[man],[]).
%s(Type,X,B,G-G,[val,asks,lee,to,believe,sal],[]).

test(1,X) :-
	s(Type,X,B,G-G,[the,man,sleeps,on,the,couch],[]).
test(a1,X) :-
	s(Type,X,B,G-G,[the,man,sleeps,on,the,ugly,couch],[]).
test(b1,X) :-
	s(decl,X,B,G-G,[the,couch,the,man,sleeps,on],[]).
test(c1,X) :-
	s(Type,X,B,G-G,[on,the,couch,the,man,sleeps],[]).
test(d1,X) :-
	s(Type,X,B,G-G,[the,ugly,couch,the,man,sleeps,on],[]).
test(e1,X) :-
	s(Type,X,B,G-G,[on,the,ugly,couch,the,man,sleeps],[]).
test(f1,X) :-
	s(Type,X,B,G-G,[couch,the,man,sleeps,on,the,ugly],[]). %no

test(2,X) :-
	s(Type,X,B,G-G,[the,men,sleep,on,the,couch],[]).
test(3,X) :-
	s(Type,X,B,G-G,[the,man,sleep,on,the,couches],[]).
test(4,X) :-
	s(Type,X,B,G-G,[the,men,sleeps,on,the,couches],[]).
test(5,X) :-
	s(Type,X,B,G-G,[the,man,sleeps],[]).
test(6,X) :-
	s(Type,X,B,G-G,[she,gives,the, couch,to,i],[]).
test(7,X) :-
	s(Type,X,B,G-G,[she,gives,the, couch,to,me],[]).
test(8,X) :-
	s(Type,X,B,G-G,[her,gives,the, couch,to,me],[]).
test(9,X) :-
	s(Type,X,B,G-G,[i,give,the, couch,to,her],[]).
test(10,X) :-
	s(Type,X,B,G-G,[i,gives,the, couch,to,her],[]).
test(11,X) :-
	s(Type,X,B,G-G,[i,sleeps],[]).
test(12,X) :-
	s(Type,X,B,G-G,[i,sleep],[]).
test(13,X) :-
	s(Type,X,B,G-G,[she,sleeps],[]).
test(14,X) :-
	s(Type,X,B,G-G,[she,sleep],[]).
test(15,X) :-
	s(Type,X,B,G-G,[her,sleeps],[]).
test(16,X) :-
	s(Type,X,B,G-G,[i,bet,you,a,couch,that,you,give,the,couch,to,me],[]).
test(17,X) :-
	s(Type,X,B,G-G,[i,bet,you,a,couch,that,you,give,the,couch,to,i],[]).
test(18,X) :-
	s(Type,X,B,G-G,[i,bet,you,a,couch,that,you,give,the,couch,to,him],[]).
test(19,X) :-
	s(Type,X,B,G-G,[i,bet,you,a,couch,that,i,give,the,couch,to,him],[]).
test(20,X) :-
	s(Type,X,B,G-G,[he,bets,you,a,couch,that,i,give,the,couch,to,him],[]).
test(21,X) :-
	s(Type,X,B,G-G,[we,bets,you,a,couch,that,i,give,the,couch,to,him],[]).

test(22,X):-
	s(Type,X,B,G-G,[sal,gave,a,picture,for,val],[]). % no
test(23,X):-
	s(Type,X,B,G-G,[sal,gave,a,picture,to,val],[]). % yes
test(24,X):-
	s(Type,X,B,G-G,[sal,looked,up,"sandy's",nose],[]).
test(25,X):-
	s(Type,X,B,G-G,[sal,looked,up,"sandy's",number],[]).
test(26,X):-
	s(Type,X,B,G-G,[it,was,val,that,looked,up,sal],[]).
test(27,X):-
	s(Type,X,B,G-G,[it,was,val,that,sal,looked,up],[]).
test(28,X):-
	s(Type,X,B,G-G,[it,was,val,who,gave,sal,a,picture,of,sal],[]).
test(29,X):-
	s(Type,X,B,G-G,[it,was,up,"sandy's",number,that,sal,looked],[]). %no-yes, w/o sem
test(30,X):-
	s(Type,X,B,G-G,[it,was,up,"sandy's",nose,that,sal,looked],[]).   %yes
test(a30,X) :-
	s(Type,X,B,G-G,[it,is,a,cat,that,hates,val],[]). %yes
test(b30,X) :-
	s(Type,X,B,G-G,[it,is,a,cat,that,hate,val],[]). %no
test(c30,X) :-
	s(Type,X,B,G-G,[it,is,cats,that,hate,val],[]). %yes
test(d30,X) :-
	s(Type,X,B,G-G,[it,is,cats,that,hates,val],[]). %no

test(31,X):-
	s(Type,X,B,G-G,[what,val,painted,was,a,picture,of,sal],[]).   %yes
test(32,X):-
	s(Type,X,B,G-G,[what,val,gave,was,a,picture,of,sal],[]).   %no
test(a32,X) :-
	s(Type,X,B,G-G,[what,val,hates,are,cats],[]). %yes
test(b32,X),
	s(Type,X,B,G-G,[what,val,hates,is,cats],[]). %?

test(33,X):-
	s(Type,X,B,G-G,[what,val,gave,to,sal,was,a,picture,of,sal],[]). %yes (fails)
test(34,X):-
	s(Type,X,B,G-G,[a,picture,of,sal,was,painted],[]).
test(35,X):-
	s(Type,X,B,G-G,[a,picture,of,sal,was,painted,by,val],[]).
test(36,X):-
	s(Type,X,B,G-G,[a,picture,of,sal,val,painted],[]).
test(37,X):-
	s(Type,X,B,G-G,[what,val,gave,sal,was,a,picture,of,sal],[]).
test(38,X):-
	s(Type,X,B,G-G,[what,val,gave,a,picture,was,a,charity],[]).
test(39,X):-
	s(Type,X,B,G-G,[sal,believed,val,hated,a,cat],[]). %yes
test(40,X):-
	s(Type,X,B,G-G,[sal,thought,val,hated,a,cat],[]).%yes
test(41,X):-
	s(Type,X,B,G-G,[sal,suggested,val,hated,a,cat],[]).%yes
test(42,X):-
	s(Type,X,B,G-G,[sal,asked,val,hated,a,cat],[]).%no
test(43,X):-
	s(Type,X,B,G-G,[sal,wondered,val,hated,a,cat],[]).%no
test(44,X):-
	s(Type,X,B,G-G,[sal,thought,val,a,cat],[]).%yes
test(45,X):-
	s(Type,X,B,G-G,[sal,believed,val,a,cat],[]).%yes
test(46,X):-
	s(Type,X,B,G-G,[sal,suggested,val,a,cat],[]). %no
test(47,X):-
	s(Type,X,B,G-G,[sal,asked,val,a,cat],[]). %no
test(48,X):-
	s(Type,X,B,G-G,[sal,wondered,val,a,cat],[]). %no
test(49,X):-
	s(Type,X,B,G-G,[sal,thought,val],[]). %no
test(50,X):-
	s(Type,X,B,G-G,[sal,believed,val],[]). %yes
test(51,X):-
	s(Type,X,B,G-G,[sal,suggested,val],[]). %yes
test(52,X):-
	s(Type,X,B,G-G,[sal,asked,val],[]). %yes
test(53,X):-
	s(Type,X,B,G-G,[sal,wondered,val],[]). %no
test(54,X):-
	s(Type,X,B,G-G,[sal,thought,val,to,hate,a,cat],[]).%yes
test(a54,X):-
	s(Type,X,B,G-G,[a,cat,sal,thought,val,to,hate],[]).%yes %How to pass gaps through complements??%
test(55,X):-
	s(Type,X,B,G-G,[sal,believed,val,to,hate,a,cat],[]). %yes
test(a55,X):-
	s(Type,X,B,G-G,[a,cat,sal,believed,val,to,hate],[]). %yes
test(56,X):-
	s(Type,X,B,G-G,[sal,suggested,val,to,hate,a,cat],[]). %no
test(57,X):-
	s(Type,X,B,G-G,[sal,asked,val,to,hate,a,cat],[]). %yes
test(58,X):-
	s(Type,X,B,G-G,[sal,wondered,val,to,hate,a,cat],[]). %no
test(a58,X):-
	s(Type,X,B,G-G,[a,cat,sal,wondered,val,to,hate],[]). %no

test(59,X):-
	s(Type,X,B,G-G,[sal,thought,whether,val,believed,sal],[]).%no
test(60,X):-
	s(Type,X,B,G-G,[sal,believed,whether,val,believed,sal],[]). %no
test(61,X):-
	s(Type,X,B,G-G,[sal,suggested,whether,val,believed,sal],[]). %yes
test(62,X):-
	s(Type,X,B,G-G,[sal,wondered,whether,val,believed,sal],[]). %yes
test(63,X):-
	s(Type,X,B,G-G,[sal,asked,whether,val,believed,sal],[]). %yes

test(64,X):-
	s(Type,X,B,G-G,[sal,thought,who,believed,sal],[]).%no
test(65,X):-
	s(Type,X,B,G-G,[sal,believed,who,believed,sal],[]).%no
test(66,X):-
	s(Type,X,B,G-G,[sal,thought,who,believed,sal],[]).%no
test(67,X):-
	s(Type,X,B,G-G,[sal,suggested,who,believed,sal],[]).%yes
test(68,X):-
	s(Type,X,B,G-G,[sal,wondered,who,believed,sal],[]).%yes
test(69,X):-
	s(Type,X,B,G-G,[sal,asked,who,believed,sal],[]).%yes
test(70,X):-
	s(Type,X,B,G-G,[sal,thought,who,sal,believed],[]).%no
test(71,X):-
	s(Type,X,B,G-G,[sal,believed,who,sal,believed],[]).%no
test(72,X):-
	s(Type,X,B,G-G,[sal,thought,who,sal,believed],[]).%no
test(73,X):-
	s(Type,X,B,G-G,[sal,suggested,who,sal,believed],[]).%yes
test(74,X):-
	s(Type,X,B,G-G,[sal,wondered,who,sal,believed],[]).%yes
test(75,X):-
	s(Type,X,B,G-G,[sal,asked,who,sal,believed],[]).%yes
test(76,X):-
	s(Type,X,B,G-G,[does,sal,believe,val],[]).%yes

test(77,X):-
	s(Type,X,B,G-G,[it,is,sal,who,val,asks,lee,to,believe],[]).%yes
test(a77,X):-
	s(Type,X,B,G-G,[it,is,sal,that,val,asks,lee,to,believe],[]).%yes
test(78,X):-
	s(Type,X,B,G-G,[it,is,sal,who,val,wonders,lee,to,believe],[]).%no
test(79,X):-
	s(Type,X,B,G-G,[it,is,sal,who,val,thinks,lee,believes,hates,cats],[]).%yes
test(80,X):-
	s(Type,X,B,G-G,[it,is,sal,who,val,thinks,lee,believes,hate,cats],[]).%no
test(81,X):-
	s(Type,X,B,G-G,[it,is,dogs,who,val,thinks,lee,believes,hate,cats],[]).%yes
test(82,X):-
	s(Type,X,B,G-G,[it,is,dogs,who,val,thinks,lee,believes,hates,cats],[]).%no

test(83,X):-
	s(Type,X,B,G-G,[cats,val,thinks,lee,believes,hate,dogs],[]).%yes
test(a83,X):-
	s(Type,X,B,G-G,[cats,val,thinks,lee,believes,that,hate,dogs],[]).%no
test(b83,X):-
	s(Type,X,B,G-G,[cats,val,believes,that,hate,dogs],[]).%no
test(c83,X):-
	s(Type,X,B,G-G,[cats,val,believes,hate,dogs],[]).%yes
test(84,X):-
	s(Type,X,B,G-G,[cats,val,thinks,lee,believes,hates,dogs],[]).%no
test(85,X):-
	s(Type,X,B,G-G,[cats,val,thinks,lee,wants,to,believe,hates,dogs],[]).%no
test(86,X):-
	s(Type,X,B,G-G,[cats,val,thinks,lee,wants,to,believe,hate,dogs],[]).%yes

test(87,X) :-
	s(Type,X,B,G-G,[it,seems,that,lee,hates,dogs],[]).
test(88,X) :-
	s(Type,X,B,G-G,[it,seems,lee,hates,dogs],[]).
test(89,X) :-
	s(Type,X,B,G-G,[lee,seems,to,hate,dogs],[]).
test(90,X) :-
	s(Type,X,B,G-G,[lee,seems,to,want,to,hate,dogs],[]).
test(91,X) :-
	s(Type,X,B,G-G,[it,is,lee,who,seems,to,want,to,hate,dogs],[]).
test(92,X) :-
	s(Type,X,B,G-G,[it,is,lee,who,seem,to,want,to,hate,dogs],[]). %no
test(93,X) :-
	s(Type,X,B,G-G,[it,seems,whether,lee,hates,dogs],[]). %no
test(94,X) :-
	s(Type,X,B,G-G,[it,seems,why,lee,hates,dogs],[]). %no
test(95,X) :-
	s(Type,X,B,G-G,[it,seems,when,lee,hates,dogs],[]). %no
test(96,X) :-
	s(Type,X,B,G-G,[it,seems,who,hates,dogs],[]). %no

test(97,X) :-
	s(Type,X,B,G-G,[lee,is,the,person,that,val,gave,a,cat,to],[]). %yes
test(98,X) :-
	s(Type,X,B,G-G,[lee,is,the,person,that,gave,a,cat,to,val],[]). %yes
test(99,X) :-
	s(Type,X,B,G-G,[lee,is,the,person,that,gives,cats,to,a,charity],[]). %yes
test(100,X) :-
	s(Type,X,B,G-G,[they,are,the,people,that,give,cats,to,a,charity],[]). %yes
test(101,X) :-
	s(Type,X,B,G-G,[they,are,the,person,that,give,cats,to,a,charity],[]). %no
test(102,X) :-
	s(Type,X,B,G-G,[they,are,the,people,that,gives,cats,to,a,charity],[]). %no
test(103,X) :-
s(Type,X,B,G-G,[they,are,the,people,that,val,thinks,give,cats,to,a,charity],[]). %yes
test(104,X) :-
s(Type,X,B,G-G,[they,are,the,people,that,val,thinks,that,give,cats,to,a,charity],[]).%no
test(105,X) :-
s(Type,X,B,G-G,[it,is,the,answer,that,i,know,whether,lee,understands],[]).
test(106,X) :-
s(Type,X,B,G-G,[it,is,lee,that,i,know,whether,understands,the,answer],[]).
test(107,X) :-
s(Type,X,B,G-G,[it,is,she,who,they,think,sings],[]).
test(108,X) :-
s(Type,X,B,G-G,[it,is,they,who,she,thinks,sleep],[]).
test(109,X) :-
s(Type,X,B,G-G,[it,is,they,who,she,thinks,sleeps],[]).
test(110,X) :-
s(Type,X,B,G-G,[it,is,she,who,they,think,sing],[]).
test(111,X) :-
s(Type,X,B,G-G,[a,couch,i,bet,you,that,you,give,a,cat,to,me],[]).
test(112,X) :-
s(Type,X,B,G-G,[a,cat,i,bet,you,a,couch,that,you,give,to,me],[]).
test(113,X) :-
s(Type,X,B,G-G,[to,val,i,bet,you,a,couch,that,you,give,a,cat],[]).
test(114,X) :-
s(Type,X,B,G-G,[it,is,to,sal,that,i,bet,you,a,couch,that,you,give,a,cat],[]).
test(115,X) :-
s(Type,X,B,G-G,[it,is,you,that,i,bet,sal,a,couch,that,give,me,a,cat],[]).
test(116,X) :-
s(Type,X,B,G-G,[that,she,gives,me,a,cat,i,bet,sal,a,couch],[]).
test(117,X) :-
s(Type,X,B,G-G,[to,sal,val,gave,a,picture],[]).
test(118,X) :-
s(Type,X,B,G-G,[it,was,val,who,sal,gave,lee,a,picture,of],[]).
test(119,X) :-
s(Type,X,B,G-G,[val,it,is,a,cat,that,hates],[]).
test(120,X) :-
s(Type,X,B,G-G,[sal,what,val,painted,was,a,picture,of],[]).
test(121,X) :-
s(Type,X,B,G-G,[of,sal,what,val,painted,was,a,picture],[]).
test(a121,X) :-
s(Type,X,B,G-G,[to,sal,what,val,gave,was,a,cat],[]).
test(122,X) :-
s(Type,X,B,G-G,[cats,what,val,hates,are],[]).
test(123,X) :-
s(Type,X,B,G-G,[cats,what,val,hates,is],[]).
test(124,X) :-
s(Type,X,B,G-G,[by,val,a,picture,of,sal,was,painted],[]).
test(125,X) :-
s(Type,X,B,G-G,[val,a,picture,of,sal,was,painted,by],[]).
test(126,X) :-
s(Type,X,B,G-G,[val,sal,believed,hates,a,cat],[]).
test(127,X) :-
s(Type,X,B,G-G,[val,sal,believed,hate,a,cat],[]).
test(a127,X) :-
s(Type,X,B,G-G,[you,sal,believes,hate,a,cat],[]).
test(b127,X) :-
s(Type,X,B,G-G,[i,sal,believes,hate,a,cat],[]).
test(c127,X) :-
s(Type,X,B,G-G,[they,sal,believes,hate,a,cat],[]).
test(128,X) :-
s(Type,X,B,G-G,[a,cat,sal,thought,val],[]).
test(129,X) :-
s(Type,X,B,G-G,[val,sal,believed,a,cat],[]).
test(130,X) :-
s(decl,X,B,G-G,[a,cat,sal,suggested,val],[]).
test(131,X) :-
s(Type,X,B,G-G,[val,sal,thought,to,hate,dogs],[]).
test(132,X) :-
s(Type,X,B,G-G,[a,cat,sal,suggested,val,to,hate],[]).
test(133,X) :-
s(Type,X,B,G-G,[val,sal,suggested,whether,believed,sal],[]).
test(134,X) :-
s(Type,X,B,G-G,[lee,sal,wondered,whether,val,believed],[]).
test(135,X) :-
s(Type,X,B,G-G,[it,was,lee,sal,wondered,whether,val,believed],[]).
test(136,X) :-
s(Type,X,B,G-G,[val,sal,wondered,whether,believes,sal],[]).
test(a136,X) :-
s(Type,X,B,G-G,[whether,val,believes,sal,lee,wondered],[]).
test(b136,X) :-
s(Type,X,B,G-G,[sal,lee,wondered,whether,val,believes],[]).
test(137,X) :-
s(Type,X,B,G-G,[sal,thought,that,val,believes,sal],[]).
test(138,X) :-
s(Type,X,B,G-G,[sal,believed,that,val,believed,sal],[]).
test(139,X) :-
s(Type,X,B,G-G,[sal,suggested,that,val,believed,sal],[]).
test(140,X) :-
s(Type,X,B,G-G,[sal,wondered,that,val,believed,sal],[]).
test(141,X) :-
s(Type,X,B,G-G,[sal,asked,that,val,believed,sal],[]).
test(142,X) :-
s(Type,X,B,G-G,[sal,verified,that,val,believed,sal],[]).
test(143,X) :-
s(Type,X,B,G-G,[sal,said,that,val,believed,sal],[]).
test(144,X) :-
s(Type,X,B,G-G,[to,the,end,sal,said,that,val,believed,sal],[]).
test(a144,X) :-
s(Type,X,B,G-G,[sal,said,that,val,believed,sal,to,the,end],[]).
test(145,X) :-
s(Type,X,B,G-G,[it,is,lee,that,sal,said,that,believed,sal],[]).
test(146,X) :-
s(Type,X,B,G-G,[it,is,lee,that,sal,said,believed,sal],[]).
test(147,X) :-
s(Type,X,B,G-G,[who,believed,sal],[]).
test(148,X) :-
s(Type,X,B,G-G,[who,does,sal,believe],[]).
test(149,X) :-
s(Type,X,B,G-G,[who,does,sal,think,val,believes],[]).
test(150,X) :-
s(Type,X,B,G-G,[which,cat,does,sal,think,val,believes],[]).
test(151,X) :-
s(Type,X,B,G-G,[does,which,cat,sal,think,val,believes],[]).


% initial fragment doesn't have information about
% scomp barriers, or gaps.
%s(Type,Parse,BarrierInfo,GapInfo,Start,Finish) :-
%	s(Type,Parse,Start,Finish).




%testem defined in gaputilities.pl
testem :-
     pass([1,a1,b1,c1,d1,e1,2,5,7,9,12,13,16,18,19,20,23,24,25,26,27,28,30,a30,c30,31,a32,33,34,35,36,37,38,39,40,41,44,45,50,51,52,54,a54,55,a55,57,61,62,63,67,68,69,73,74,75,76,77,79,81,83,c83,86,87,88,89,90,91,97,98,99,100,103,105,107,108,111,112,113,114,116,117,118,121,124,125,126,127,a127,b127,c127,128,129,131,134,135,a136,b136,137,138,139,142,143,144,a144,146,147,148,149,150]),
     fail([f1,3,4,6,8,10,11,14,15,17,21,22,b30,d30,32,b32,42,43,46,47,48,49,53,56,58,a58,59,60,64,65,66,70,71,72,78,80,82,a83,b83,84,85,92,93,94,95,96,101,102,104,106,109,110,115,119,120,122,123,130,132,133,136,140,141,145,151]).

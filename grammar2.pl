:- consult([utilities1,testsuite1a]).
:- unknown(_,warning).


s([s,[NP,VP]]) --> np(NP,NUM,nom), vp(VP,NUM).
pp([pp,[P,NP]]) --> p(P), np(NP,_,obj).

np([np(Num,Case), [DET, N]], NUM, Case) --> det(DET, NUM), n(N, NUM).
np([np(Num, Case), [PN]], NUM, Case) --> pn(PN,NUM).
np([np(NUM,Case), [PN]], NUM, Case) --> pro(PN, NUM, Case).
vp([vp(NUM), [V]], NUM) --> vi(V,NUM).
vp([vp(NUM), [V,A]], NUM) --> vi(V,NUM),adjunct(A).
vp([vp(NUM), [V,N]], NUM) --> vt(V,NUM), np(N,_,obj).
vp([vp(NUM), [V,N,P]], NUM) --> vd(V,NUM),np(N,_,obj),pp(P).
adjunct([adjunct, PP]) --> pp(PP).

p([p, [W]]) --> [W],{lex(p,W)}.
det([det(NUM), [W]], NUM) --> [W], {lex(det,W,NUM)}.
n([n(NUM), [W]], NUM) --> [W],{lex(n,W,NUM)}.
pn([pn(NUM,[W])], NUM) --> [W],{lex(pn,W,NUM)}.
pro([pr(NUM,Case), [W]], NUM, Case) --> [W], {lex(pro,W,NUM,Case)}.
v([v(NUM), [W]], NUM) --> [W], {lex(v,W,Num)}.
vi([v(NUM), [W]], NUM) --> [W], {lex(vi,W,Num)}.
vt([v(NUM), [W]], NUM) --> [W], {lex(vt,W,Num)}.
vd([v(NUM), [W]], NUM) --> [W], {lex(vd,W,Num)}.

lex(det,a,sg).
lex(det,an,sg).
lex(det,the,_).
lex(p,of).
lex(p,to).
lex(p,on).

lex(n,men,pl).
lex(n,women,pl).
lex(n,man,sg).
lex(n,woman,sg).
lex(n,couch,sg).
lex(n,couches,pl).
lex(pn,bill,sg).
lex(pn,parliament,sg).
lex(pro,i,sg,nom).
lex(pro,me,sg,obj).
lex(pro,she,sg,nom).
lex(pro,her,sg,obj).
lex(pro,he,sg,nom).
lex(pro,him,sg,obj).

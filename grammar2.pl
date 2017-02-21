:- consult([utilities1,testsuite1a]).
:- unknown(_,warning).


s([s,[NP,VP]]) --> np(NP,Per,Num,nom), vp(VP,Per,Num).

s([s, [ADV,S]]) --> adv(ADV), s(S).

s(S)  --> question(S).

question([q, [AUX, S]]) --> aux(AUX), s(S).
question([q, [PRO, S]]) --> q_pro(PRO), s_no_obj(S).
question([q, [PRO, S]]) --> q_pro(PRO), s_no_subj(S,_).
question([q, [QTEMP,AUX, S]]) --> q_temp(QTEMP), aux(AUX), s(S).
question([q, [QADV, AUX, S]]) --> q_adv(QADV), aux(AUX), s(S).
question([q, [QADV,ADV,AUX,S]]) --> q_adv(QADV), adv(ADV), aux(AUX), s(S).
question([q, [VC, S]]) --> vc(VC,Per,Num), s_no_verb(S,Per,Num).
s_no_verb([s, [NP,VP]],Per,Num) --> np(NP,Per,Num,nom), vp_no_verb(VP,_,Num).

%Clauses which can be embedded into a noun phrase
npmodifierclause([relclause, [COMP,S]],Num) --> comp(COMP), s_no_subj(S,Num).
npmodifierclause([relclause, [S]],Num) --> s_no_subj(S,Num).
npmodifierclause([relclause, [COMP,S]],_) --> comp(COMP), s_no_obj(S).
npmodifierclause([relclause, [S]],_) --> s_no_obj(S).
s_no_subj([s,[elipsis,VP]], Num) --> vp(VP,_,Num).
s_no_obj([s,[NP,VP]]) --> np(NP,Per,Num,nom), vp_no_obj(VP,Per,Num).
%Clauses which can be the object of a verb phrase
vcompclause([relclause, [COMP,S]]) --> comp(COMP), s(S).
vcompclause([relclause, [S]]) --> s(S).

ap([ap, [ADJ]]) --> adj(ADJ).
pp([pp,[P,NP]]) --> p(P), np(NP,_,_,obj).

np([np(Num,Case), [DET, N]],3,Num, Case) --> det(DET, Num), nbar(N, Num).
np([np(Num, Case), [PN]],_,Num,Case) --> pn(PN,Num).
np([np(Num,Case), [PN]],Per,Num, Case) --> pro(PN,Per,Num, Case).

vp([vp, [ADV, VP]],Per,Num) --> adv(ADV), vp(VP,Per,Num).

vp([vp(Num), [V]],Per,Num) --> vi(V,Per,Num).
vp([vp(Num), [V,A]],Per,Num) --> vi(V,Per,Num),adjunct(A).
vp([vp(Num), [V,N]],Per,Num) --> vt(V,Per,Num), np(N,_,_,obj).
vp([vp(Num), [V,REL]],Per,Num) --> vt(V,Per,Num), vcompclause(REL).
vp([vp(Num), [V,N,P]],Per,Num) --> vd(V,Per,Num),np(N,_,_,obj),pp(P).
vp([vp(Num), [V, ADV]],Per,Num) --> vi(V,Per,Num), adv(ADV).
vp([vp(Num), [V,A,ADV]],Per,Num) --> vi(V,Per,Num),adjunct(A), adv(ADV).
vp([vp(Num), [V,ADV,A]],Per,Num) --> vi(V,Per,Num), adv(ADV), adjunct(A).
vp([vp(Num), [V,N,ADV]],Per,Num) --> vt(V,Per,Num), np(N,_,_,obj), adv(ADV).
vp([vp(Num), [V,N,P,ADV]],Per,Num) --> vd(V,Per,Num),np(N,_,_,obj),pp(P), adv(ADV).
vp([vp(Num), [V,N1,N2,P]], Per, Num) --> vtt(V,Per,Num),np(N1,_,_,obj),np(N2,_,_,obj),pp(P).
vp([vp(Num), [V,N]], Per, Num) --> vc(V,Per,Num), np(N,_,_,obj).
%rules for elipsis constructions in relative clauses
vp_no_obj([vp(Num), [V,elipsis]],Per,Num) --> vt(V,Per,Num).
vp_no_obj([vp(Num), [V,N,P]],Per,Num) --> vd(V,Per,Num),np(N,_,_,obj),p(P).
vp_no_obj([vp(Num), [V,elipsis,ADV]],Per,Num) --> vt(V,Per,Num), adv(ADV).
vp_no_obj([vp(Num), [V,N,P,ADV]],Per,Num) --> vd(V,Per,Num),np(N,_,_,obj),p(P),adv(ADV).
adjunct([adjunct, PP]) --> pp(PP).
%Rules for elipsis constructions in inverted questions
vp_no_verb([vp(Num), [elipsis, N]],_,_) --> np(N,_,_,obj).

p([p, [W]]) --> [W],{lex(p,W)}.
comp([comp, [W]]) --> [W],{lex(comp,W)}.
adv([adv, [W]]) --> [W],{lex(adv,W)}.
adj([adj,[W]]) --> [W],{lex(adj,W)}.
det([det(Num), [W]], Num) --> [W], {lex(det,W,Num)}.
aux([aux, [W]]) --> [W],{lex(aux, W)}.
q_temp([q_temp, [W]]) --> [W],{lex(q_temp, W)}.
q_adv([q_adv, [W]]) --> [W],{lex(q_adv, W)}.
q_pro([q_pro, [W]]) --> [W],{lex(q_pro, W)}.
nbar([nbar, [N]], Num) --> n(N,Num).
nbar([nbar, [N, REL]], Num) --> n(N,Num), npmodifierclause(REL, Num).
nbar([nbar, [AP,Nbar]], Num) --> ap(AP), nbar(Nbar,Num).
nbar([nbar, [N, P]], Num) --> n(N,Num), pp(P).
n([n(Num), [W]], Num) --> [W],{lex(n,W,Num)}.
pn([pn(Num,[W])], Num) --> [W],{lex(pn,W,Num)}.
pro([pr(Per,Num,Case), [W]], Per, Num, Case) --> [W], {lex(pro,W,Per,Num,Case)}.
v([v(Num), [W]], Per,Num) --> [W], {lex(v,W,Per,Num)}.
vi([v(Num), [W]],Per,Num) --> [W], {lex(vi,W,Per,Num)}.
vt([v(Num), [W]],Per,Num) --> [W], {lex(vt,W,Per,Num)}.
vd([v(Num), [W]],Per,Num) --> [W], {lex(vd,W,Per,Num)}.
vtt([v(Num), [W]],Per,Num) --> [W], {lex(vtt,W,Per,Num)}.
vc([v(Num), [W]],Per,Num) --> [W], {lex(vc,W,Per,Num)}.

lex(det,a,sg).
lex(det,an,sg).
lex(det,the,_).

lex(p,of).
lex(p,to).
lex(p,on).
lex(p,for).
lex(comp,that).
lex(aux, did).
lex(q_temp, when).
lex(q_adv, how).
lex(q_pro, who).
lex(q_pro, what).
lex(adv, peacefully).
lex(adv, quietly).
lex(adv, quickly).

lex(adj, quiet).
lex(adj, tall).

lex(n,men,pl).
lex(n,women,pl).
lex(n,man,sg).
lex(n,woman,sg).
lex(n,couch,sg).
lex(n,book,sg).
lex(n,apple,sg).
lex(n,orange,sg).
lex(n,oranges,pl).
lex(n,capital,sg).
lex(n,couches,pl).
lex(pn,bill,sg).
lex(pn,parliament,sg).
lex(pn,dublin,sg).
lex(pn,ireland,sg).
lex(pro,i,1,sg,nom).
lex(pro,me,1,sg,obj).
lex(pro,we,1,pl,nom).
lex(pro,us,1,pl,obj).
lex(pro,she,3,sg,nom).
lex(pro,this,3,sg,nom).
lex(pro,her,3,sg,obj).
lex(pro,you,2,_,_).
lex(pro,yall,2,pl,_).
lex(pro,he,3,sg,nom).
lex(pro,him,3,sg,obj).
lex(pro,they,3,pl,nom).
lex(pro,them,3,pl,obj).

lex(vi, sleep,_,pl).
lex(vi, sleep,1,sg).
lex(vi, sleep,2,sg).
lex(vi, sleeps,3,sg).
lex(vt, eat,_,pl).
lex(vt, eat, 1,sg).
lex(vt, eat, 2,sg).
lex(vt, eats, 3,sg).
lex(vd, give,_,pl).
lex(vd, give,1,sg).
lex(vd, give,2,sg).
lex(vd, gives,3,sg).
lex(vtt,swapped,_,_).
%vc = verb copula, thought it made sense to class it seperately
lex(vc,am,1,sg).
lex(vc,are,2,_).
lex(vc,is,3,sg).
lex(vc,are,_,pl).

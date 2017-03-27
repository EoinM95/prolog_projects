% Last Modified: Mon Mar  6 10:17:00 2017 (vogel)
% it is not necessary to use this as a base grammar, but it
% is possible to do so.

:- unknown(_,trace).
:- consult([gaputilities,gaptestsuite]).

%s(Type,Parse,BarrierInfo,GapInfo,Start,Finish) :-
%	s(Type,Parse,Start,Finish)
%

% cleft
s(decl,[s,[[pr(exp), [W]],[topic,VP],[comment,Topic]]],B, GI-GO) -->
        pro(exp,[pr(exp), [W]],Per,Num,Case),
        vp(vc,VP,P,N,fin,GI-GO),
        relc([that],Topic).

% pseudocleft
s(decl,[s,[[topic,Topic],[comment,VP]]],B, GI-GO) -->
        relc([what],Topic),
        vp(vc,VP,P,N,fin,GI-GO).

% topicalization
s(decl,[s,[[topic,Topic],[comment,[NP,VP]]]],B, _) -->
	np(Topic,Per1,Num2,obj,nogap-nogap),
	np(NP,Per,Num,nom,nogap-nogap),
	vp(Type,VP,Per,Num,fin,gap-nogap).%vpobjgone(Type,VP,Per,Num,fin).

% sub-aux inversion questions
s(interog,[q, [V,NP,VP]],B,_) -->
	v(aux,V,Per,Num,fin),
	np(NP,Per,Num,nom,nogap-nogap),
	vp(Type,VP,Per,Num,bse,nogap-nogap).

% sub-aux inversion questions
s(interog,[q, [W, S]],B, GI-GO) -->
	wh_word(W,adv,P,N,C),
	s(decl,S, B, GI-GO).

% role-query questions (of subject)
s(interog,[q, [W, VP]],B, GI-GO) -->
	wh_word(W,role,P,N,C),
	vp(Type,VP,_,_,fin,nogap-nogap).

% role-query questions (of object)
s(interog,[q, [W, [s, [NP,VP]]]],B, _) -->
	wh_word(W,role,P,N,C),
	np(NP,Per,Num,nom, nogap-nogap),
	vp(Type,VP,Per,Num,fin,gap-nogap).%vpobjgone(Type,VP,Per,Num,fin). % see below

% a basic delarative
s(decl,[s, [NP,VP]],B, nogap-nogap) -->
	np(NP,Per,Num,nom,nogap-nogap),
	vp(Type,VP,Per,Num,fin,nogap-nogap).

s(decl,[s, [A,NP,VP]],B, nogap-nogap) -->
    adjunct(A, nogap-nogap),
  	np(NP,Per,Num,nom,nogap-nogap),
  	vp(Type,VP,Per,Num,fin,gap-nogap).

% a nonfinite basic delarative
s(inf,[s, [NP,VP]],B, _) -->
	np(NP,Per,Num,nom,nogap-nogap),
	vp(inf,VP,Per,Num,inf,nogap-nogap).

s(nv,[s, [NP1,NP2]],B, _) -->
	np(NP1,Per,Num,obj,nogap-nogap),
	np(NP2,Per1,Num1,_,nogap-nogap).



s(comp,[Comp, [S]],B, GI-GO) -->
	comp(Comp),
	s(Form,S,B, GI-GO),
	{Form \== comp}.

% np classes
% 1 -- DPs with nonnull determiners are always 3rd person
% 2 -- proper names can be either 2nd or 3rd
% 3 -- pronouns are marked for case (thus, case is inherited from
%      the pronoun.  case comes from the verb in the other instances
np([np(Num,Case), [DET,N]],3,Num,Case,nogap-nogap) -->
	det(DET,Num),
	nom(N,Num).
np([np(Num,Case), [PN]],_,Num,Case,nogap-nogap) -->
	pn(PN,Num).
np([np(Num,Case), [PN]],Per,Num,Case,nogap-nogap) -->
	pro(pro,PN,Per,Num,Case).

np([np(Num,Case)],Per,Num,Case,gap-nogap) --> [].

nom(N,Num) --> n(N,Num).
nom([nom, [A, N]],Num) --> adj(A,Type), nom(N,Num).
nom([nom, [N, A]],Num) --> n(N,Num), adjunct(A, G-G).

% subject relatives go in one fell swoop
relc(Type,[relc, [Pro,VP]]) -->
	pro(relpro,Pro,_,_,nom),
	vp(Vtype,VP,Per,Num,Form,nogap-nogap).

% object relatives are tedious using this flat structure
relc(Type,[relc, [Pro,N,V]]) -->
	pro(relpro,Pro,_,_,nom),
	np(N,Per,Num,nom,nogap-nogap),
	vc(V,Per,Num,Form).
% vp(vc,[vp(Num,Form), [V,N]],Per,Num,Form) -->

%vp(vi,[vp(Num,Form), [V]],Per,Num,Form) --> exists only un subject rel.

relc(Type,[relc, [Pro,N,V]]) -->
	pro(relpro,Pro,_,_,nom),
	np(N,Per,Num,nom,nogap-nogap),
	vt(V,Per,Num,Form).
%vp(vt,[vp(Num,Form), [V,N]],Per,Num,Form) -->

relc(Type,[relc, [Pro,N,V,A]]) -->
	pro(relpro,Pro,_,_,nom),
	np(N,Per,Num,nom,nogap-nogap),
	vt(V,Per,Num,Form),
	adjunct(A, G-G).

relc(Type,[relc, [Pro,N,V,N1]]) -->
	pro(relpro,Pro,_,_,nom),
	np(N,Per,Num,nom,nogap-nogap),
	vd(V,Per,Num,Form),
	np(N1,_,_,obj,nogap-nogap).
%vp(vd,[vp(Num,Form), [V,N,P]],Per,Num,Form) -->

relc(Type,[relc, [Pro,N,V,P]]) -->
	pro(relpro,Pro,_,_,nom),
	np(N,Per,Num,nom,nogap-nogap),
	vd(V,Per,Num,Form),
	pp(to,P, G-G).
%vp(vd,[vp(Num,Form), [V,N,P]],Per,Num,Form) -->

relc(Type,[relc, [Pro,N,V,N2,Scomp]]) -->
	pro(relpro,Pro,_,_,nom),
	np(N,Per,Num,nom,nogap-nogap),
	vtr(V,Per,Num,Form),
	np(N2,_,_,obj,nogap-nogap),
	s(comp,Scomp, B, G-G).
%vp(vtr,[vp(Num,Form), [V,N1,N2,Scomp]],Per,Num,Form) -->

relc(Type,[relc, [Pro,N,V,N1,N2]]) -->
	pro(relpro,Pro,_,_,nom),
	np(N,Per,Num,nom,nogap-nogap),
	vtr(V,Per,Num,Form),
	np(N1,_,_,obj,nogap-nogap),
	np(N2,_,_,obj,nogap-nogap).



% subcatorization frames
% here, case assignment from verbs isn't deemed lexical
vp(inf,[infinitive, VS],Per,Num,inf,GapIn-GapOut) -->
	vinf([to],Per,Num,inf),
	vp(Type,VS,_,_,bse,GapIn-GapOut).


vp(vc,[vp(Num,Form), [V,N]],Per,Num,Form,GapIn-GapOut) -->
	vc(V,Per,Num,Form),
	np(N,_,_,nom,G-G).
vp(vc,[vp(sg,Form), [V,PP]],Per,sg,Form,GapIn-GapOut) -->
	vc(V,Per,sg,Form),
	pp(Type,PP,G-G).

vp(vi,[vp(Num,Form), [V]],Per,Num,Form,_) --> vi(V,Per,Num,Form).
vp(vi,[vp(Num,Form), [V,A]],Per,Num,Form,GapIn-GapOut) --> vi(V,Per,Num,Form),adjunct(A,GapIn-GapOut).
vp(vt,[vp(Num,Form), [[vt(norm,Num),V],Complement]],Per,Num,Form,_) -->
	vt([vt(Subcat,Num),V],Per,Num,Form),
	{complement_structured(Subcat,Complement,Term)},
	 Term.


vp(vt,[vp(Num,Form), [[vt(norm,Num),V],N]],Per,Num,Form,GapIn-GapOut) -->
	vt([vt(norm,Num),V],Per,Num,Form),
	np(N,_,_,obj,G-G).
vp(vt,[vp(Num,Form), [V,N,A]],Per,Num,Form,GapIn-GapOut) -->
	vt(V,Per,Num,Form),
	np(N,_,_,obj,GapIn-GapMid),
	adjunct(A, GapMid-GapOut).
vp(vd,[vp(Num,Form), [V,N,P]],Per,Num,Form,GapIn-GapOut) -->
	vd(V,Per,Num,Form),
	np(N,_,_,obj,GapIn-GapMid),
	pp(to,P,GapMid-GapOut).
vp(vd,[vp(Num,Form), [V,N1,N2]],Per,Num,Form,GapIn-GapOut) -->
	vd(V,Per,Num,Form),
	np(N1,_,_,obj,GapIn-GapMid),
	np(N2,_,_,obj,GapMid-GapOut).

vp(vtr,[vp(Num,Form), [V,N1,N2,Scomp]],Per,Num,Form,GapIn-GapOut) -->
	vtr(V,Per,Num,Form),
	np(N1,_,_,obj,GapIn-GapMid1),
	np(N2,_,_,obj,GapMid1-GapMid2),
	s(comp,Scomp,B, GapMid2-GapOut).

% passive
vp(Type,[vp(Num,Form), [V,VP]],Per,Num,fin,_) -->
	v(paux,V,Per,Num,fin),
	vp(Type,VP,_,_,ppl,gap-nogap).%vpobjgone(Type,VP,_,_,ppl).

vp(Type,[vp(Num,Form), [V,VP,PP]],Per,Num,fin,GapIn-GapOut) -->
	v(paux,V,Per,Num,fin),
	vp(Type,VP,_,_,ppl,gap-nogap).%vpobjgone(Type,VP,_,_,ppl),
	pp(by,PP, GapIn-GapOut).

% passive component constituency hack
% this is expressively equivalent as a constituent
% name to the slash representation; however, it
% involves ill-motivated constituent structure.
% vpobjgone(vt,[vp(Num,Form), [V]],Per,Num,Form) -->
% 	vt(V,Per,Num,Form).
% vpobjgone(vt,[vp(Num,Form), [V,A]],Per,Num,Form) -->
% 	vt(V,Per,Num,Form),
% 	adjunct(A).
% vpobjgone(vd,[vp(Num,Form), [V,P]],Per,Num,Form) -->
% 	vd(V,Per,Num,Form),
% 	pp(to,P).
% vpobjgone(vd,[vp(Num,Form), [V,N2]],Per,Num,Form) -->
% 	vd(V,Per,Num,Form),
% 	np(N2,_,_,obj,nogap-nogap).
% vpobjgone(vtr,[vp(Num,Form), [V,N1,Scomp]],Per,Num,Form) -->
% 	vtr(V,Per,Num,Form),
% 	np(N1,_,_,obj,nogap-nogap),
% 	s(comp,Scomp,B, G-G).
% vpobjgone(vtr,[vp(Num,Form), [V,N1,N2]],Per,Num,Form) -->
% 	vtr(V,Per,Num,Form),
% 	np(N1,_,_,obj,nogap-nogap),
% 	np(N2,_,_,obj,nogap-nogap).


pp(Type,[pp, [P,NP]],GapIn-GapOut) --> p(Type,P), np(NP,_,_,obj,GapIn-GapOut).
pp(Type,[pp],gap-nogap) --> [].
adjunct([adjunct, PP],GapIn-GapOut) --> pp(Type,PP,GapIn-GapOut).
adjunct([adjunct],GapIn-GapOut) --> [].



%Preterminals
p(W,[p, [W]]) --> [W],{lex(p,W)}.
det([det(Num), [W]],Num) --> [W],{lex(det,W,Num)}.
n([n(Num),[W]],Num) --> [W],{lex(n,W,Num)}.
pn([pn(Num), [W]],Num) --> [W],{lex(pn,W,Num)}.
pro(relpro,[pr(Per,Num,Case), [W]],Per,Num,Case) --> [W],
                                {lex(relpro,W,Per,Num,Case)}.
pro(pro,[pr(Per,Num,Case), [W]],Per,Num,Case) --> [W],
                                {lex(pro,W,Per,Num,Case)}.
pro(exp,[pr(exp), [W]],Per,Num,Case) --> [W],
                                {lex(exp,W)}.
%
wh_word(W,Sort,P,N,C) --> [W], {lex(wh,Sort,W,P,N,C)}.

vinf([to],Per,Num,inf) --> [to].

vc([v(Num), [W]],Per,Num,Form) --> [W],{lex(vc,W,Per,Num,Form)}.
v(aux,[v(Num), [W]],Per,Num,Form) --> [W],{lex(aux,W,Per,Num,Form)}.
v(paux,[v(Num), [W]],Per,Num,Form) --> [W],{lex(paux,W,Per,Num,Form)}.
v([v(Num), [W]],Per,Num,Form) --> [W],{lex(v,W,Per,Num,Form)}.
vi([vi(Num), [W]],Per,Num,Form) --> [W],{lex(vi,W,Per,Num,Form)}.

vt([vt(Type,Num), [W]],Per,Num,Form) --> [W],{lex(vt(Type),W,Per,Num,Form)}.
%phrasal verbs
vt([vt(Type,Num), [W,P]],Per,Num,Form) --> [W,P],{lex(vt(Type),[W,P],Per,Num,Form)}.

vd([vd(Num), [W]],Per,Num,Form) --> [W],{lex(vd,W,Per,Num,Form)}.
vtr([vtr(Num), [W]],Per,Num,Form) --> [W],{lex(vtr,W,Per,Num,Form)}.
comp([W]) --> [W], {lex(comp,W)}.

adj([adj,[W]],Type) --> [W], {lex(adj,W,Type)}.

lex(wh,adv,whether,_,_,_).
lex(wh,adv,why,_,_,_).
lex(wh,adv,if,_,_,_).
lex(wh,role,who,_,_,nom).
lex(wh,role,what,_,_,_).
lex(wh,role,whom,_,_,obj).

lex(comp,that).
lex(adj,former,npred).
lex(adj,ugly,int).

lex(det,a,sg).
lex(det,an,sg).
lex(det,the,X).
lex(det,"sandy's",X).
lex(p,on).
lex(p,of).
lex(p,by).
lex(p,to).
lex(p,up).
lex(p,for).
lex(n,cat,sg).
lex(n,cats,pl).
lex(n,charity,sg).
lex(n,charities,pl).
lex(n,picture,sg).
lex(n,pictures,pl).
lex(n,number,sg).
lex(n,numbers,pl).
lex(n,nose,sg).
lex(n,noses,pl).
lex(n,men,pl).
lex(n,women,pl).
lex(n,man,sg).
lex(n,woman,sg).
lex(n,couch,sg).
lex(n,couches,pl).
lex(pn,bill,sg).
lex(pn,sal,sg).  % new lexical entries
lex(pn,val,sg).
lex(pn,parliament,pl).

lex(exp,it).
lex(relpro,what,_,_,_).
lex(relpro,who,_,_,_).
lex(relpro,which,_,_,_).
lex(relpro,that,_,_,_).

lex(pro,i,1,sg,nom).
lex(pro,me,1,sg,obj).
lex(pro,we,1,pl,obj).
lex(pro,us,1,pl,obj).
lex(pro,she,3,sg,nom).
lex(pro,her,3,sg,obj).
lex(pro,you,2,X,Y).
lex(pro,yall,2,pl,Y).
lex(pro,he,3,sg,nom).
lex(pro,him,3,sg,obj).
lex(pro,they,3,pl,nom).
lex(pro,them,3,pl,obj).

lex(vi,slept,P,N,ppl).
lex(vi,sleep,P,N,bse).
lex(vi,sleeps,3,sg,fin).
lex(vi,sleep,1,pl,fin).
lex(vi,sleep,2,pl,fin).
lex(vi,sleep,1,sg,fin).
lex(vi,sleep,2,sg,fin).
lex(vi,sleep,3,pl,fin).

lex(vc,been,P,N,ppl).
lex(vc,be,P,N,bse).
lex(vc,is,1,sg,fin).
lex(vc,are,2,sg,fin).
lex(vc,is,3,sg,fin).
lex(vc,are,P,pl,fin).
lex(vc,was,1,sg,fin).
lex(vc,were,2,sg,fin).
lex(vc,was,3,sg,fin).
lex(vc,were,_,pl,fin).

lex(paux,had,P,N,fin).
lex(paux,was,1,sg,fin).
lex(paux,were,2,sg,fin).
lex(paux,was,3,sg,fin).
lex(paux,were,P,pl,fin).
lex(paux,have,1,sg,fin).
lex(paux,have,2,sg,fin).
lex(paux,has,3,sg,fin).
lex(paux,have,P,pl,fin).

lex(aux,done,P,N,ppl).
lex(aux,do,P,N,bse).
lex(aux,do,1,sg,fin).
lex(aux,do,2,sg,fin).
lex(aux,does,3,sg,fin).
lex(aux,do,P,pl,fin).

%word(vt([np,s(interog,B,G-G),s(bse,B,G-G),s(comp,B,G-G)]),[saw],fin).
%word(vt([np,s(interog,B,G-G),s(fin),s(comp,B,G-G),s(inf,B,G-G)]),[heard],fin).
%word(vt([np,s(interog,B,G-G),s(fin),s(comp,B,G-G),s(inf,B,G-G)]),[knew],fin).
%word(vt([np,s(interog,B,G-G),s(fin),s(comp,B,G-G),s(inf,B,G-G)]),[verified],fin).
%word(vt([np,s(interog,B,G-G),s(fin),s(comp,B,G-G),s(inf,B,G-G)]),[established],fin).
%word(vt([s(fin),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),[thought],fin).
%word(vt([s(fin),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),[denied],fin).
%word(vt([np,s(fin),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),[believed],fin).
%word(vt([np,s(fin),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),[suspected],fin).
%word(vt([np,s(interog,B,G-G),s(fin),s(comp,B,G-G),s(bse,B,G-G)]),[suggested],fin).
%word(vt([np,s(interog,B,G-G),s(fin),s(comp,B,G-G)]),[indicated],fin).
%word(vt([s(interog,B,G-G),s(fin),s(comp,B,G-G)]),[said],fin).
%word(vt([np,s(interog,B,G-G),s(inf,B,G-G)]),[told],fin).
%word(vt([np,s(interog,B,G-G),s(inf,B,G-G)]),[asked],fin).
%word(vt([s(interog,B,G-G)]),[wondered],fin).
%word(vt([s(interog,B,G-G)]),[inquired],fin).


lex(vt([np(S,N1,P1,C,G-G),s(decl,B,G-G),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),believed,P,N,fin).
lex(vt([np(S,N1,P1,C,G-G),s(decl,B,G-G),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),believed,P,N,ppl).
lex(vt([np(S,N1,P1,C,G-G),s(decl,B,G-G),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),believe,P,N,bse).
lex(vt([np(S,N1,P1,C,G-G),s(decl,B,G-G),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),believe,1,sg,fin).
lex(vt([np(S,N1,P1,C,G-G),s(decl,B,G-G),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),believe,2,sg,fin).
lex(vt([np(S,N1,P1,C,G-G),s(decl,B,G-G),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),believes,3,sg,fin).
lex(vt([np(S,N1,P1,C,G-G),s(decl,B,G-G),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),believe,P,pl,fin).


lex(vt([s(decl,B,G-G),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),thought,P,N,ppl).
lex(vt([np(S,N1,P1,C,G-G),s(interog,B,G-G),s(decl,B,G-G),s(comp,B,G-G),s(bse,B,G-G)]),suggested,P,N,ppl).
lex(vt([s(interog,B,G-G)]),wondered,P,N,ppl).
lex(vt([np(S,N1,P1,C,G-G),s(interog,B,G-G),s(inf,B,G-G)]),asked,P,N,ppl).
lex(vt([s(decl,B,G-G),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),thought,P,N,fin).
lex(vt([np(S,N1,P1,C,G-G),s(interog,B,G-G),s(decl,B,G-G),s(comp,B,G-G),s(bse,B,G-G)]),suggested,P,N,fin).
lex(vt([s(interog,B,G-G)]),wondered,P,N,fin).
lex(vt([np(S,N1,P1,C,G-G),s(interog,B,G-G),s(inf,B,G-G)]),asked,P,N,fin).

word(vt([np(S,N1,P1,C,G-G),s(interog,B,G-G),s(bse,B,G-G),s(comp,B,G-G)]),[saw],P,N,fin).
word(vt([np(S,N1,P1,C,G-G),s(interog,B,G-G),s(decl,B,G-G),s(comp,B,G-G),s(inf,B,G-G)]),[heard],P,N,fin).
word(vt([np(S,N1,P1,C,G-G),s(interog,B,G-G),s(decl,B,G-G),s(comp,B,G-G),s(inf,B,G-G)]),[knew],P,N,fin).
word(vt([np(S,N1,P1,C,G-G),s(interog,B,G-G),s(decl,B,G-G),s(comp,B,G-G),s(inf,B,G-G)]),[verified],P,N,fin).
word(vt([np(S,N1,P1,C,G-G),s(interog,B,G-G),s(decl,B,G-G),s(comp,B,G-G),s(inf,B,G-G)]),[established],P,N,fin).
word(vt([s(decl,B,G-G),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),[denied],P,N,fin).
word(vt([np(S,N1,P1,C,G-G),s(decl,B,G-G),s(comp,B,G-G),s(nv,B,G-G),s(inf,B,G-G)]),[suspected],P,N,fin).
word(vt([np(S,N1,P1,C,G-G),s(interog,B,G-G),s(decl,B,G-G),s(comp,B,G-G)]),[indicated],P,N,fin).
word(vt([s(interog,B,G-G),s(decl,B,G-G),s(comp,B,G-G)]),[said],P,N,fin).
word(vt([np(S,N1,P1,C,G-G),s(interog,B,G-G),s(inf,B,G-G)]),[told],P,N,fin).
word(vt([s(interog,B,G-G)]),[inquired],P,N,fin).



lex(vt(norm),eaten,P,N,ppl).
lex(vt(norm),eat,P,N,bse).
lex(vt(norm),eat,1,pl,fin).
lex(vt(norm),eat,2,pl,fin).
lex(vt(norm),eat,3,pl,fin).
lex(vt(norm),eat,1,pl,fin).
lex(vt(norm),eat,2,pl,fin).
lex(vt(norm),eats,3,sg,fin).

lex(vt(norm),painted,P,N,fin).
lex(vt(norm),painted,P,N,ppl).

lex(vd,given,P,N,ppl).
lex(vd,give,P,N,bse).
lex(vd,give,1,sg,fin).
lex(vd,give,2,sg,fin).
lex(vd,gives,3,sg,fin).
lex(vd,give,1,pl,fin).
lex(vd,give,2,pl,fin).
lex(vd,give,3,pl,fin).

lex(vd,gave,P,N,fin).

lex(vt(norm),looked,P,N,ppl).
lex(vt(norm),looked,P,N,fin).
lex(vt(norm),[looked,up],P,N,fin).

lex(vt(norm),hated,P,N,fin).
lex(vt(norm),hate,P,N,bse).

lex(vtr,bet,P,N,ppl).
lex(vtr,bet,P,N,bse).
lex(vtr,bet,1,sg,fin).
lex(vtr,bet,2,sg,fin).
lex(vtr,bets,3,sg,fin).
lex(vtr,bet,N,pl,fin).


testem0 :-
     pass([1,a1,2,5,7,9,12,13,16,18,19,20,23,24,25,26,27,28,29,30,31,33,34,35,36,37,38,39,40,41,44,45,50,51,52,54,55,57,61,62,63,67,68,69,73,74,75,76]),
     fail([3,4,6,8,10,11,14,15,17,21,22,32,42,43,46,47,48,49,53,56,58,59,60,64,65,66,70,71,72]).

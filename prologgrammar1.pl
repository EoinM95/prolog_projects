% Last Modified: Mon Jan 16 08:58:26 2017 (vogel)


% Initial Prolog Theory of Grammar

:- consult([utilities1,testsuite1]).
:- unknown(_,warning).

% grammar that uses Prolog utilities

% things missing: valency entries, vocabulary, constituent structures, etc.

word(det,[the]).
word(det,[a]).
word(det,[an]).
word(det,["sandy's"]).

word(p,[in]).
word(p,[on]).
word(p,[of]).
word(p,[to]).
word(p,[up]).
word(a(int),[green]).
word(a(npred),[former]).


word(vt,[hate],[np],[np],bse).
word(vt,[hated],[np],[np],fin).
word(vt,[like],[np],[np],bse).
word(vt,[liked],[np],[np,vcompclause,s(inf)],fin).
word(vt,[ask],[np],[np],bse).
word(vt,[asked],[np],[np,whetherclause,whoclause],fin).
word(vt,[asked],[np],[s(comp),s(inf)],fin).
word(vt,[looked,up],[np],fin).

% word/4 cat, phon, subcat/2, form
word(vt,[sold],[np],[np],fin).
word(vt,[looked,up],[np],[np],fin).
word(vt,[looked],[np],[pp(X)],fin).
word(vt,[painted],[np],[np],fin).
word(vt,[painted],[np],[np],ppl). %subj + obj
word(vt,[knew],[np],[s(fin),s(comp),s(inf),vcompclause,whetherclause,whoclause],fin).
word(vt,[knew],[np],[np],fin).
word(vt,[thought],[np],[s(fin),s(comp),s(nv),s(inf),vcompclause],fin).
word(vt,[believed],[np],[s(fin),s(inf),vcompclause,s(nv)],fin).
word(vt,[believed],[np],[np],fin).
word(vt,[denied],[np],[s(fin),s(nv),s(comp),vcompclause],fin).
word(vt,[doubted],[np],[vcompclause,np,s(fin),s(inf)],fin).
word(vt,[hated],[np],[vcompclause,s(inf)],fin).
word(vt,[suggested],[np],[np,s(fin),vcompclause,whetherclause,whoclause],fin).
word(vt, [persuaded], [np], [np, s(inf)], fin).
word(vt, [proved], [np], [vcompclause,s(inf),s(fin),s(nv),whetherclause], fin).
word(vt,[queried],[np],[np,whetherclause,whoclause],fin).
word(vt,[refuted],[np],[np,vcompclause],fin).
word(vt,[regretted],[np],[s(fin),vcompclause,s(inf)],fin).
word(vt,[verified],[np],[s(fin),vcompclause,whetherclause,whoclause],fin).
word(vt, [wanted], [np], [np, s(inf)], fin).
word(vt,[wondered],[np],[whetherclause,whoclause],fin).
word(vt,[said],[np],[s(fin),vcompclause,whetherclause,whoclause],fin).
word(vd,[gave],[np],[pp(to),np],[np,pp([to])],fin).
word(vi,[sneezed],[np],fin).
word(va,[was],[np],[vp],fin).
word(vc,[was],[np],[np,ap,vp,pp(X)],fin).

word(name,[sal]).
word(name,[val]).
word(name,[teri]).
word(name,[lee]).
word(n,[detail]).
word(n,[details]).
word(n,[dog]).
word(n,[ass]).
word(n,[cat]).
word(n,[coffee]).
word(n,[nose]).
word(n,[number]).
word(n,[picture]).
word(n,[pharmacology]).
word(pn,[it]).
word(relpro,[what]).
word(relpro,[that]).
word(comp,[that]).
word(comp,[whether]).
word(exp,[it]).


np(NS) :-
        word(pn,NS).
np(NS) :-
        word(name,NS).
np(NS) :-
        nbar(NS).
np(NS) :-
        append([DS,NBS],NS),
        det(DS),
        nbar(NBS).

%coordinated NPs (syncategorematic)
np(NS) :-
        append([NP1,[and],NP2],NS),
        np(NP1),
        np(NP2).

nbar(NBS) :-
        word(n,NBS).


%adjunctp should probably be adjectivep
nbar(NBS) :-
        append([AP,NB],NBS),
        adjunctp(Type,AP),
        nbar(NB).
nbar(NBS) :-
        append([NB,PP],NBS),
        pp(_,PP),
        nbar(NB).

%coordinated nbar (syncategorematic)
nbar(NBS) :-
        append([NB1,[and],NB2],NBS),
        nbar(NB1),
        nbar(NB2).

%object relative
relc(Rtype,RS) :-
        append([Rtype,SS,VS],RS),
        word(relpro,Rtype),
        np(SS),
        word(vt,VS,S,O,fin).
%object relative -- ditransitive
relc(Rtype,RS) :-
        append([Rtype,SS,VS,OS],RS),
        word(relpro,Rtype),
        np(SS),
        word(vd,VS,S,O1,O2,fin),
	append([O1,O2],Objects),
	valence(Objects,OS,Type).

%subject relative
relc(Rtype,RS) :-
        append([Rtype,SS,VS],RS),
        word(relpro,Rtype),
        vp(vt,VS,fin).

vcompclause([that|S]) :-
    s(fin,S).

whetherclause([whether|S]) :-
  s(fin,S).

whoclause([who|S]) :-
  %slightly hacky but the only way I could get this to work.
  %Ideally it would be any pronoun but the result of word(pn,X)
  %is always X = [it], thus [[it]|S] is not a valid sentence
  %making this a bit awkward
  s(fin,[it|S]).


p(PS,PS) :-
        word(p,PS).

det(DS) :-
        word(det,DS).

vp(Type,[to|VS],inf) :-
        vp(Type,VS,bse).

vp(vi,VS,fin) :-
        word(vi,VS,S,fin).
vp(vt,VS,Form) :-
        append([V,SS],VS),
        word(vt,V,S,OS,Form),
	valence(OS,SS,Type).

vp(vd,VS,Form) :-
        %dative marking & no dative marking
        append([V,OS1,OS2],VS),
        word(vd,V,S,O1,O2,Form),
	valence(O1,OS1,T1),
	valence(O2,OS2,T2).

% for non constituent NP coordination (syncategorematic)
% no dative marking & dative marking.
vp(vd,VS,Form) :-
        append([V,OS1,OS2,[and],OS3,OS4],VS),
        word(vd,V,O1,O2,Form),
        valence(O1,OS1,T1),
	valence(O2,OS2,T2),
	valence(O1,OS3,T1),
	valence(O2,OS4,T2).

%constituent VP coordination (syncategorematic)
vp(Sort,VS,Form) :-
        append([V1,[and],V2],VS),
        vp(Sort,V1,Form),
        vp(Sort1,V2,Form).

vp(vc,VS,fin) :-
        append([V,AP],VS),
        word(vc,V,S,O,fin),
        adjunctp(a(pred),AP).
vp(vc,VS,fin) :-
        append([V,NP],VS),
        word(vc,V,S,O,fin),
        np(NP).
vp(vc,VS,fin) :-
        append([V,PP],VS),
        word(vc,V,S,O,fin),
        pp(_,PP).


adjunctp(a(pred),AP) :-
        word(a(int),AP).
adjunctp(a(npred),AP) :-
        word(a(npred),AP).

% cleft
s(fin,S) :-
        append([Exp,VS,Topic],S),
        word(exp,Exp),
        vp(vc,VS,fin),
        relc([that],Topic).

% pseudocleft
s(fin,S) :-
        append([Topic,VS],S),
        relc([what],Topic),
        vp(vc,VS,fin).

% passive
vp(Type,[H|VS],fin) :-
        word(va,[H],S,O,fin),
        vp(Type,VS,ppl,passive).
vp(vt,VS,ppl,passive) :-
        % subject demoted
        append([V,[by],SS],VS),
        word(vt,V,SSub,SOb,ppl),
	valence(SSub,SS,Type).

vp(vt,VS,ppl,passive) :-
        % subject not realized
        append([V],VS),
        word(vt,V,SSub,SOb,ppl).


%topicalization
s(fin,S) :-
        append([OS,SubS,V],S),
        word(vt,V,SC,OC,fin),
	valence(SC,OS,Type),
        np(SubS).
s(fin,S) :-
        append([NS,VS],S),
        np(NS),
        vp(vi,VS,Form).
s(Form,S) :-
        append([NS,VS],S),
        np(NS),
        vp(vt,VS,Form).
s(Form,S) :-
        append([NS,VS],S),
        np(NS),
        vp(vd,VS,Form).

s(nv,S) :-
        append([NS1,NS2],S),
        np(NS1),
        np(NS2).
s(nv,S) :-
        append([NS,PP],S),
        np(NS),
        pp(Sort,PP).

p(PS) :-
        word(p,PS).


pp(PS,PP) :-
        append([PS,NS],PP),
	      p(PS),
        np(NS).

% coordinated PPs (must be same sort; syncategorematic).
pp(Sort,PP) :-
        append([PP1,[and],PP2],PP),
        pp(Sort,PP1),
        pp(Sort,PP2).

% Last Modified: Mon Jan 16 08:58:41 2017 (vogel)

% Initial testsuite



test(1) :- s(X,[sal,painted,a,picture,of,val]).
test(a1) :- s(X,[sal,painted,a,picture,to,val]). %a more complete grammar
                                                 %would rule this out
                                                 %as `picture' subcategorizes
test(2) :- s(X,[sal,gave,a,picture,to,val]). % yes
test(3) :- s(X,[sal,gave,a,picture,for,val]). % no
test(4) :- s(X,[sal,painted,a,picture,of,val,and,a,picture,of,sal]).
test(5) :- s(X,[sal,painted,a,picture,of,val,and,sold,it]).
test(6) :- s(X,[sal,gave,a,picture,to,val,and,a,picture,to,sal]). %yes
test(7) :- s(X,[sal,gave,a,picture,of,val,and,a,picture,of,sal]). %yes
test(8) :- s(X,[sal,looked,up,"sandy's",nose]).
test(9) :- s(X,[sal,looked,up,"sandy's",details]).
test(10) :- s(X,[it,was,val,that,looked,up,sal]).
test(11) :- s(X,[it,was,val,that,sal,looked,up]).
test(12) :- s(X,[it,was,up,"sandy's",details,that,sal,looked]). %no-yes, w/o sem
test(13) :- s(X,[it,was,up,"sandy's",nose,that,sal,looked]).   %yes
test(a13) :- s(X,[what,val,painted,was,a,picture,of,sal]).   %yes
test(b13) :- s(X,[what,val,gave,was,a,picture,of,sal]).   %yes
test(c13) :- s(X,[what,val,gave,to,sal,was,a,picture,of,sal]).
%yes (fails)
%req. proper UDC
test(d13) :- s(X,[a,picture,of,sal,was,painted]).
test(e13) :- s(X,[a,picture,of,sal,was,painted,by,val]).
test(f13) :- s(X,[a,picture,of,sal,val,painted]).
test(14) :- s(X,[sal,believed,val,hated,a,cat]). %yes
test(15) :- s(X,[sal,thought,val,hated,a,cat]).%yes
test(16) :- s(X,[sal,suggested,val,hated,a,cat]).%yes
test(17) :- s(X,[sal,thought,val,a,cat]).%yes
test(18) :- s(X,[sal,believed,val,a,cat]).%yes
test(19) :- s(X,[sal,suggested,val,a,cat]). %no
test(20) :- s(X,[sal,thought,val]). %no
test(21) :- s(X,[sal,believed,val]). %yes
test(22) :- s(X,[sal,suggested,val]). %yes
test(23) :- s(X,[sal,thought,val,to,hate,a,cat]).%yes
test(24) :- s(X,[sal,believed,val,to,hate,a,cat]). %yes
test(25) :- s(X,[sal,suggested,val,to,hate,a,cat]). %no


test(a25) :- s(X,[teri, asked, lee, hated, pharmacology]).
test(a26) :- s(X,[teri, asked, lee, an, ass]).
test(a27) :- s(X,[teri, asked, val]).
test(a28) :- s(X,[teri, asked, val, to, hate, coffee]).
test(a29) :- s(X,[teri, asked, that, lee, hated, pharmacology]).%false
test(a30) :- s(X,[teri, asked, whether, lee, hated, pharmacology]).
test(a31) :- s(X,[teri, asked, who, hated, pharmacology]).

test(b25) :- s(X,[teri, believed, lee, hated, pharmacology]).
test(b26) :- s(X,[teri, believed, lee, an, ass]).%false
test(b27) :- s(X,[teri, believed, val]).
test(b28) :- s(X,[teri, believed, val, to, hate, coffee]).
test(b29) :- s(X,[teri, believed, that, lee, hated, pharmacology]).
test(b30) :- s(X,[teri, believed, whether, lee, hated, pharmacology]).%false
test(b31) :- s(X,[teri, believed, who, hated, pharmacology]).

test(de25) :- s(X,[teri, denied, lee, hated, pharmacology]).
test(de26) :- s(X,[teri, denied, lee, an, ass]).
test(de27) :- s(X,[teri, denied, val]).
test(de28) :- s(X,[teri, denied, val, to, hate, coffee]).
test(de29) :- s(X,[teri, denied, that, lee, hated, pharmacology]).
test(de30) :- s(X,[teri, denied, whether, lee, hated, pharmacology]).
test(de31) :- s(X,[teri, denied, who, hated, pharmacology]).

test(do25) :- s(X,[teri, doubted, lee, hated, pharmacology]).
test(do26) :- s(X,[teri, doubted, lee, an, ass]).
test(do27) :- s(X,[teri, doubted, val]).
test(do28) :- s(X,[teri, doubted, val, to, hate, coffee]).
test(do29) :- s(X,[teri, doubted, that, lee, hated, pharmacology]).
test(do30) :- s(X,[teri, doubted, whether, lee, hated, pharmacology]).
test(do31) :- s(X,[teri, doubted, who, hated, pharmacology]).

test(h25) :- s(X,[teri, hated, lee, hated, pharmacology]).
test(h26) :- s(X,[teri, hated, lee, an, ass]).
test(h27) :- s(X,[teri, hated, val]).
test(h28) :- s(X,[teri, hated, val, to, hate, coffee]).
test(h29) :- s(X,[teri, hated, that, lee, hated, pharmacology]).
test(h30) :- s(X,[teri, hated, whether, lee, hated, pharmacology]).
test(h31) :- s(X,[teri, hated, who, hated, pharmacology]).


test(k25) :- s(X,[teri, knew, lee, hated, pharmacology]).
test(k26) :- s(X,[teri, knew, lee, an, ass]).
test(k27) :- s(X,[teri, knew, val]).
test(k28) :- s(X,[teri, knew, val, to, hate, coffee]).
test(k29) :- s(X,[teri, knew, that, lee, hated, pharmacology]).
test(k30) :- s(X,[teri, knew, whether, lee, hated, pharmacology]).
test(k31) :- s(X,[teri, knew, who, hated, pharmacology]).

test(l25) :- s(X,[teri, liked, lee, hated, pharmacology]).
test(l26) :- s(X,[teri, liked, lee, an, ass]).
test(l27) :- s(X,[teri, liked, val]).
test(l28) :- s(X,[teri, liked, val, to, hate, coffee]).
test(l29) :- s(X,[teri, liked, that, lee, hated, pharmacology]).
test(l30) :- s(X,[teri, liked, whether, lee, hated, pharmacology]).
test(l31) :- s(X,[teri, liked, who, hated, pharmacology]).

test(per25) :- s(X,[teri, persuaded, lee, hated, pharmacology]).
test(per26) :- s(X,[teri, persuaded, lee, an, ass]).
test(per27) :- s(X,[teri, persuaded, val]).
test(per28) :- s(X,[teri, persuaded, val, to, hate, coffee]).
test(per29) :- s(X,[teri, persuaded, that, lee, hated, pharmacology]).
test(per30) :- s(X,[teri, persuaded, whether, lee, hated, pharmacology]).
test(per31) :- s(X,[teri, persuaded, who, hated, pharmacology]).

test(pro25) :- s(X,[teri, proved, lee, hated, pharmacology]).
test(pro26) :- s(X,[teri, proved, lee, an, ass]).
test(pro27) :- s(X,[teri, proved, val]).
test(pro28) :- s(X,[teri, proved, val, to, hate, coffee]).
test(pro29) :- s(X,[teri, proved, that, lee, hated, pharmacology]).
test(pro30) :- s(X,[teri, proved, whether, lee, hated, pharmacology]).
test(pro31) :- s(X,[teri, proved, who, hated, pharmacology]).

test(q25) :- s(X,[teri, queried, lee, hated, pharmacology]).
test(q26) :- s(X,[teri, queried, lee, an, ass]).
test(q27) :- s(X,[teri, queried, val]).
test(q28) :- s(X,[teri, queried, val, to, hate, coffee]).
test(q29) :- s(X,[teri, queried, that, lee, hated, pharmacology]).
test(q30) :- s(X,[teri, queried, whether, lee, hated, pharmacology]).
test(q31) :- s(X,[teri, queried, who, hated, pharmacology]).

test(ref25) :- s(X,[teri, refuted, lee, hated, pharmacology]).
test(ref26) :- s(X,[teri, refuted, lee, an, ass]).
test(ref27) :- s(X,[teri, refuted, val]).
test(ref28) :- s(X,[teri, refuted, val, to, hate, coffee]).
test(ref29) :- s(X,[teri, refuted, that, lee, hated, pharmacology]).
test(ref30) :- s(X,[teri, refuted, whether, lee, hated, pharmacology]).
test(ref31) :- s(X,[teri, refuted, who, hated, pharmacology]).

test(reg25) :- s(X,[teri, regretted, lee, hated, pharmacology]).
test(reg26) :- s(X,[teri, regretted, lee, an, ass]).
test(reg27) :- s(X,[teri, regretted, val]).
test(reg28) :- s(X,[teri, regretted, val, to, hate, coffee]).
test(reg29) :- s(X,[teri, regretted, that, lee, hated, pharmacology]).
test(reg30) :- s(X,[teri, regretted, whether, lee, hated, pharmacology]).
test(reg31) :- s(X,[teri, regretted, who, hated, pharmacology]).

test(sa25) :- s(X,[teri, said, lee, hated, pharmacology]).
test(sa26) :- s(X,[teri, said, lee, an, ass]).
test(sa27) :- s(X,[teri, said, val]).
test(sa28) :- s(X,[teri, said, val, to, hate, coffee]).
test(sa29) :- s(X,[teri, said, that, lee, hated, pharmacology]).
test(sa30) :- s(X,[teri, said, whether, lee, hated, pharmacology]).
test(sa31) :- s(X,[teri, said, who, hated, pharmacology]).


test(su25) :- s(X,[teri, suggested, lee, hated, pharmacology]).
test(su26) :- s(X,[teri, suggested, lee, an, ass]).
test(su27) :- s(X,[teri, suggested, val]).
test(su28) :- s(X,[teri, suggested, val, to, hate, coffee]).
test(su29) :- s(X,[teri, suggested, that, lee, hated, pharmacology]).
test(su30) :- s(X,[teri, suggested, whether, lee, hated, pharmacology]).
test(su31) :- s(X,[teri, suggested, who, hated, pharmacology]).


test(t25) :- s(X,[teri, thought, lee, hated, pharmacology]).
test(t26) :- s(X,[teri, thought, lee, an, ass]).
test(t27) :- s(X,[teri, thought, val]).
test(t28) :- s(X,[teri, thought, val, to, hate, coffee]).
test(t29) :- s(X,[teri, thought, that, lee, hated, pharmacology]).
test(t30) :- s(X,[teri, thought, whether, lee, hated, pharmacology]).
test(t31) :- s(X,[teri, thought, who, hated, pharmacology]).


test(v25) :- s(X,[teri, verified, lee, hated, pharmacology]).
test(v26) :- s(X,[teri, verified, lee, an, ass]).
test(v27) :- s(X,[teri, verified, val]).
test(v28) :- s(X,[teri, verified, val, to, hate, coffee]).
test(v29) :- s(X,[teri, verified, that, lee, hated, pharmacology]).
test(v30) :- s(X,[teri, verified, whether, lee, hated, pharmacology]).
test(v31) :- s(X,[teri, verified, who, hated, pharmacology]).


test(wa25) :- s(X,[teri, wanted, lee, hated, pharmacology]).
test(wa26) :- s(X,[teri, wanted, lee, an, ass]).
test(wa27) :- s(X,[teri, wanted, val]).
test(wa28) :- s(X,[teri, wanted, val, to, hate, coffee]).
test(wa29) :- s(X,[teri, wanted, that, lee, hated, pharmacology]).
test(wa30) :- s(X,[teri, wanted, whether, lee, hated, pharmacology]).
test(wa31) :- s(X,[teri, wanted, who, hated, pharmacology]).


test(won25) :- s(X,[teri, wondered, lee, hated, pharmacology]).
test(won26) :- s(X,[teri, wondered, lee, an, ass]).
test(won27) :- s(X,[teri, wondered, val]).
test(won28) :- s(X,[teri, wondered, val, to, hate, coffee]).
test(won29) :- s(X,[teri, wondered, that, lee, hated, pharmacology]).
test(won30) :- s(X,[teri, wondered, whether, lee, hated, pharmacology]).
test(won31) :- s(X,[teri, wondered, who, hated, pharmacology]).


good([a27,a28,a30,a31,b25,b27,b28,b29,de25,de26,de29,do25,do27,do28,do29,h27,h28,h29,k25,k27,k28,k29,k30,k31,l27,l28,l29,per27,per28,pro25,pro26,pro28,pro29,pro30,q27,q30,q31,ref27,ref29,reg25,reg28,reg29,sa25,sa29,sa30,sa31,su25,su27,su28,su29,su30,su31,t25,t26,t28,t29,v25,v29,v30,v31,wa27,wa28,won30,won31]).

bad([a25,a26,a29,b26,b30,b31,de27,de28,de30,de31,do26,do30,do31,h25,h26,h30,h31,k26,l25,l26,l30,l31,per25,per26,per29,per30,per31,pro27,pro31,q25,q26,q28,q29,ref25,ref26,ref28,ref30,ref31,reg26,reg27,reg30,reg31,sa26,sa27,sa28,su26,t27,t30,t31,v26,v27,v28,wa25,wa26,wa29,wa30,wa31,won25,won26,won27,won28,won29]).


total([a25,a26,a27,a28,a29,a30,a31,b25,b26,b27,b28,b29,b30,b31,de25,de26,de27,de28,de29,de30,de31,do25,do26,do27,do28,do29,do30,do31,h25,h26,h27,h28,h29,h30,h31,k25,k26,k27,k28,k29,k30,k31,l25,l26,l27,l28,l29,l30,l31,per25,per26,per27,per28,per29,per30,per31,pro25,pro26,pro27,pro28,pro29,pro30,pro31,q25,q26,q27,q28,q29,q30,q31,ref25,ref26,ref27,ref28,ref29,ref30,ref31,reg25,reg26,reg27,reg28,reg29,reg30,reg31,sa25,sa26,sa27,sa28,sa29,sa30,sa31,su25,su26,su27,su28,su29,su30,su31,t25,t26,t27,t28,t29,t30,t31,wa25,wa26,wa27,wa28,wa29,wa30,wa31,won25,won26,won27,won28,won29,won30,won31]).


testem :-
     pass([1,2,4,5,6,8,9,10,11,12,13,a13,c13,d13,e13,f13,
           14,15,16,17,18,21,22,23,24]),
	   good(G),
	   pass(G),
	   bad(B),
     fail([a1,3,7,b13,19,20,25]),
     fail(B).

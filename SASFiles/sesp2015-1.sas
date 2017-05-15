libname SESP2015 "C:\Users\Niall\Dropbox\NIALL1\MET\Suppes Talk\SESP2015";
run;

title "SESP 2015 Analyses";
proc sort data=sesp2015.rtexpt;
by subj time trait_vale block;
run;

*\hetero\ Manuscript March 23rd 2017, rtms as dv;
title 'Fixed effect of trait_vale, time and their interaction';
title2 'Random trait valence effect (two variances and a covariance)';
title3 'RT in Milliseconds';
proc mixed data=sesp2015.rtexpt covtest noclprint;
class subj trait_vale time_e;
model rt = trait_vale time_e trait_vale*time_e/solution ddfm=kr;
random  trait_vale/subject = subj type = un g gcorr;
lsmeans trait_vale time_e trait_vale*time_e/cl;
run;


title 'Note this is the model used to output random effects';
title2 'Run showing sub-class means';
title3 'RT in Milliseconds';
proc mixed data=sesp2015.rtexpt covtest noclprint;
class subj trait_vale time_e;
model rt = time_e*trait_vale/noint solution ddfm=kr;
random  time_e*trait_vale/subject = subj type = un g gcorr solution;
ods exclude SolutionR;
ods output SolutionR = randpred;
*lsmeans trait_vale time_e trait_vale*time_e;
run;

proc means data=sesp2015.rtexpt;
var rt_log;
by subj time_e trait_vale;
output out=means mean=mean n=N;
run;


*RTlog version;
title 'Fixed effect of trait_vale, time and their interaction';
title2 'Random trait valence effect (two variances and a covariance)';
proc mixed data=sesp2015.rtexpt covtest noclprint;
class subj trait_vale time_e;
model rt_log = trait_vale time_e trait_vale*time_e/solution ddfm=kr;
random  trait_vale/subject = subj type = un g gcorr;
lsmeans trait_vale time_e trait_vale*time_e/cl;
run;


*RTlog version;
*Note this is the model used to output random effects;
title 'Note this is the model used to output random effects';
title2 'Run showing sub-class means';
title3 'RT_log version';
title4 'Run showing sub-class means';
proc mixed data=sesp2015.rtexpt covtest noclprint;
class subj trait_vale time_e;
model rt_log = time_e*trait_vale/noint solution ddfm=kr;
random  time_e*trait_vale/subject = subj type = un g gcorr solution;
ods exclude SolutionR;
ods output SolutionR = randpred;
*lsmeans trait_vale time_e trait_vale*time_e;
run;

*running as repeated measures ANOVA;
proc mixed data=sesp2015.rtexpt covtest noclprint;
class subj time_e;
model rt_log = trait_vale*time_e/noint solution ddfm=kr;
random  trait_vale*time_e/subject = subj type = un g gcorr solution;
ods exclude SolutionR;
ods output SolutionR = randpred;
*lsmeans trait_vale time_e trait_vale*time_e;
run;

data new;
set randpred;
if stderrpred ne 0;
if df ne 1;
run;

data post2;
set new;
if trait_vale = 1 and time_e = 1;
post2=estimate + 6.9032;
run;

data post1;
set new;
if trait_vale = 1 and time_e = -1;
post1=estimate + 6.9725;

data negt2;
set new;
if trait_vale = -1 and time_e = 1;
negt2=estimate + 7.1111;
run;

data negt1;
set new;
if trait_vale = -1 and time_e = -1;
negt1=estimate + 7.1087;
run;

data sesp2015.total;
merge negt1 post1 negt2 post2;
by subj;
posminusneg1 = post1-negt1;
posminusneg2 = post2-negt2;
run;


data total1;
set total;
if post1 ne .;
if negt1 ne .;
posminusneg1 = post1-negt1;
posminusneg2 = post2-negt2;
run;

proc corr data=total1 plots=scatter;
var negt1 post1 negt2 post2 posminusneg1 posminusneg2;
run;

proc corr data=total1 plots=scatter;
var posminusneg1 posminusneg2;
run;

proc sgscatter data = total1;
plot posminusneg1*posminusneg2;
run;


*******************************************;
*Original analyses;
proc univariate data=abby.rtexpt;
var rt;
by trait_vale;
run;


*checking whether Rt exclusions make a difference;
*only "yes me" rts;
data abby.yukartexclyes;
set abby.yukartexcl;
if resp = 1;
run;

*working with yukaexclmood dataset;
*only "yes me" rts;
data abby.yukaexclmoodyes;
set abby.yukaexclmood;
if resp = 1;
run;

*only block 2 rts;
data abby.yukaexclmoodyesblock2;
set abby.yukaexclmoodyes;
if block = 2;
run;

*for checking 2 way interactions;
*positive traits only;
data abby.yukaposonly;
set abby.yukaexclmoodyesblock2;
if trait_vale = 1;
run;

*for negative traits only;
data abby.yukanegonly;
set abby.yukaexclmoodyesblock2;
if trait_vale = -1;
run;

*only "no me" rts;
data abby.yukaexclmoodno;
set abby.yukaexclmood;
if resp = -1;
run;

*only "no me" rts with block 2;
data abby.yukaexclmoodnoblock2;
set abby.yukaexclmoodno;
if block = 2;
run;

*working with yukaexcl dataset;
*only "yes me" rts;
data abby.yukaexclyesonly;
set abby.yukaexcl;
if resp = 1;
run;

*only block 2 rts;
data abby.yukaexclyesblock2;
set abby.yukaexclyesonly;
if block = 2;
run;

*creating a dataset with only "yes me" rts;
data abby.yukayesonly;
set abby.yukatime1_2;
if resp = 1;
run;

*effect coding time. -1 is session 1, 1 is session2;
data abby.yukayesonly;
set abby.yukayesonly;
time_e = .;
if time = 1 then time_e = -1;
else if time = 2 then time_e = 1;
run;

*creating a dataset with only block2;
data abby.yukayesblock2;
set abby.yukayesonly;
if block = 2;
run;

proc mixed data=abby.yukayesblock2 covtest noclprint;
class subj;
model rt_log = regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale time_e*trait_vale regfocus*time_e*trait_vale / s;
random intercept time_e /subject = subj type = un g gcorr;
run;

*primary rt analysis;
proc mixed data=abby.yukaexclmoodyes covtest noclprint;
class subj;
model rt = regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale time_e*trait_vale regfocus*time_e*trait_vale / s ddf=23,23,
23,23,23,23,23;
random intercept time_e trait_vale /subject = subj type = vc g gcorr;
estimate 'pos prom time 1 to time 2' time_e 2 regfocus*time_e 2 time_e*trait_vale 2 regfocus*time_e*trait_vale 2;
estimate 'neg prom time 1 to time 2' time_e 2 regfocus*time_e 2 time_e*trait_vale -2 regfocus*time_e*trait_vale -2;
estimate 'pos prev time 1 to time 2' time_e 2 regfocus*time_e -2 time_e*trait_vale 2 regfocus*time_e*trait_vale -2;
estimate 'neg prev time 1 to time 2' time_e 2 regfocus*time_e -2 time_e*trait_vale -2 regfocus*time_e*trait_vale 2;
estimate 'time 2 prom pos vs. prev pos' regfocus 2 regfocus*time_e 2 regfocus*trait_vale 2 regfocus*time_e*trait_vale 2;
estimate 'time 2 prom neg vs. prev neg' regfocus 2 regfocus*time_e 2 regfocus*trait_vale -2 regfocus*time_e*trait_vale -2;
estimate 'time 1 prom pos vs. prev pos' regfocus 2 regfocus*time_e -2 regfocus*trait_vale 2 regfocus*time_e*trait_vale -2;
estimate 'time 1 prom neg vs. prev neg' regfocus 2 regfocus*time_e -2 regfocus*trait_vale -2 regfocus*time_e*trait_vale 2;
run;

*getting attribution data ready to analyze;
data abby.yukaattribexcl;
set abby.yukaattribexcl;
regfocus = rf;
time_e = time;
trait_vale = valence;
run;

*recoding trait_vale so that -1 = neg, 1=pos;
data abby.yukaattribexcl;
set abby.yukaattribexcl;
trait_vale = .;
if valence = 1 then trait_vale = -1;
else if valence = -1 then trait_vale = 1;
run;

*attribution of traits;
proc mixed data=abby.yukaattribexcl covtest noclprint;
class id;
model attributio = regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale time_e*trait_vale
regfocus*time_e*trait_vale / s ddf = 23, 23, 23, 23, 23, 23, 23;
random intercept trait_vale time_e/subject = id type = vc g gcorr;
estimate 'pos prom time 1 to time 2' time_e 2 regfocus*time_e 2 time_e*trait_vale 2 regfocus*time_e*trait_vale 2;
estimate 'neg prom time 1 to time 2' time_e 2 regfocus*time_e 2 time_e*trait_vale -2 regfocus*time_e*trait_vale -2;
estimate 'pos prev time 1 to time 2' time_e 2 regfocus*time_e -2 time_e*trait_vale 2 regfocus*time_e*trait_vale -2;
estimate 'neg prev time 1 to time 2' time_e 2 regfocus*time_e -2 time_e*trait_vale -2 regfocus*time_e*trait_vale 2;
estimate 'time 2 prom pos vs. prev pos' regfocus 2 regfocus*time_e 2 regfocus*trait_vale 2 regfocus*time_e*trait_vale 2;
estimate 'time 2 prom neg vs. prev neg' regfocus 2 regfocus*time_e 2 regfocus*trait_vale -2 regfocus*time_e*trait_vale -2;
estimate 'time 1 prom pos vs. prev pos' regfocus 2 regfocus*time_e -2 regfocus*trait_vale 2 regfocus*time_e*trait_vale -2;
estimate 'time 1 prom neg vs. prev neg' regfocus 2 regfocus*time_e -2 regfocus*trait_vale -2 regfocus*time_e*trait_vale 2;
run;

*running as repeated measures ANOVA - attribution of traits;
proc mixed data=abby.yukaattribexcl covtest noclprint;
class id regfocus time_e trait_vale;
model attributio = regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale time_e*trait_vale regfocus*time_e*trait_vale / s;
random intercept /subject = id type = vc g gcorr;
lsmeans regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale*time_e;
run;

*getting standard deviations;
proc sort data=abby.yukaexclmoodyes;
by trait_vale;
run;

proc means data=abby.yukaexclmoodyes;
var rt;
by trait_vale;
run;

proc sort data=abby.yukaexclmoodyes;
by time_e;
run;

proc means data=abby.yukaexclmoodyes;
var rt;
by time_e regfocus trait_vale;
run;

proc mixed data=abby.yukaexclyesblock2 covtest noclprint;
class subj;
model rt = time_e / s;
random intercept time_e /subject = subj type = un g gcorr;
run;

*running as repeated measures ANOVA - rt analysis;
proc mixed data=abby.yukaexclmoodyes covtest noclprint;
class subj regfocus time_e trait_vale;
model rt = regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale time_e*trait_vale regfocus*time_e*trait_vale / s
ddf = 23,23,23,23,23,23,23;
random intercept time_e trait_vale/subject = subj type = vc g gcorr;
lsmeans regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale*time_e;
run;

*changing type to vc and setting ddfm=kr;
*running as repeated measures ANOVA;
proc mixed data=abby.yukaexclmoodyes covtest noclprint;
class subj regfocus time_e trait_vale;
model rt = regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale time_e*trait_vale regfocus*time_e*trait_vale / s ddfm=kr;
random intercept time_e trait_vale/subject = subj type = vc g gcorr;
lsmeans regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale*time_e;
run;

proc mixed data=abby.yukaexclmoodyes covtest noclprint;
class subj regfocus time_e trait_vale;
model rt = regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale time_e*trait_vale regfocus*time_e*trait_vale / s ddf=23,23,23
,23,23,23, 23;
random intercept time_e trait_vale/subject = subj type = vc g gcorr;
lsmeans regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale*time_e;
run;


estimate 'prom pos 1 vs. prom pos 2' regfocus*time_e*trait_vale 0 0 0 0 0 -1 0 1;
run;

*including mood as a covariate;
proc mixed data=abby.yukaexclmoodyesblock2 covtest noclprint;
class subj regfocus time_e trait_vale;
model rt = regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale time_e*trait_vale regfocus*time_e*trait_vale mood_prom
mood_prev / s;
random intercept time_e /subject = subj type = un g gcorr;
lsmeans regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale*time_e;
run;

*including mood and self-esteem as covariates;
proc mixed data=abby.yukaexclmoodyesblock2 covtest noclprint;
class subj regfocus time_e trait_vale;
model rt = regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale time_e*trait_vale regfocus*time_e*trait_vale mood_prom
mood_prev rse / s;
random intercept time_e /subject = subj type = un g gcorr;
lsmeans regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale*time_e;
run;

*testing 2-way interactions;
proc mixed data=abby.yukanegonly covtest noclprint;
class subj regfocus time_e;
model rt = regfocus time_e regfocus*time_e / s;
random intercept time_e /subject = subj type = un g gcorr;
lsmeans regfocus time_e regfocus*time_e;
run;

*testing "no me" responses;
proc mixed data=abby.yukaexclmoodnoblock2 covtest noclprint;
class subj regfocus time_e trait_vale;
model rt = regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale time_e*trait_vale regfocus*time_e*trait_vale / s;
random intercept time_e /subject = subj type = un g gcorr;
lsmeans regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale*time_e;
run;

*testing self-attribution of traits;
proc mixed data=abby.yukaexclmood covtest noclprint;
class subj regfocus time_e trait_vale;
model resp = regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale time_e*trait_vale regfocus*time_e*trait_vale / s;
random intercept time_e /subject = subj type = un g gcorr;
lsmeans regfocus time_e trait_vale regfocus*time_e regfocus*trait_vale*time_e;
run;

#libname SESP2015 "C:\Users\Niall\Dropbox\NIALL1\MET\Suppes Talk\SESP2015";
libname hetero "C:\Users\Niall\Documents\KZNB";
libname hetero "C:\Users\Niall\Documents\KZNB\SASFiles";

run;

*2017-05-17: Latest Work on Yuka dataset to match R;

*2017-03-17;
* Written by R;
*  write.foreign(randata, "C:/Users/Niall/Documents/KZNB/randata.txt",  ;

DATA  randata ;
INFILE  "C:/Users/Niall/Documents/KZNB/randata.txt" 
     DSD 
     LRECL= 92 ;
INPUT
 X
 Experiment
 SubjectNumber
 ItemNumber
 RT
 Acc
 ExpressionValance
 Wrd1Valance
 Wrd2Valance
 NumOfCharecters
 Valance
 val
 item
 RTpred
;
RUN;

data hetero.randata;
set randata;
vald = val + 0.5; *Dummy for pos valence;
run;

*Random slope;
proc mixed data=hetero.randata covtest noclprint cl;
class SubjectNumber;
model RT = val/s;
random intercept val/subject=SubjectNumber type=un g gcorr s;
estimate 'NEG mean' intercept 1 val -0.5;
estimate 'POS mean' intercept 1 val .5;
estimate 'NEG minus POS' val 1;
run;

*Random intercept;
proc mixed data=hetero.randata covtest noclprint cl;
class SubjectNumber;
model RT = val/s;
random intercept/subject=SubjectNumber type=un g gcorr s;
estimate 'NEG mean' intercept 1 val -0.5;
estimate 'POS mean' intercept 1 val .5;
estimate 'NEG minus POS' val 1;
run;

proc means data=hetero.randata;
var RT;
by SubjectNumber;
run;

*Only three or four were significant out of 30;
proc reg data=hetero.randata;
model RT=val;
by SubjectNumber;
run;


*2017-feb-26;
*Runs to look at balanced dataset and get EMS;
run;
data hetero.rndt2;
set hetero.rndt2;
run;
proc glm data=hetero.rndt2;
class idcat valenceE;
model logrt = valenceE idcat valenceE*idcat/ss3;
random valenceE*idcat/test;
means valenceE;
run;

proc glm data=hetero.aggbalanced;
class id valenceE;
model logrt = valenceE id/ss3;
lsmeans valenceE/stderr pdiff;
run;


run;
proc glm data=hetero.rndt2;
class idcat;
model logrt = valence2 idcat valence2(idcat)/ss3;
*random valenceE(idcat)/test;
run;

proc mixed data=hetero.rndt2 covtest noclprint cl;
class idcat;
model logrt = valenceE/s;
random intercept valenceE/subject=idcat type=un g gcorr s;
estimate 'NEG mean' intercept 1 valenceE -0.5;
estimate 'POS mean' intercept 1 valenceE .5;
estimate 'NEG minus POS' valenceE 1;
run;

proc mixed data=hetero.rndt2 covtest noclprint cl;
class idcat;
model logrt = valence2/s;
random intercept valence2/subject=idcat type=un g gcorr s;
estimate 'NEG mean' intercept 1 valence2 -1;
estimate 'POS mean' intercept 1 valence2 1;
estimate 'NEG minus POS' valence2 2;
run;

*Repeated measures analysis for aggregated data in wide form;
*Aggregated data in wide form;
title 'Aggregated data in wide form';
title2;
title3;
proc glm data=hetero.aggs1aawide;
model rtneg rtpos = /nouni;
repeated valence 2/printe printm;
run;
 



*Runs for Heterogeneity Paper;
*Note: totalrt.R take the results of these analyses and creates plots;
*Look at Trait codes;
proc freq data=sesp2015.rtexpt;
tables trial
	trl1 
	trait_vale*trial
	trait_vale*trl1;
run;

title 'Fixed effect of trait_vale, time and their interaction';
title2 'Random trait valence effect (two variances and a covariance)';
title3 'Random effect of trait type within trait valence';
proc mixed data=sesp2015.rtexpt covtest noclprint;
class subj trait_vale time_e trl1;
model rt_log = trait_vale time_e trait_vale*time_e/solution ddfm=contain;
random  trait_vale/subject = subj type = un g gcorr;
random trait_vale/subject = trl1(trait_vale); *a shot in the dark;*should be intercept/subject = trl1;
lsmeans trait_vale time_e trait_vale*time_e/cl;
estimate "trait vale overall" trait_vale -1 1/ cl;
run;

*Go back to dataset with just time2;
libname suppes "C:\Users\Niall\Dropbox\NIALL1\MET\Suppes Talk";
run;

proc sort data=suppes.rtexpt;
by time_e subj block trial;
run;

data suppes.rtexpttwo;
set suppes.rtexpt1;
if subj ne 9;
if subj ne 15;
if time=2;
if filter__=1;
if rt le 5000;
run;


*Need to run PROC MIXED for time=2 only;

*+++++++++++++++++++++++++;
*rm analysis of aggregated data; 
proc sort data=suppes.rtexpttwo;
by subj trait_vale;
run;

*Create aggregated data by id, conflict;
proc means data=suppes.rtexpttwo;
var rt_log;
by subj trait_vale;
output out=suppes.outrtrm mean=mrt_log;
run;

*Aggregated data in long form;
title 'Aggregated data in long form';
title2;
title3;
proc mixed data=suppes.outrtrm covtest noclprint cl;
class subj;
model mrt_log = trait_vale/s;
random intercept/subject=subj type=un g gcorr s;
estimate 'NEG mean' intercept 1 trait_vale -1;
estimate 'POS mean' intercept 1 trait_vale 1;
estimate 'NEG minus POS' trait_vale 2;
run;


*Aggregated data in wide form;
title 'Aggregated data in wide form';
title2;
title3;
proc corr data=suppes.rmrt;
var mrt_log_1 mrt_log_2;
run;

*Aggregated data in wide form;
title 'Aggregated data in wide form';
title2;
title3;
proc glm data=suppes.rmrt;
model mrt_log_1 mrt_log_2 = ;
repeated valence 2;
run;
 
*Run for Heterogeneity paper, Results match R (May 15, 2017);
*primary analysis, but no regfocus, same dataset as used in R;
title 'Raw data in long form';
title2 'Time 2 data, no regfocus, 		';
title3;
proc mixed data=suppes.rtexpttwo covtest noclprint cl;
class subj;
model rt_log = trait_vale/s ddfm=satterthwaite;
random intercept trait_vale/subject=subj type=un g gcorr s;
ods exclude SolutionR;
ods output SolutionR = rtrand;
run;

*For brown bag on 09-18-2016;
*Run with trl1 as random effect;
*primary analysis, but no regfocus, same dataset as used in R;
title 'Raw data in long form';
title2 'Time 2 data, no regfocus, 		';
title3 'Specify stimuli as random';
proc mixed data=suppes.rtexpttwo covtest noclprint;
class subj trl1;
model rt_log = trait_vale/s;
random intercept trait_vale/subject=subj type=un g gcorr s;
random intercept/subject=trl1 type=un g gcorr s;
ods exclude SolutionR;
ods output SolutionR = rtrand;
run;


*For brown bag on 09-18-2016;
*Run with trl1 as random effect;
*primary analysis, but no regfocus, same dataset as used in R;
title 'Raw data in long form';
title2 'Time 2 data, no regfocus, 		';
title3 'Specify stimuli as random';
title4 'Bayesian estimation';
proc mixed data=suppes.rtexpttwo covtest noclprint;
class subj trl1;
model rt_log = trait_vale/s;
prior jeffreys/out=bmixed;
random intercept trait_vale/subject=subj type=vc g gcorr s;
random intercept/subject=trl1 type=vc g gcorr s;
ods exclude SolutionR;
ods output SolutionR = rtrand;
run;

proc sgplot data=bmixed;
histogram beta2;
 
proc sgplot data=bmixed;
histogram covp2;
run;


*For brown bag on 09-18-2016;
*Run with trl1 as random effect;
*primary analysis, but no regfocus, same dataset as used in R;
title 'Raw data in long form';
title2 'Time 2 data, no regfocus, 		';
title3 'Specify stimuli as random, but keep valence fixed';
proc mixed data=suppes.rtexpttwo covtest noclprint;
class subj trl1;
model rt_log = trait_vale/s;
random intercept/subject=subj type=un g gcorr;
random intercept/subject=trl1 type=un g gcorr;
ods exclude SolutionR;
ods output SolutionR = rtrand;
run;




data ch5.outconfrm;
set ch5.outconfrm;
drop _TYPE_ _FREQ_;
run;

proc sort data=ch5.conflict;
by id time;
run;
data betw;
set ch5.conflict;
if time=0;
keep id relqual conflictbc;
run;

data ch5.confrm2l;
 merge ch5.outconfrm betw;
 by id;
 run;

data ch5.confrm2l;
set ch5.confrm2l;
*Create effect-coded conflict to compare with aggregated rm results;
conf=.;
if conflict=0 then conf=-0.5;
else if conflict=1 then conf=0.5;
run;





title "SESP 2015 Analyses";
proc sort data=sesp2015.rtexpt;
by subj time block;
run;


title 'Fixed effect of trait_vale, time and their interaction';
title2 'Random trait valence effect (two variances and a covariance)';
proc mixed data=sesp2015.rtexpt covtest noclprint;
class subj trait_vale time_e;
model rt_log = trait_vale time_e trait_vale*time_e/solution ddfm=kr;
random  trait_vale/subject = subj type = un g gcorr;
lsmeans trait_vale time_e trait_vale*time_e/cl;
run;

*Random int and trait_vale: both time points;
proc mixed data=sesp2015.rtexpt covtest noclprint;
class subj trait_vale time_e;
model rt_log = trait_vale time_e trait_vale*time_e/solution ddfm=kr;
random  trait_vale/subject = subj type = un g gcorr;
lsmeans trait_vale time_e trait_vale*time_e;
run;



*Note this is the model used to output random effects;
*Run showing sub-class means;
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

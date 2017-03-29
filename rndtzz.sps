* Encoding: UTF-8.


GET DATA  /TYPE=TXT
  /FILE="C:\Users\Niall\Documents\KZNB\rndtzz.csv"
  /ENCODING='Locale'
  /DELCASE=LINE
  /DELIMITERS=","
  /QUALIFIER='"'
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  ID_A F3.0
  pracword A2
  valence A8
  word A12
  trials_2.thisRepN A2
  trials_2.thisTrialN A2
  trials_2.thisN A2
  trials_2.thisIndex A2
  trials.thisRepN F1.0
  trials.thisTrialN F2.0
  trials.thisN F2.0
  trials.thisIndex F2.0
  key_resp_3.keys A2
  key_resp_3.rt A2
  key_resp_4.keys A2
  key_resp_4.rt A2
  response.keys A2
  response.corr F1.0
  response.rt F11.9
  date A16
  frameRate F11.8
  expName A20
  session F1.0
  participant F3.0
  trial40 F2.0
  trial40c F6.3
  rt F11.9
  logrt F16.14
  id F3.0
  valenceE F4.1
  rtms F11.6
  rt.z A20.
CACHE.
EXECUTE.
DATASET NAME DataSet2 WINDOW=FRONT.



#Minimal model for valence effect using rndtzz dataset.
#First without temporal ordering.
MIXED logrt WITH valenceE 
  /FIXED=valenceE | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(id) COVTYPE(UN).

  /SAVE = FIXPRED(fixed) PRED(blup).
 * /REPEATED=time | SUBJECT(id) COVTYPE(AR1).

*Model with no random slope;
MIXED logrt WITH valenceE 
  /FIXED=valenceE | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT valenceE | SUBJECT(id) COVTYPE(UN)
  /SAVE = FIXPRED(fixed) PRED(blup).


MIXED logrt WITH valenceE trial40c
  /FIXED=valenceE trial40c | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT valenceE | SUBJECT(id) COVTYPE(UN).
 * /REPEATED=time | SUBJECT(id) COVTYPE(AR1).

#MODEL USING ML TO COMPARE WITH MPLUS;

MIXED intimacy WITH time01 treatment
  /FIXED=time01 treatment time01*treatment | SSTYPE(3)
  /METHOD=ML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT time01 | SUBJECT(id) COVTYPE(UN).

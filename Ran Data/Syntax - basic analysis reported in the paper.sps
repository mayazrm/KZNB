* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(subjective = 1 & OTRate <= 0.6).
VARIABLE LABELS filter_$ 'subjective = 1 & OTRate <= 0.6 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

SORT CASES  BY Experiment.
SPLIT FILE LAYERED BY Experiment.

GLM SubCon SubNon BY presentation_duration
  /WSFACTOR=cong 2 Polynomial 
  /METHOD=SSTYPE(3)
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=cong 
  /DESIGN=presentation_duration.



*Import experiment 6.
GET DATA  /TYPE=TXT
  /FILE="C:\Users\Niall\Dropbox\NIALL1\MET\Suppes Talk\Ran "+
    "Hassin\fwdfilesforniel\exp6_all_data_long.csv"
  /ENCODING='Locale'
  /DELCASE=LINE
  /DELIMITERS=","
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  prim F2.0
  Prime.result F2.0
  target F2.0
  congruent A3
  operand A1
  distance F2.0
  subject F1.0
  rt A4
  OT F11.9
  subjective.0.is.unaware F1.0
  presentaition_time F4.0
  addition.place F1.0
  counter.balance F1.0
  exclude_from_analysis A5.
CACHE.
EXECUTE.
DATASET NAME DataSet2 WINDOW=FRONT.

DATASET ACTIVATE DataSet2.
USE ALL.
COMPUTE filter_$=(exclude_from_analysis = 'FALSE' & operand = 'S').
VARIABLE LABELS filter_$ "exclude_from_analysis = 'FALSE' & operand = 'S' (FILTER)".
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*Exp6: Model with rt = f(congrue).
DATASET ACTIVATE DataSet2.
MIXED rt BY congruent presentation_time
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=congruent presentation_time congruent*presentation_time | SSTYPE(3)
  /METHOD=REML
  /PRINT=DESCRIPTIVES G  SOLUTION TESTCOV
 /RANDOM=INTERCEPT congruent | SUBJECT(subject) COVTYPE(VC).

 *Try random stimulus.
*Exp6: Model with rt = f(congrue, stimulus).
DATASET ACTIVATE DataSet2.
MIXED rt BY congruent presentation_time
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=congruent presentation_time congruent*presentation_time | SSTYPE(3)
  /METHOD=REML
  /PRINT=DESCRIPTIVES G  SOLUTION TESTCOV
 /RANDOM=INTERCEPT | SUBJECT(subject) COVTYPE(VC)
  /RANDOM=INTERCEPT congruent | SUBJECT(prim) COVTYPE(UN).



 


GET DATA  /TYPE=TXT
  /FILE="C:\Users\Niall\Dropbox\NIALL1\MET\Suppes Talk\Ran "+
    "Hassin\fwdfilesforniel\exp7_all_data_long.csv"
  /ENCODING='Locale'
  /DELCASE=LINE
  /DELIMITERS=","
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  prime A6
  prime.result F2.0
  distance F2.0
  target F2.0
  operation A1
  congruent A3
  counterbalance F1.0
  subject F1.0
  rt F4.0
  presentation.duration F4.0
  subjective.test..1.indicates.that.the.subject.was.not.aware.of.p F1.0
  objective.test.score..percent.correct..chance.level.is.0.5 F11.9
  Exclude_from_analysis A5.
CACHE.
EXECUTE.
DATASET NAME DataSet3 WINDOW=FRONT.


*new filter.
DATASET ACTIVATE DataSet3.
USE ALL.
COMPUTE filter_$=(subjective = 1 & OTRate <= 0.6 & operation = 'S').
VARIABLE LABELS filter_$ 'subjective = 1 & OTRate <= 0.6 & operation = S (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*Exp7: Model with rt = f(congrue).
DATASET ACTIVATE DataSet3.
MIXED rt BY congruent duration
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=congruent duration  congruent*duration | SSTYPE(3)
  /METHOD=REML
  /PRINT=DESCRIPTIVES G  SOLUTION TESTCOV
 /RANDOM=INTERCEPT congruent| SUBJECT(subject) COVTYPE(VC).




*Exp7: Model with rt = f(congrue, stim).
DATASET ACTIVATE DataSet3.
MIXED rt BY congruent duration
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=congruent duration  congruent*duration | SSTYPE(3)
  /METHOD=REML
  /PRINT=DESCRIPTIVES G  SOLUTION TESTCOV
 /RANDOM=INTERCEPT congruent| SUBJECT(subject) COVTYPE(VC)
  /RANDOM=INTERCEPT | SUBJECT(prime) COVTYPE(VC).




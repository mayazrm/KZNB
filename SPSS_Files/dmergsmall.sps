* Encoding: windows-1252.
DATA LIST FILE= "C:/Users/Niall/Documents/KZNB/dmergsmall.txt"  free (",")
/ id rtms logrt valenceE prom.c prev.c  .

VARIABLE LABELS
id "id" 
 rtms "rtms" 
 logrt "logrt" 
 valenceE "valenceE" 
 prom.c "prom.c" 
 prev.c "prev.c" 
 .
EXECUTE.
*RT in milliseconds.
MIXED rtms WITH valenceE prom.c prev.c
  /FIXED=valenceE prom.c  prev.c valenceE*prom.c valenceE*prev.c prom.c*prev.c 
                 valenceE*prom.c*prev.c | SSTYPE(3)
  /METHOD=REML
  /PRINT=DESCRIPTIVES G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT valenceE | SUBJECT(id) COVTYPE(UN).

EXECUTE.

*RT in log milliseconds.
MIXED logrt WITH valenceE prom.c prev.c
  /FIXED=valenceE prom.c  prev.c valenceE*prom.c valenceE*prev.c prom.c*prev.c 
                 valenceE*prom.c*prev.c | SSTYPE(3)
  /METHOD=REML
  /PRINT=DESCRIPTIVES G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT valenceE | SUBJECT(id) COVTYPE(UN).

EXECUTE.

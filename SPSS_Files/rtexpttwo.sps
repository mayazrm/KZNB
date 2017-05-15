* Encoding: UTF-8.
  *Run up to date model with Yuka dataset/=.
 
MIXED rt_log WITH trait_vale 
  /FIXED=trait_vale | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT trait_vale | SUBJECT(subj) COVTYPE(UN).

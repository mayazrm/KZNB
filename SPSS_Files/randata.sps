* Encoding: windows-1252.
DATA LIST FILE= "C:/Users/Niall/Documents/KZNB/randata.txt"  free (",")
/ X Expermnt SbjctNmb ItemNmbr RT Acc ExprssnV Wrd1Vlnc Wrd2Vlnc NmOfChrc Valance val item RTpred  .

VARIABLE LABELS
X "X" 
 Expermnt "Experiment" 
 SbjctNmb "SubjectNumber" 
 ItemNmbr "ItemNumber" 
 RT "RT" 
 Acc "Acc" 
 ExprssnV "ExpressionValance" 
 Wrd1Vlnc "Wrd1Valance" 
 Wrd2Vlnc "Wrd2Valance" 
 NmOfChrc "NumOfCharecters" 
 Valance "Valance" 
 val "val" 
 item "item" 
 RTpred "RTpred" 
 .
EXECUTE.

MIXED RT WITH val
  /FIXED=val | SSTYPE(3)
  /METHOD=REML
  /PRINT=G  SOLUTION TESTCOV
  /RANDOM=INTERCEPT val | SUBJECT(SbjctNmb) COVTYPE(UN).
 * /REPEATED=time | SUBJECT(id) COVTYPE(AR1).
EXECUTE.


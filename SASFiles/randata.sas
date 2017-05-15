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

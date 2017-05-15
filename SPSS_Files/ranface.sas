libname hetero "C:\Users\Niall\Documents\KZNB";
run;

title 'Ran Face Orientation Experiment';
proc mixed data=hetero.ranface covtest noclprint cl;
class participant;
model rt = fo1 fo2/solution ddfm=bw;
random  intercept fo1 fo2/subject = participant type = un g gcorr;
*lsmeans trait_vale time_e trait_vale*time_e/cl;
run;


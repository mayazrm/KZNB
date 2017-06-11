* Encoding: UTF-8.
GET DATA  /TYPE=TXT 
  /FILE="C:\Users\Niall\Documents\KZNB\aggs1aawide.csv" 
  /ENCODING='Locale' 
  /DELCASE=LINE 
  /DELIMITERS="," 
  /QUALIFIER='"' 
  /ARRANGEMENT=DELIMITED 
  /FIRSTCASE=2 
  /IMPORTCASE=ALL 
  /VARIABLES= 
  id F3.0 
  rtneg F16.14 
  rtpos F16.14. 
CACHE. 
EXECUTE. 
DATASET NAME DataSet1 WINDOW=FRONT. 
 
SAVE OUTFILE='C:\Users\Niall\Documents\KZNB\aggs1aawide.sav' 
  /COMPRESSED. 
DATASET ACTIVATE DataSet1. 
GLM rtneg rtpos 
  /WSFACTOR=valence 2 Polynomial 
  /METHOD=SSTYPE(3) 
  /PLOT=PROFILE(valence) 
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(valence) 
  /PRINT=PARAMETER RSSCP GEF 
  /CRITERIA=ALPHA(.05) 
  /WSDESIGN=valence.
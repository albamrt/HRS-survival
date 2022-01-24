
********************************************************************************;
*********** LONGITUDINAL + FINAL EVENT (dead/transplant/no transplant);
********************************************************************************;

********************************************;
***************** PREDICT ******************;
%inc "Y:\102_PREDICT\PROGRAMS\PREDICT - SETUP.sas";

*************** IMPORT CSVs; 
proc import datafile=".../predict-aclara-diagnostic-visit.xlsx"
     out = diagnostic_data
     dbms = xlsx replace;
     getnames = yes;
run;

data psubjects;
set diagnostic_data;
if cohort = 'PREDICT';
keep idpatient diagnostic_visit diagnostic_visitdt diagnostic_creatinine diagnostic_urine diagnostic_total_ml cohort;
run;

*************** ADD VISITS; 
* visits datset as long;
data visits; set statroot.visits; keep idpatient visit_dt_:; run;

proc transpose data = visits out = visits;
   by idpatient;
run;

data visits;
  length visit $4.;
  set visits (rename = (col1 = visitdt));
  visit = substr(_name_, 10);
  drop _name_ _LABEL_;
run; 

data visits;
set visits;
if visitdt = . then delete;
run;
* add dates to predict dataset;
proc sort data=visits; by IDPATIENT; run;
proc sort data=psubjects; by IDPATIENT; run;
data predict;
merge
	psubjects(in = a)
	visits;
by Idpatient;
if a;
run;

*************** ADD DEMOGRAPHICS, MEDICAL HISTORY, MORTALITY;

*SUBJECTS;
proc sort data=predict; by idpatient; run;
*DEMOGRAPHICS;
proc sort data=statroot.Dm; by idpatient; run; 
*MEDICAL HISTORY;
proc sort data=statroot.mh; by idpatient; run;
*MORTALITY;
proc sort data=statroot.txmort; by idpatient; run;

data predict;
merge
predict (in=a)
statroot.Dm (keep = idpatient age sex)
statroot.mh (keep = idpatient etciralc etcirhepc etcirhepb etcirnafld etcircryp etciroth etunknown)
statroot.txmort (keep = idpatient mort_28dM mort_90dM LT2 LTfree_dt); 
by idpatient;
if a;
run;

*************** ADD PHYSICAL EXAMINATION, LABS, SCORES, CLINFEAT, TREATMENT;

* TRANSPOSE PHYSICAL EXAMINATION;
data physexam;
set statroot.physexam (keep = idpatient map_: hr_:); 
run;

proc transpose data = physexam out = physexam;
   by idpatient;
run;

data physexam;
  length visit $4.;
  set work.physexam;
  variable = scan(_name_, 1, '_');
  visit = scan(_name_, 2, '_');
  drop _name_ _LABEL_;
run; 

proc sort data=physexam; by idpatient visit; run;
proc transpose data=physexam out=physexam;
    by idpatient visit;
    id variable;
    var col1;
run;

* TRANSPOSE LABS;
data labs;
set statroot.labs; 
run;

proc transpose data = labs out = labs;
   by idpatient;
run;

data labs;
  length visit $4.;
  set work.labs;
  variable = scan(_name_, 1, '_');
  visit = scan(_name_, 2, '_');
  drop _name_ _LABEL_;
run; 

data labs;
set labs;
if visit = 'dt' then delete;
run;

proc sort data=labs; by idpatient visit; run;
proc transpose data=labs out=labs;
    by idpatient visit;
    id variable;
    var col1;
run;

* TRANSPOSE SCORES;
data scores;
set statroot.scores; 
run;

proc transpose data = scores out = scores;
   by idpatient;
run;

data scores;
  length visit $4.;
  set work.scores;
  variable = scan(_name_, 1, '_');
  visit = scan(_name_, 2, '_');
  drop _name_ _LABEL_;
run; 

proc sort data=scores; by idpatient visit; run;
proc transpose data=scores out=scores;
    by idpatient visit;
    id variable;
    var col1;
run;

* TRANSPOSE CLINFEAT;
data clinfeat;
set statroot.clinfeat; 
run;

proc transpose data = clinfeat out = clinfeat;
   by idpatient;
run;

data clinfeat;
  set work.clinfeat;
  visit = scan(_name_, -1, '_');
  variable = substr(_name_, 1 , length(_name_)-length(visit)-1);
  drop _name_ _LABEL_;
run; 

proc sort data=clinfeat; by idpatient visit; run;
proc transpose data=clinfeat out=clinfeat;
    by idpatient visit;
    id variable;
    var col1;
run;

proc freq data=clinfeat;
   tables visit;
run;

data clinfeat;
set clinfeat;
if visit in ('AW0' 'AW1' 'HA' 'IN' 'R1' 'R2' 'R3' 'R4' 'R5' 'W1' 'W12' 'W4' 'W8');
run;


* TRANSPOSE TREATMENT;
data treatment;
set statroot.tr; 
run;

proc transpose data = treatment out = treatment;
   by idpatient;
run;

data treatment;
  set work.treatment;
  visit = scan(_name_, -1, '_');
  variable = substr(_name_, 1 , length(_name_)-length(visit)-1);
  drop _name_ _LABEL_;
run; 

proc sort data=treatment; by idpatient visit; run;
proc transpose data=treatment out=treatment;
    by idpatient visit;
    id variable;
    var col1;
run;

*MERGE;
proc sort data=predict; by idpatient visit; run;
proc sort data=work.physexam; by idpatient visit;  run; 
proc sort data=work.labs; by idpatient visit;  run; *missing SPO2 PAFIO2 SPFIO2;
proc sort data=work.scores; by idpatient visit; run; *missing CLIFOF(instead clifcof), LIVERFLR(instead liverfail), LIVERDOF(liversc), RENALFLR(renalfail), 
*RENALDOF(renalsc), CEREBFLR(cerebfail), CEREBDOF(cerebsc), COAGFLR(coagfail), COAGDOF(coagsc), CARDIFLR(cardiofail), CARDIDOF(cardiosc);
*RESPFLR(respfail), RESPDOF(respsc), CLIFAD(clifcad);
proc sort data=work.clinfeat; by idpatient visit; run;
proc sort data=work.treatment; by idpatient visit; run;

data predict;
merge
predict (in=a)
work.physexam (keep = idpatient visit map hr) 
work.labs (keep = idpatient visit wbc lym mono neut lympc monopc imneut crp alb hgb INR bili creat na chol PAO2 PACO2) 
work.scores (keep = idpatient visit aclfyn aclfgr meld child clifcaclf clifcof liverfail liversc renalfail renalsc cerebfail cerebsc coagfail coagsc cardiofail cardiosc respfail respsc clifcad)
work.clinfeat (keep = idpatient visit asci he gblee binf)
work.treatment (keep = idpatient visit tr_alb tr_vasop);
by idpatient visit; 
if a;
run;

*************** REMOVE EMPTY ROWS;
data predict;
set predict;
if visit = 'SCR' or visit = 'FU3' or visit = 'FU6' or visit = 'FU12' then delete;
run;

*************** EXPORT;
proc export data = predict
    outfile=".../predict-longitudinal-no-filter.xlsx"
    dbms=xlsx  REPLACE;
run;

********************************************;
***************** ACLARA *******************;
%inc "Y:\114_ACLARA\PROGRAMS\ACLARA - SETUP.sas";

*************** IMPORT CSVs; 
proc import datafile=".../predict-aclara-diagnostic-visit.xlsx"
     out = diagnostic_data
     dbms = xlsx replace;
     getnames = yes;
run;

data asubjects;
set diagnostic_data;
if cohort = 'ACLARA';
keep idpatient diagnostic_visit diagnostic_visitdt diagnostic_creatinine diagnostic_urine diagnostic_total_ml cohort;
run;

*************** ADD VISITS; 
* add dates to aclara dataset;
data visits;
set statsas.vd (keep = idpatient visit visitfl_dt);
if visit = 1 then delete; *delete screening;
if visit = 7 then delete; *delete Study termination;
run;

proc sort data=visits; by IDPATIENT; run;
proc sort data=asubjects; by IDPATIENT; run;
data aclara;
merge
	asubjects(in = a)
	visits;
by Idpatient;
if a;
run;

*************** ADD DEMOGRAPHICS, MEDICAL HISTORY, MORTALITY;
*SUBJECTS;
proc sort data=aclara; by idpatient; run;
*DEMOGRAPHICS;
proc sort data=statsas.Dm; by idpatient; run; 
*MEDICAL HISTORY;
proc sort data=statsas.mh; by idpatient visit; run;
*MORTALITY;
proc sort data=statsas.txmortvars; by idpatient; run;

data aclara;
merge
aclara (in=a)
statsas.Dm (keep = idpatient age sex)
statsas.mh (keep = idpatient etciralc etcirhepc etcirhepb etcirhepd etcirnafld etcircryp etcirbili etcirscle etcirah etcirwil etcirhemo etciroth etunknown)
statsas.txmortvars (keep = idpatient mort_28d mort_90d surv mort_dt lt_dt fupfin_dt); 
by idpatient;
if a;
run;

*************** ADD PHYSICAL EXAMINATION, LABS, SCORES, CLINFEAT, TREATMENT;
proc sort data=aclara; by IDPATIENT visit; run;
proc sort data=statsas.lb; by IDPATIENT visit; run;
proc sort data=statsas.tr; by IDPATIENT visit; run;
proc sort data=statsas.sc; by idpatient visit; run; *missing CLIFOF(instead clifcof), LIVERFLR(instead cof_liverfail_db), LIVERDOF(cof_liversc_db), RENALFLR(cof_renalfail_db), 
*RENALDOF (cof_renalsc_db), CEREBFLR(cof_cerebfail_db), CEREBDOF(cof_cerebsc_db), COAGFLR(cof_coagfail_db), COAGDOF(cof_coagsc_db), CARDIFLR(cof_cardiofail_db), CARDIDOF(cof_cardiosc_db);
*RESPFLR(cof_respfail_db), RESPDOF(), CLIFAD();
*cof_liversc_db = 1, 2, 3;
proc sort data=statsas.cf; by idpatient visit; run;
proc sort data=statsas.pe; by idpatient visit; run;

data aclara;
merge
	aclara(in = a)
	statsas.tr(keep = idpatient visit tr_alb tr_vasop)
	statsas.pe (keep = idpatient visit map hr)
	statsas.lb (keep = idpatient visit wbc lym mono neut lympc monopc imneut crp alb hgb INR bili creat na chol PAO2 PACO2) 
	statsas.sc (keep = idpatient visit aclfyn aclfgr meld child clifcaclf clifcof cof_liverfail_db cof_liversc_db cof_renalfail_db cof_renalsc_db cof_cerebfail_db cof_cerebsc_db cof_coagfail_db cof_coagsc_db cof_cardiofail_db cof_cardiosc_db cof_respfail_db cof_respsc_db clifcad)
	statsas.cf (keep = idpatient visit asci_v he_v gblee binf);
by Idpatient visit;
if a;
run;
/*
data aclara;
set aclara;
Albumin = input(tr_alb, 12.);
Vasopressors = input(tr_vasop, 12.);
run;
*/

*************** ADD DATE OF FINAL EVENT;
data aclara;
set aclara(rename=(surv = LT2));
format LTfree_dt DATE9.;
if LT2 = 1 then LTfree_dt = mort_dt;
else if LT2 = 2 then LTfree_dt = lt_dt;
else if LT2 = 0 then LTfree_dt = fupfin_dt;
run;

*************** VISIT TO CODE;
data aclara;
set aclara;
rename visit = visit_num;
run;

proc freq data=aclara;
   tables visit_num ;
run;

data aclara;
set aclara;
format visit $4.;
if visit_num = 2 then visit = 'IN';
if visit_num = 3 then visit = 'D8';
if visit_num = 4 then visit = 'AD0';
if visit_num = 5 then visit = 'AD8';
if visit_num = 6 then visit = 'EOS';
if visit_num = 7 then visit = 'ST';
if visit_num = 8 then visit = 'FU';
if visit_num = 9 then visit = 'FU';
if visit_num = 10 then visit = 'FU';
if visit_num = 11 then visit = 'FU';
run;

data aclara;
set aclara(drop = visit_num);
*tr_alb tr_vasop);
run;

*************** REMOVE EMPTY ROWS;
data aclara;
set aclara;
if visit = 'FU' then delete;
run;

*************** EXPORT;
proc export data=aclara
    outfile=".../aclara-longitudinal-no-filter.xlsx"
    dbms=xlsx  REPLACE;
run;


*************************************************************;
******************** MERGE PREDICT & ACLARA *****************;
*************************************************************;

proc datasets library=WORK kill; run; quit;

*************** IMPORT CSVs; 
proc import datafile=".../predict-longitudinal-no-filter.xlsx"
     out=predict
     dbms=xlsx replace;
     getnames=yes;
run;

proc import datafile=".../aclara-longitudinal-no-filter.xlsx"
     out=aclara
     dbms=xlsx replace;
     getnames=yes;
run;

* change format;
/*data predict;
  set predict;
  aux_vasop = input(vasopressors, 12.);
  aux_alb = input(albumin, 12.);
  
  if LT2 = 0 then aux_LT2 = 'No Transplanted';
  else if LT2 = 1 then aux_LT2 = 'Dead before LT';
  else if LT2 = 2 then aux_LT2 = 'Transplanted';
  drop vasopressors albumin LT2;
  rename aux_vasop = Vasopressors aux_alb = Albumin aux_LT2 = LT2;
run;
*/
data aclara;
  set aclara;
  rename visitfl_dt = visitdt asci_v = asci he_v = he mort_28d = mort_28dM mort_90d = mort_90dM 
cof_liversc_db = liversc cof_liverfail_db = liverfail cof_renalsc_db = renalsc  cof_renalfail_db = renalfail cof_cerebsc_db = cerebsc cof_cerebfail_db = cerebfail
cof_coagsc_db = coagsc cof_coagfail_db = coagfail cof_cardiosc_db = cardiosc cof_cardiofail_db = cardiofail cof_respsc_db = respsc cof_respfail_db = respfail;
run;

*************** MERGE; 
data DATA; 
set predict aclara; 
run;

*************** EDIT & FILTER DATA; 
* 1. select only data after diagnostic;
data data;
set data;
if visitdt < diagnostic_visitdt then delete;
run;

* 2. remove EOS visits without creatinine, albumin and vasopressors;
data data;
set data;
if visit = 'EOS' & creat = . & tr_alb = . & tr_vasop = . then delete;
run;

* 3. add variable 'resolution' (if creatinine < 1.5);
data data;
set data;
if creat >= 1.5 then resolution = 'No response';
else if creat < 1.5 then resolution = 'Response';
if creat = . then resolution = '.';
run;

*** if only 1 session per subject -> NA ???;

* 4. Check if subjects have taken albumin or vasopressors at any of the sessions between diagnostic & resolution;
/*proc sort data = data out = data;
    by idpatient;
run;

data any_alb;
   set data;
   by idpatient;
   if First.idpatient then sum_albumin=0;
   sum_albumin + albumin;
   if Last.idpatient;
run;

data any_alb;
set any_alb;
if sum_albumin = 0 then any_albumin = 0;
else if sum_albumin > 0 then any_albumin = 1;
run;

data any_vasop;
   set data;
   by idpatient;
   if First.idpatient then sum_vasopressors=0;
   sum_vasopressors + vasopressors;
   if Last.idpatient;
run;

data any_vasop;
set any_vasop;
if sum_vasopressors = 0 then any_vasopressors = 0;
else if sum_vasopressors > 0 then any_vasopressors = 1;
run;

proc sort data=data; by IDPATIENT; run;
proc sort data=any_alb; by IDPATIENT; run;
proc sort data=any_vasop; by IDPATIENT; run;
data data;
merge
	data
	any_alb (keep = idpatient any_albumin)
	any_vasop (keep = idpatient any_vasopressors);
by idpatient;
run;

* 5. Remove subjects with no treatment of albumin/vasopressors/both;
/*data data;
set data;
if any_albumin = 0 & any_vasopressors = 0 then delete;
run;
*/
* 6. remove subjects with NAs at resolution (if only 1 visit) ???;
 
*************** EXPORT AS CSV; 
proc export data=data
    outfile=".../predict-aclara-longitudinal-no-filter.xlsx"
    dbms=xlsx REPLACE;
run;

*************** EXPORT LAST VISIT AS CSV; 
/*proc sort data=data; by IDPATIENT visitdt; run;

data last_data;
   set data;
   by idpatient;
   if Last.idpatient;
run;

proc export data=last_data
    outfile="U:/tfm/0-select-data/data/predict-aclara-last-visit.xlsx"
    dbms=xlsx REPLACE;
run;

*************** EXPORT DIAGNOSTIC VISIT AS CSV; 
proc sort data=data; by IDPATIENT visitdt; run;

data diagnostic_data;
   set data;
   by idpatient;
   if First.idpatient;
run;


proc export data=diagnostic_data
    outfile="U:/tfm/0-select-data/data/predict-aclara-diagnostic-visit-filtered.xlsx"
    dbms=xlsx REPLACE;
run;


*************** subjects with less that 1.5 ml of urine;
data low_ml_urine;
set diagnostic_data;
if diagnostic_total_ml < 1.5;
run; 
*/


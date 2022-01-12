proc datasets library=WORK kill; run; quit;

*************** IMPORT DATA; 
proc import datafile="\\10.34.75.200\alba.morato/tfm/0-select-data/data/predict-aclara-longitudinal-no-filter.xlsx"
     out = data
     dbms = xlsx replace;
     getnames = yes;
run;

*************** FILTER; 

* 1. Remove subjects with no treatment;
** 1.1. Check if subjects have taken albumin or vasopressors at any of the sessions between diagnostic & resolution;
proc sort data = data out = data;
    by idpatient;
run;

data any_alb;
   set data;
   by idpatient;
   if First.idpatient then sum_albumin=0;
   sum_albumin + tr_alb;
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
   sum_vasopressors + tr_vasop;
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

** 1.2. Remove subjects with no treatment of albumin/vasopressors/both;
data data;
set data;
if any_albumin = 0 & any_vasopressors = 0 then delete;
run;

* 2. Remove subjects with no urine sample at diagnostic;
data data;
set data;
if diagnostic_urine = 0 then delete;
run;

* 3. Select only diagnostic visit;
proc sort data=data; by idpatient visitdt; run;
data diagnostic_data;
set data;
by idpatient visitdt;
if first.idpatient;
run;
* n = 184;

* Following dataset should be empty;
data errors;
set diagnostic_data;
if diagnostic_visitdt = visitdt then delete;
run;

*************** CHECK WHICH SUBJECTS COME FROM WHICH SAMPLE; 

* IMPORT URINE DATABASES;
proc import datafile="\\10.34.75.200\alba.morato\tfm\data\urine\20210920_UrineWilmut_Predict.xlsx"
     out=work.urinepw
     dbms=xlsx
     replace;
     getnames=yes;
	 sheet = "Orinas 500 uL";
run;

proc import datafile="\\10.34.75.200\alba.morato\tfm\data\urine\20210920_Urine_Predict_actualizada.xlsx"
     out=work.urinep
     dbms=xlsx
     replace;
     getnames=yes;
	 sheet = "Hoja1";
run;

proc import datafile="\\10.34.75.200\alba.morato\tfm\data\urine\20211025_ACLARAOrinasWilmut.xlsx"
     out=work.urineaw
     dbms=xlsx
     replace;
     getnames=yes;
	 sheet = "Hoja2";
run;

proc import datafile="\\10.34.75.200\alba.morato\tfm\data\urine\20211025_ACLARAOrinas15mL.xlsx"
     out=work.urinea
     dbms=xlsx
     replace;
     getnames=yes;
	 sheet = "Hoja1";
run;

* Create new variables to identify cohort and sample type;
data urinepw;
set urinepw;
cohort = 'PREDICT';
sample_type = 'wilmut';
run;

data urinep;
set urinep;
cohort = 'PREDICT';
sample_type = '15ml';
run;

data urineaw;
set urineaw (rename =(idvisit = visit_id));
cohort = 'ACLARA';
sample_type = 'wilmut';
run;

data urinea;
set urinea (rename =(idvisit = visit_id));
cohort = 'ACLARA';
sample_type = '15ml';
run;

* CONCATENATE;
data urine;
set urinepw urinep urineaw urinea;
run;

* FORMAT VISITS;
data urine;
set urine;
format visit $4.;
if visit = 'S00' & cohort = 'PREDICT' then visit = 'IN';
if visit = 'S01' & cohort = 'PREDICT' then visit = 'W1';
if visit = 'S04' & cohort = 'PREDICT' then visit = 'W4'; 
if visit = 'S08' & cohort = 'PREDICT' then visit = 'W8';
if visit = 'S12' & cohort = 'PREDICT' then visit = 'W12';
if visit = 'A00' & cohort = 'PREDICT' then visit = 'AW0';
if visit = 'A01' & cohort = 'PREDICT' then visit = 'AW1';
if visit = 'E2_' & cohort = 'PREDICT' then delete;
if visit = 'R01' & cohort = 'PREDICT' then delete;
if visit = 'R02' & cohort = 'PREDICT' then delete;
if visit = 'R04' & cohort = 'PREDICT' then delete;
if visit = 'R09' & cohort = 'PREDICT' then delete;
if visit = 'S00' & cohort = 'ACLARA' then visit = 'IN';
if visit = 'S01' & cohort = 'ACLARA' then visit = 'D8';
if visit = 'S02' & cohort = 'ACLARA' then visit = 'EOS';
if visit = 'A00' & cohort = 'ACLARA' then visit = 'AD0';
if visit = 'A01' & cohort = 'ACLARA' then visit = 'AD8';
if visit = 'A02' & cohort = 'ACLARA' then visit = 'EOS';
if visit = 'S98' & cohort = 'ACLARA' then delete;
if visit = 'S99' & cohort = 'ACLARA' then delete;
run;

** WHICH SAMPLES ARE OF 15 ml?;
data urine_15ml;
set urine;
if sample_type = '15ml';
run;

proc sort data=urine_15ml; by idpatient visit; run;
proc sort data=diagnostic_data; by idpatient visit; run;

data urine_15ml_selected;
merge
	urine_15ml (IN = in_urine)
	diagnostic_data (IN = in_data);
by Idpatient visit;
IF in_urine = 1 & in_data = 1;
run;
* 98;

data urine_not_selected ;
  merge urine_15ml_selected(in=a)
        diagnostic_data(in=b);
  by Idpatient visit;
  if not A and B;
run;

data urine_not_selected;
set urine_not_selected(drop = visit_id patient_id n sample_type sample_id idpatient_);
run;

** WHICH SAMPLES ARE OF 500 ul (wilmut)?;
data urine_wilmut;
set urine;
if sample_type = 'wilmut';
run;

proc sort data=urine_wilmut; by idpatient visit; run;
proc sort data=urine_not_selected; by idpatient visit; run;

data urine_wilmut_selected;
merge
	urine_wilmut (IN = in_urine)
	urine_not_selected (IN = in_data);
by Idpatient visit;
IF in_urine = 1 & in_data = 1;
run;
* 86;

* TOTAL: 98 + 86 = 184;

data all_urine_selected;
set urine_15ml_selected urine_wilmut_selected;
run;

data all_urine_selected;
set all_urine_selected (keep = visit_id idpatient cohort sample_type visitdt n);
if sample_type = '15ml' then total_ml = 15*n;
else if sample_type = 'wilmut' then total_ml = 5*n;
run;

/*
data urine_selected_less;
set all_urine_selected;
if total_ml < 15;
run;
*/
proc sort data=all_urine_selected; by cohort sample_type; run;

** EXPORT;
proc export data=all_urine_selected
    outfile="\\10.34.75.200\alba.morato/tfm/0-select-data/urine-selection/samples.xlsx"
    dbms=xlsx REPLACE;
run;

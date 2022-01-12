
*************************************************************;
************************** PREDICT **************************;
*************************************************************;
%inc "Y:\102_PREDICT\PROGRAMS\PREDICT - SETUP.sas";

*************** URINE; 
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

proc sort data=work.urinep; by IDPATIENT VISIT; run;
proc sort data=work.urinepw; by IDPATIENT VISIT; run;

data urinep;
set urinep;
rename n = n_15;
ml_15 = 1.5;
run;
data urinepw;
set urinepw;
rename n = n_5;
ml_5 = 0.5;
run;
data urinep;
merge
	work.urinep 
	work.urinepw
; by IDPATIENT VISIT
;
run;

data urinep;
set urinep;
if n_15 = . then n_15 = 0;
if n_5 = . then n_5 = 0;
total_ml = n_15*1.5 + n_5*0.5;
run;

/*data urinep;
set urinep;
drop sample_id visit_id patient_id n;
run;*/

proc sort data=urinep nodupkey;
	by idpatient visit;
run;

proc freq data = urinep;
table visit;
run;

data urinep;
set urinep;
if visit = 'S00' then visit = 'IN';
if visit = 'S01' then visit = 'W1';
if visit = 'S04' then visit = 'W4'; 
if visit = 'S08' then visit = 'W8';
if visit = 'S12' then visit = 'W12';
if visit = 'A00' then visit = 'AW0';
if visit = 'A01' then visit = 'AW1';
if visit = 'E2_' then delete;
if visit = 'R01' then delete;
if visit = 'R02' then delete;
if visit = 'R04' then delete;
if visit = 'R09' then delete;
if visit = 'R10' then delete;
run;

data urinep;
set urinep;
urine = 1;
run;

*************** HRS; 
data aki;
	set statroot.clinfeat;
	keep idpatient akinog_tp:;
run;

proc transpose data = aki out = aki;
   by idpatient;
run;

data HRS;
  length visit $4.;
  set aki (rename = (col1 = AKI_type));
  WHERE AKI_type = 2;
  visit = substr(_name_, 11);
  drop _name_ _LABEL_;
run; 

data HRS;
set HRS;
if visit = 'HA' then delete;
AKI = 'Yes';
run;

*************** ALBUMIN; 
data ptreatment;
set STATROOT.TR;
keep  tr_alb_hrstr: Visit IDPATIENT;
format Visit $3.;
if tr_alb_hrstr_IN = 1 then Visit = 'IN'; else
if tr_alb_hrstr_W1 = 1 then Visit = 'W1'; else
if tr_alb_hrstr_W4 = 1 then Visit = 'W4'; else
if tr_alb_hrstr_W8 = 1 then Visit = 'W8'; else
if tr_alb_hrstr_W12 = 1 then Visit = 'W12'; else
if tr_alb_hrstr_AW0 = 1 then Visit = 'AW0'; else
if tr_alb_hrstr_AW1 = 1 then Visit = 'AW1';
run;

data ptreatment;
set ptreatment;
keep idpatient Visit Albumin_HRS;
if Visit = "" then delete;
Albumin_HRS = 1;
run;

*************** TERLIPRESSIN; 
data terlipressin;
set statroot.ve_tr_vasop;
where vasop_tp ne . and vasop_hrstr = 1;
run;

proc sort data=terlipressin;
	by idpatient vasop_onsetdt;
run;

data terlipressin;
set terlipressin;
if vasop_onsetdt = . then delete;
if vasop_enddt = . then vasop_enddt = vasop_onsetdt;
run;

proc sort data=statroot.visits; by IDPATIENT; run;

data terlipressin;
merge
	work.terlipressin (in = a) 
	statroot.visits 
; by IDPATIENT
;if a;
run;

data terlipressin;
set terlipressin;
keep  idpatient vasop_tp vasop_onsetdt vasop_enddt visit_dt_IN visit_dt_W1 visit_dt_W4 visit_dt_W8 visit_dt_W12 visit_dt_AW0 visit_dt_AW1 Visit;
format Visit $3.;
run;


data terlipressin;
set terlipressin;
days_from_vasop_start = .;
days_from_vasop_end= .;
	if vasop_onsetdt le visit_dt_IN and vasop_onsetdt ne . then do; 
	Visit = 'IN' ; days_from_vasop_end = visit_dt_IN - vasop_enddt; days_from_vasop_start = visit_dt_IN - vasop_onsetdt;
	end;
	if vasop_onsetdt le visit_dt_W1 and vasop_onsetdt ne . and Visit eq '' then do; 
	Visit = 'W1' ;  days_from_vasop_end = visit_dt_W1 - vasop_enddt; days_from_vasop_start = visit_dt_W1 - vasop_onsetdt;
	end;
	if vasop_onsetdt le visit_dt_W4 and vasop_onsetdt ne . and Visit eq '' then do; 
	Visit = 'W4' ; days_from_vasop_end = visit_dt_W4 - vasop_enddt; days_from_vasop_start = visit_dt_W4 - vasop_onsetdt;
	end;
	if vasop_onsetdt le visit_dt_W8 and vasop_onsetdt ne . and Visit eq '' then do; 
	Visit = 'W8' ; days_from_vasop_end = visit_dt_W8 - vasop_enddt; days_from_vasop_start = visit_dt_W8 - vasop_onsetdt;
	end;
	if vasop_onsetdt le visit_dt_W12 and vasop_onsetdt ne . and Visit eq '' then do; 
	Visit = 'W12' ; days_from_vasop_end = visit_dt_W12 - vasop_enddt; days_from_vasop_start = visit_dt_W12 - vasop_onsetdt;
	end;
	if vasop_onsetdt le visit_dt_AW0 and vasop_onsetdt ne . and Visit eq '' then do; 
	Visit = 'AW0' ; days_from_vasop_end = visit_dt_AW0 - vasop_enddt; days_from_vasop_start = visit_dt_AW0 - vasop_onsetdt;
	end;
	if vasop_onsetdt le visit_dt_AW1 and vasop_onsetdt ne . and Visit eq '' then do; 
	Visit = 'AW1' ; days_from_vasop_end = visit_dt_AW1 - vasop_enddt; days_from_vasop_start = visit_dt_AW1 - vasop_onsetdt;
	end;
run;

data terlipressin;
set terlipressin;
keep idpatient Visit days_from_vasop_start days_from_vasop_end vasop_HRS vasop_tp;
vasop_HRS = 1;
run;

* Imputation by hand of missing subject 322001;
data terlipressin;
set terlipressin;
if idpatient = '322001' then do; visit = 'IN'; days_from_vasop_end =-4; days_from_vasop_start = -1; end;
run;

*************** MERGE ALBUMIN & TERLIPRESSIN; 
proc sort data=ptreatment; by IDPATIENT VISIT; run;
proc sort data=terlipressin; by IDPATIENT VISIT; run;
data ptreatment;
merge
	ptreatment 
	terlipressin;
by Idpatient Visit;
run;

*************** MERGE TREATMENT & HRS; 
data ptreatment(drop=visit2);
length visit $4.;
set ptreatment(rename=(visit=visit2));
visit = visit2;
run;

proc sort data=ptreatment; by IDPATIENT VISIT; run;
proc sort data=HRS; by IDPATIENT VISIT; run;

data predict;
merge
	ptreatment
	HRS(keep = idpatient visit AKI);
by Idpatient Visit;
run;

data predict;
set predict;
if AKI = '' then AKI = 'No';
run;

*************** ADD CREATININE; 
data work.creatinine;
set statall.labs (keep = idpatient creat_:); 
run;

proc transpose data = creatinine out = creatinine;
   by idpatient;
run;

data creatinine;
  length visit $4.;
  set creatinine (rename = (col1 = Creatinine));
  visit = substr(_name_, 7);
  drop _name_ _LABEL_;
run; 
proc sort data=predict; by IDPATIENT visit; run;
proc sort data=creatinine; by IDPATIENT visit; run;
data predict;
merge
	predict(in =a)
	creatinine;
by Idpatient visit;
if a;
run;

*************** ADD PREVIOUS & POSTERIOR CREATININE; 
proc sort data=predict; by IDPATIENT; run;
proc sort data=statroot.visits; by IDPATIENT; run;
data predict;
merge
	predict(in =a)
	statroot.visits(keep = idpatient visit_1-visit_13) ;
by idpatient;
if a;
run;

* format of visit_ as value of variable v_;
data predict;
set predict;
format v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 $3.;
v1 = vvalue(visit_1);
v2 = vvalue(visit_2);
v3 = vvalue(visit_3);
v4 = vvalue(visit_4);
v5 = vvalue(visit_5);
v6 = vvalue(visit_6);
v7 = vvalue(visit_7);
v8 = vvalue(visit_8);
v9 = vvalue(visit_9);
v10 = vvalue(visit_10);
v11 = vvalue(visit_11);
v12 = vvalue(visit_12);
v13 = vvalue(visit_13);
put v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13;
run;

data predict;
set predict;
length prev_visit post_visit $4.;
prev_visit = ".";
post_visit = ".";
if visit = v1 then do; post_visit = v2; end;
else if visit = v2 then do; prev_visit = v1; post_visit = v3; end;
else if visit = v3 then do; prev_visit = v2; post_visit = v4; end;
else if visit = v4 then do; prev_visit = v3; post_visit = v5; end;
else if visit = v5 then do; prev_visit = v4; post_visit = v6; end;
else if visit = v6 then do; prev_visit = v5; post_visit = v7; end;
else if visit = v7 then do; prev_visit = v6; post_visit = v8; end;
else if visit = v8 then do; prev_visit = v7; post_visit = v9; end;
else if visit = v9 then do; prev_visit = v8; post_visit = v10; end;
else if visit = v10 then do; prev_visit = v9; post_visit = v11; end;
else if visit = v11 then do; prev_visit = v10; post_visit = v12; end;
else if visit = v12 then do; prev_visit = v11; post_visit = v13; end;
else if visit = v13 then do; prev_visit = v12; end;
run;


proc sort data=predict; by IDPATIENT prev_visit; run;
proc sort data=creatinine; by IDPATIENT visit; run;
data predict;
merge
	predict(in=a) 
	creatinine(rename=(visit=prev_visit creatinine=prev_creatinine)); 
by idpatient prev_visit;
if a;
run;
proc sort data=predict; by IDPATIENT post_visit; run;
data predict;
merge
	predict(in=a)
	creatinine(rename=(visit=post_visit creatinine=post_creatinine)); 
by idpatient post_visit;
if a;
run;

*************** MERGE ALL WITH URINES BY DATE; 
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
* add dates to predict dataset;
proc sort data=visits; by IDPATIENT VISIT; run;
proc sort data=predict; by IDPATIENT VISIT; run;
data predict;
merge
	predict (in = a)
	visits;
by Idpatient Visit;
if a;
run;
* add previous visits date;
proc sort data=predict; by IDPATIENT PREV_VISIT; run;
data predict;
merge
	predict (in = a)
	visits(rename= (visit = prev_visit visitdt = prev_visitdt));
by Idpatient prev_visit;
if a;
run;
* merge predict dataset and urinep;
proc sort data=urinep; by IDPATIENT visit; run;
proc sort data=predict; by IDPATIENT visit; run;
data predict;
merge
	predict(in =a)
	urinep;
by Idpatient visit;
if a;
run;
*add previous urine;
proc sort data=predict; by IDPATIENT prev_visit; run;
data predict;
merge
	predict(in =a)
	urinep(rename =(visit = prev_visit urine = prev_urine total_ml = prev_total_ml));
by Idpatient prev_visit;
if a;
run;

*************** ADD ALBUMIN (FOR HRS OR NOT); 
data albumin;
set statroot.tr (keep = idpatient tr_alb_HA tr_alb_IN tr_alb_W1 tr_alb_W4 tr_alb_W8 tr_alb_W12 tr_alb_R1 tr_alb_R2 tr_alb_R3 tr_alb_R4 tr_alb_R5 tr_alb_AW0 tr_alb_AW1); 
run; 

proc transpose data = albumin out = albumin;
   by idpatient;
run;

data albumin;
  length visit $4.;
  set albumin (rename = (col1 = Albumin));
  visit = substr(_name_, 8);
  drop _name_ _LABEL_;
run; 

data albumin;
  set albumin;
  _num = put(albumin, 4.);
  drop albumin;
  rename _num = albumin;
run;

proc sort data=predict; by IDPATIENT visit; run;
proc sort data=albumin; by IDPATIENT visit; run;
data predict;
merge
	predict(in =a)
	albumin;
by Idpatient visit;
if a;
run;

data predict;
set predict;
if visit = '' then delete;
run;
*************** ADD VASOPRESSORS (FOR HRS OR NOT); 
data vasopressors;
set statroot.tr (keep = idpatient tr_vasop_HA tr_vasop_IN tr_vasop_W1 tr_vasop_W4 tr_vasop_W8 tr_vasop_W12 tr_vasop_R1 tr_vasop_R2 tr_vasop_R3 tr_vasop_R4 tr_vasop_R5 tr_vasop_AW0 tr_vasop_AW1); 
run; 

proc transpose data = vasopressors out = vasopressors;
   by idpatient;
run;

data vasopressors;
  length visit $4.;
  set vasopressors (rename = (col1 = Vasopressors));
  visit = substr(_name_, 10);
  drop _name_ _LABEL_;
run; 

data vasopressors;
  set vasopressors;
  _num = put(vasopressors, 4.);
  drop vasopressors;
  rename _num = vasopressors;
run;

proc sort data=predict; by IDPATIENT visit; run;
proc sort data=vasopressors; by IDPATIENT visit; run;
data predict;
merge
	predict(in =a)
	vasopressors;
by Idpatient visit;
if a;
run;
*************** RE-FORMAT PREDICT; 
data predict;
set predict;
akigr = ".";
run;

data predict;
set predict (keep = idpatient visit visitdt AKI Albumin Vasopressors vasop_tp Albumin_HRS vasop_HRS days_from_vasop_start days_from_vasop_end urine total_ml creatinine akigr prev_visit prev_visitdt prev_creatinine prev_urine post_visit post_creatinine prev_total_ml);
if urine = . then urine = 0;
if prev_urine = . then prev_urine = 0;
cohort = 'PREDICT';
run;

*************** FILTER;
proc sort data=predict; by IDPATIENT visitdt; run;
data predict;
	set predict;
	by idpatient visitdt;
	if first.idpatient;
run;

/*data predict;
set predict;
if urine = 0 and prev_urine = 0 then delete;
run;
*/

*************** EXPORT AS XLSX; 
proc export data=predict
    outfile="\\10.34.75.200\alba.morato/tfm/0-select-data/data/predict-current-previous-creatinine-urine.xlsx"
    dbms=xlsx  REPLACE;
run;

************ inspect subjects with AKI but no treatment; 
data AKI_notreat;
set predict;
if AKI = 'Yes' and Albumin = 0 and Vasopressors = 0;
run;

proc sort data=AKI_notreat; by IDPATIENT; run;
proc sort data=statroot.tr; by IDPATIENT; run;
data AKI_notreat;
merge
	AKI_notreat(in =a)
	statroot.tr(keep =idpatient tr_alb: tr_vasop:);
by Idpatient;
if a;
run;

*************************************************************;
************************** ACLARA ***************************;
*************************************************************;
%inc "Y:\114_ACLARA\PROGRAMS\ACLARA - SETUP.sas";

*************** URINE; 
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

proc sort data=work.urinea; by IDPATIENT VISIT; run;
proc sort data=work.urineaw; by IDPATIENT VISIT; run;

data urinea;
set urinea;
rename n = n_15;
ml_15 = 1.5;
run;
data urineaw;
set urineaw;
rename n = n_5;
ml_5 = 0.5;
run;

data urinea;
merge
	work.urinea 
	work.urineaw
; by IDPATIENT VISIT
;
run;

data urinea;
set urinea;
if n_15 = . then n_15 = 0;
if n_5 = . then n_5 = 0;
total_ml = n_15*1.5 + n_5*0.5;
run;

proc freq data = urinea;
table visit;
run;

data urinea;
set urinea;
format visit $4.;
if visit = 'S00' then visit = 'IN';
if visit = 'S01' then visit = 'D8';
if visit = 'S02' then visit = 'EOS';
if visit = 'A00' then visit = 'AD0';
if visit = 'A01' then visit = 'AD8';
if visit = 'A02' then visit = 'EOS';
if visit = 'S98' then delete;
if visit = 'S99' then delete;
run;

data urinea;
set urinea;
urine = 1;
run;

*************** ALBUMIN;
data albumin;
set statsas.pcm;
keep idpatient tr_dt tr_enddt tr_maxdose tr_alb_dic tr_alb_ind;
where tr_alb_dic ne .;
run;

data visits_long;
set statsas.vd (keep = idpatient visit visitfl_dt);
run;

proc sql noprint;
create table albumin as
select * from albumin a
join visits_long b
on a.idpatient = b.idpatient;
quit;

data albumin;
set albumin;
where tr_dt le visitfl_dt le tr_enddt;
run;


*************** TERLIPRESSIN;
data terlipressin;
set statsas.pcm;
keep idpatient tr_dt tr_enddt tr_maxdose tr_vasop_dict  tr_vasop_ind;
where tr_vasop_dict ne .;
;
run;

proc sql noprint;
create table terlipressin as
select * from terlipressin a
join visits_long b
on a.idpatient = b.idpatient;
quit;

data terlipressin;
set terlipressin;
where tr_dt le visitfl_dt le tr_enddt;
run;

*************** MERGE ALBUMIN & TERLIPRESSIN;
proc sort data=terlipressin; by IDPATIENT visit; run;
proc sort data=albumin; by IDPATIENT visit; run;
data treatment;
merge
	terlipressin (drop = tr_dt tr_enddt)
	albumin (drop = tr_dt tr_enddt); 
by IDPATIENT visit;
run;

* remove screening;
data treatment;
set treatment;
if Visit = 1 then delete;
run;

* select only visits with HRS indications;
data treatment;
set treatment;
if tr_vasop_ind = 3 or  tr_alb_ind = 2;
run;

* select first date for each subject;
proc sort data=treatment; by IDPATIENT visitfl_dt; run;
data treatment;
	set treatment;
	by idpatient visitfl_dt;
	if first.idpatient;
run;

proc sort data=treatment; by IDPATIENT visit; run;
proc sort data=statsas.cf; by IDPATIENT visit; run;
proc sort data=statsas.lb; by IDPATIENT visit; run;
*************** ADD AKI & CREATININE; 
data aclara;
merge
	work.treatment (in = a) 
	statsas.cf (keep = idpatient visit aki_v akigr_v akispo_hs aki_lv)
	statsas.lb (keep = idpatient visit creat)
; by IDPATIENT visit
;if a;
run;

*************** ADD PREVIOUS & POSTERIOR CREATININE; 
* visits to wide;
data visits;
set statsas.vd (keep = idpatient visit visitfl_dt);
if visit = 1 then delete; *delete screening;
if visit = 7 then delete; *delete Study termination;
run;
proc sort data=visits;
by idpatient visitfl_dt;

proc transpose data=visits(keep = idpatient visit) out=visits_w label=DESC name=visit prefix=v;
 by idpatient; run; 

proc sort data=aclara; by IDPATIENT; run;
proc sort data=visits_w; by IDPATIENT; run;
data aclara;
merge
	aclara(in =a)
	visits_w(keep = idpatient v1-v8) ;
by idpatient;
if a;
run;

data aclara;
set aclara;
if visit = v1 then do; post_visit = v2; end;
else if visit = v2 then do; prev_visit = v1; post_visit = v3; end;
else if visit = v3 then do; prev_visit = v2; post_visit = v4; end;
else if visit = v4 then do; prev_visit = v3; post_visit = v5; end;
else if visit = v5 then do; prev_visit = v4; post_visit = v6; end;
else if visit = v6 then do; prev_visit = v5; post_visit = v7; end;
else if visit = v7 then do; prev_visit = v6; post_visit = v8; end;
else if visit = v8 then do; prev_visit = v7; end;
run;

data creatinine;
set statsas.lb (keep = idpatient visit creat);
run;

proc sort data=aclara; by IDPATIENT prev_visit; run;
proc sort data=creatinine; by IDPATIENT visit; run;
data aclara;
merge
	aclara(in=a) 
	creatinine(rename=(visit=prev_visit creat=prev_creatinine)); 
by idpatient prev_visit;
if a;
run;

*add previous visit date;
proc sort data=aclara; by IDPATIENT post_visit; run;
proc sort data=visits; by IDPATIENT visit; run;
data aclara;
merge
	aclara(in=a)
	visits(rename=(visit=prev_visit visitfl_dt=prev_visitfl_dt)); 
by idpatient prev_visit;
if a;
run;

proc sort data=aclara; by IDPATIENT post_visit; run;
data aclara;
merge
	aclara(in=a)
	creatinine(rename=(visit=post_visit creat=post_creatinine)); 
by idpatient post_visit;
if a;
run;


*************** MERGE URINE WITH THE REST OF THE DATA;
* Create visit variable with abbreviation;
data aclara;
set aclara;
visitcode = visit;
prev_visitcode = prev_visit;
post_visitcode = post_visit;
run;

proc freq data=aclara;
   tables visitcode ;
   tables prev_visitcode;
   tables post_visitcode;
run;
data aclara;
set aclara;
drop visit prev_visit post_visit;
run;

data aclara;
set aclara;
format visit prev_visit post_visit $4.;
if visitcode = 2 then visit = 'IN';
if visitcode = 3 then visit = 'D8';
if visitcode = 4 then visit = 'AD0';
if visitcode = 5 then visit = 'AD8';
if visitcode = 6 then visit = 'EOS';
if visitcode = 7 then visit = 'ST';

if prev_visitcode = 2 then prev_visit = 'IN';
if prev_visitcode = 3 then prev_visit = 'D8';
if prev_visitcode = 4 then prev_visit = 'AD0';
if prev_visitcode = 5 then prev_visit = 'AD8';
if prev_visitcode = 6 then prev_visit = 'EOS';
if prev_visitcode = 7 then prev_visit = 'ST';

if post_visitcode = 2 then post_visit = 'IN';
if post_visitcode = 3 then post_visit = 'D8';
if post_visitcode = 4 then post_visit = 'AD0';
if post_visitcode = 5 then post_visit = 'AD8';
if post_visitcode = 6 then post_visit = 'EOS';
if post_visitcode = 7 then post_visit = 'ST';
if post_visitcode = 8 then post_visit = 'FU';
if post_visitcode = 9 then post_visit = 'FU';
run;

proc sort data=aclara; by IDPATIENT visit; run;
proc sort data=urinea; by IDPATIENT visit; run;
data aclara;
merge
	work.aclara (in = a) 
	urinea; 
by IDPATIENT visit;
if a;
run;

* urine sample at previous session;
proc sort data=aclara; by IDPATIENT prev_visit; run;
data aclara;
merge
	work.aclara (in = a) 
	urinea (rename = (visit=prev_visit urine=prev_urine total_ml = prev_total_ml)); 
by IDPATIENT prev_visit;
if a;
run;

*************** RE-FORMAT ACLARA; 
data aclara;
set aclara (rename=(tr_vasop_dict = vasop_tp aki_v = AKI visitfl_dt = visitdt creat = creatinine akigr_v = akigr prev_visitfl_dt = prev_visitdt));
days_from_vasop_start = .;
days_from_vasop_end = .;
Albumin_HRS = .;
vasop_HRS = .;
run;

data aclara;
set aclara (keep = idpatient visit visitdt AKI tr_alb_dic vasop_tp Albumin_HRS vasop_HRS days_from_vasop_start days_from_vasop_end urine total_ml creatinine akigr prev_visit prev_visitdt prev_creatinine prev_urine post_visit post_creatinine prev_total_ml);
if tr_alb_dic ne "." then Albumin = 1; else Albumin = 0;
if vasop_tp ne "." then Vasopressors = 1; else Vasopressors = 0;
if urine = . then urine = 0;
if prev_urine = . then prev_urine = 0;
run;

*************** FILTER;
* delete those with no urine samples;
/*data aclara;
set aclara;
if urine = 0 and prev_urine = 0 then delete;
run;
*/

data aclara;
set aclara;
cohort = 'ACLARA';
run;

*************** EXPORT AS CSV; 
proc export data=aclara
    outfile="\\10.34.75.200\alba.morato/tfm/0-select-data/data/aclara-current-previous-creatinine-urine.xlsx"
    dbms=xlsx REPLACE;
run;

*************************************************************;
******************** MERGE PREDICT & ACLARA *****************;
*************************************************************;

proc datasets library=WORK kill; run; quit;

*************** IMPORT CSVs; 
proc import datafile="\\10.34.75.200\alba.morato/tfm/0-select-data/data/predict-current-previous-creatinine-urine.xlsx"
     out=predict
     dbms=xlsx replace;
     getnames=yes;
run;

proc import datafile="\\10.34.75.200\alba.morato/tfm/0-select-data/data/aclara-current-previous-creatinine-urine.xlsx"
     out=aclara
     dbms=xlsx replace;
     getnames=yes;
run;

* change format;
data aclara;
  set aclara;
  alb_HRS = input(albumin_HRS, best12.);
  v_HRS = input(vasop_HRS, best12.);
  vasop_st = input(days_from_vasop_start, best12.);
  vasop_end = input(days_from_vasop_end, best12.);

  drop albumin_HRS vasop_HRS days_from_vasop_start days_from_vasop_end;
  rename alb_HRS = albumin_HRS v_HRS = vasop_HRS vasop_st = days_from_vasop_start vasop_end = days_from_vasop_end;
run;


data predict;
  set predict;
  _num = input(akigr, best12.);
  AKI_cat = AKI;
  alb = input(Albumin, best12.);
  vasop = input(Vasopressors, best12.);
  drop akigr AKI Albumin Vasopressors;
  rename _num = akigr alb = Albumin vasop = Vasopressors;
run;

data predict;
set predict;
if aki_cat = 'Yes' then AKI = 1;
else if aki_cat = 'No' then AKI = 0;
run;

data predict;
set predict;
drop aki_cat;
run;


*************** MERGE; 
data DATA; 
set predict aclara; 
run;

*************** EDIT & FILTER DATA; 
* 1. CHECK IF LEVELS OF CREATININE IN PREVIOUS VISITS WERE HIGHER;
data data;
set data;
format diagnostic_visitdt date9.;
if prev_creatinine > creatinine then 
	do;
		diagnostic_visit = prev_visit;
		diagnostic_visitdt = prev_visitdt;
		diagnostic_creatinine = prev_creatinine;
		diagnostic_urine = prev_urine;
		diagnostic_total_ml = prev_total_ml;
	end;
else 
	do;
		diagnostic_visit = visit;
		diagnostic_visitdt = visitdt;
		diagnostic_creatinine = creatinine;
		diagnostic_urine = urine;
		diagnostic_total_ml = total_ml;
	end;
run;

* 2. SELECT ONLY ROWS WITH URINE SAMPLE;
/*data data;
set data;
if diagnostic_urine = 1;
run;
*/

* 3. REMOVE CASES WHERE CREATININE AT DIAGNOSTIC IS < 1.5;
data data;
set data;
if diagnostic_creatinine >= 1.5;
run;

* 4. CHECK URINE ML;

data low_ml;
set data;
if diagnostic_total_ml < 1.5;
run;

*************** EXPORT AS CSV; 
proc export data=data
    outfile="\\10.34.75.200\alba.morato/tfm/0-select-data/data/predict-aclara-diagnostic-visit.xlsx"
    dbms=xlsx REPLACE;
run;

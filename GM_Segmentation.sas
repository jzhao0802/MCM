

libname dir_ge "C:\work\working materials\MCM";
%let dataPath = C:\work\working materials\MCM;

proc import out=dir_ge.Gm_promo_data
datafile = "&dataPath/MSD_Promo_OneKey_GE .xlsx"
dbms=excel replace;
GETNAMES=YES;
RANGE='top4k,middle2.5k,low 2.5k,no1k$A1:LP10001';
/*datarow=10001;*/
run;
data dir_ge.gm_promo_data;
set dir_ge.gm_promo_data(obs=10000);
run;

proc transpose data=dir_ge.gm_promo_data out=temp1;
ID CUST_ID_EXT_GENESYS;
VAR Besuch_2014_1 -- Webinar_2015_9; 
run;

data temp2;
set temp1;
re = PRXPARSE("/(\w+) (\d{4})-\d+/");

if prxmatch(re, _label_) then do;
type=prxposn(re, 1, _label_);
year=prxposn(re, 2, _label_);
output;
end;
/*if ^prxmatch(re, _label_) then do;*/
/*type=.;*/
/*year=.;*/
/*output;*/
/*end;*/
run;

proc means data=temp2 sum;
var _numeric_;
class type year;
output out=temp3 sum=;
run;

data temp4;
set temp3;
where type=3;
run;

proc contents
     data = temp2
          noprint
          out = temp2_info
               (keep = name varnum);
run;

proc sort data = temp2_info; by varnum; run;

data temp2_info;
set temp2_info;
if name not in ("re", "type", 'year', '_NAME_', '_LABEL_');
run;
data temp2_info1;
  set temp2_info nobs=nobs;
  if _n_ ^= nobs then do Name_Ref = compress("sum("||name||"),");end;
  else do Name_Ref=compress("sum("||name||")");end;
  call symput ('name_list', NAME_Ref); 
/*  %put _user_;*/
run;
%put &name_list;

data _null_;
 length allvars $32767;
 retain allvars ' ';
 set temp2_info1 end=eof;
 allvars = trim(left(allvars))||' '||left(name_ref);
 if eof then call symput('varlist', allvars);
 run;
%put &varlist;

proc sql;
select name_ref into: name_list from temp2_info1;
quit;
%put &name_list.;
proc sql;
create table temp3 as
select &name_list.
from temp2
group by type year;
quit;

DATA EXTRACT;
set temp1;
 IF _N_ = 1 THEN DO;
 PATTERN = PRXPARSE("/\w+ (\d{4}-\d+)/");
 IF MISSING(PATTERN) THEN DO;
 PUT "ERROR IN COMPILING REGULAR EXPRESSION";
 STOP;
 END;
 END;
 RETAIN PATTERN;
/* LENGTH NUMBER $ 15;*/
/* INPUT STRING $CHAR80.;*/
 CALL PRXSUBSTR(PATTERN,_LABEL_,START,LENGTH);
 IF START GT 0 THEN DO;
 NUMBER = SUBSTR(STRING,START,LENGTH);
 NUMBER = COMPRESS(NUMBER," ");
 OUTPUT;
 END;
 KEEP NUMBER; 

/*check count of doctors under various conditions*/
proc sql;
select count(*) from dir_fr.Fr_promo_data
where calls NE .;

select count(*) from dir_fr.Fr_promo_data
where mailing_flag NE "";

select count(*) from dir_fr.Fr_promo_data
where meeting_cost NE .;

select count(*) from dir_fr.Fr_promo_data
where calls NE .
and meeting_cost NE .;  /*2499*/

select count(*) from dir_fr.Fr_promo_data
where calls NE .
and mailing_flag NE ""
and meeting_cost NE .;    /*70*/

quit;
proc sql;
select count(distinct One_Key) from dir_fr.Fr_promo_data;
select count(*) from  dir_fr.Fr_promo_data;
quit;

/*subset input promo data*/
data fr_promo_subset_1;
set dir_fr.Fr_promo_data(keep = One_Key  calls  meeting_cost );

if calls NE .  and meeting_cost NE .;

run;

/*try hierarchical clustering using both calls and meetings_cost*/
proc cluster data = fr_promo_subset_1 outtree = output_tree method = ward noprint;
var calls  meeting_cost;
ID One_Key;
run;

proc tree data =output_tree  noprint out=out2 n=5;
copy calls  meeting_cost;
run;
/*proc compare base = out compare= out2;run; */
proc freq data=out2;tables CLUSTER * CLUSNAME/list;run;

/*ods html body = "D:\Project_Files2\Europe\Global MCM\MCM\001 Data\France Ipsen\Fr_Data_Distribution.html";*/
proc sgplot data=out2 ;
scatter y=meeting_cost  x=calls / group=cluster;
run;
/*ods html;*/


/*check distribution*/
proc univariate data = fr_promo_subset_1;
HISTOGRAM meeting_cost  /*  /NORMAL CFILL = ltgray  */;
INSET N = 'Number of Physicians' MEDIAN (8.2) MEAN (8.2) STD='Standard Deviation' (8.3)/ POSITION = ne;
run;
proc univariate data = fr_promo_subset_1;
HISTOGRAM calls  /*  /NORMAL CFILL = ltgray  */;
INSET N = 'Number of Physicians' MEDIAN (8.2) MEAN (8.2) STD='Standard Deviation' (8.3)/ POSITION = ne;
run;

/*try using only calls and see if the distribution among cluster will be more balanced*/
proc cluster data = fr_promo_subset_1 outtree = output_tree2 method = ward noprint;
var calls ;
ID One_Key;
run;
proc tree data =output_tree2  noprint out=out3  n=84;
copy calls ;
run;
proc freq data=out3;tables CLUSTER * CLUSNAME/list;run;

/*standardize promo variables*/
proc standard data=fr_promo_subset_1 MEAN=0  STD=1 out = fr_promo_subset_1std(drop=Calls_Decile  Meeting_Decile);run;
proc cluster data = fr_promo_subset_1std outtree = output_tree_std method = ward noprint;
var calls  meeting_cost;
ID One_Key;
run;
proc tree data =output_tree_std  noprint out=out_std n=5;
copy calls  meeting_cost;
run;
proc freq data=out_std;tables CLUSTER * CLUSNAME/list missing;run;
proc sgplot data=out_std ;
scatter y=meeting_cost  x=calls / group=cluster;
run;

/*the standardize promo variables will help improve the clustering results a bit. it can produce a slightly more balanced distribution among clusters*/

/*decile*/
%include "D:\yxue\IMS Decile Macro\DECILE.SAS";
%DECILE(fr_promo_subset_1, calls, 10, Calls_Decile);
%DECILE(fr_promo_subset_1, meeting_cost, 10, Meeting_Decile);
proc freq data=fr_promo_subset_1;
tables Calls_Decile  Meeting_Decile/missing;run;
/*the distribution is not good, there might be some problem with the results*/

proc export data= fr_promo_subset_1
outfile = "D:\Project_Files2\Europe\Global MCM\MCM\001 Data\France Ipsen\fr_promo_subset_1.csv"
dbms= csv replace;
run;


/*use the deciles from R*/
proc import out = fr_deciles_from_R
datafile ="D:\Project_Files2\Europe\Global MCM\MCM\001 Data\France Ipsen\Fr_promo_decile_from_R.csv"
dbms=csv replace;
getnames=yes;
datarow=2;
guessingrows= 2000;
run;

proc cluster data = fr_deciles_from_R outtree = output_tree_R  method = ward       noprint;
var call_decile  meeting_decile ;
ID One_Key;
run;
proc tree data =output_tree_R  noprint out=out_R  n=3;
copy call_decile  meeting_decile ;
run;
proc sgplot data=out_R ;
scatter y=meeting_decile  x=call_decile / group=cluster;
run;
proc freq data=out_R;tables CLUSTER * CLUSNAME/list missing;run;
 

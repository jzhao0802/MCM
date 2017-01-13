

libname dir_ge "C:\work\working materials\MCM";
%let dataPath = C:\work\working materials\MCM;

proc import out=dir_ge.Gm_promo_onekey_data
datafile = "&dataPath/MSD_Promo_OneKey_GE .xlsx"
dbms=xlsx replace;
GETNAMES=YES;
/*sheet="selected";*/
RANGE='selected$A1:LP10002';
/*datarow=10001;*/
run;

/*data dir_ge.Gm_promo_onekey_data;*/
/*set dir_ge.Gm_promo_onekey_data;*/
/*if _n_ >1;*/
/*run;*/

data promo_onekey_data_1obs;
set dir_ge.Gm_promo_onekey_data(obs=1);
run;


proc transpose data=promo_onekey_data_1obs out=temp1_1;
ID CUST_ID_EXT_GENESYS;
VAR IDENT -- erm_chtigt; 
run;

data _null_;
 length blank_vars $32767;
 retain blank_vars ' ';
 set temp1_1 end=eof;
 if add='blank' then do;
 	blank_vars = trim(left(blank_vars))||' '||left(_name_);
 end;
 if eof then call symput('blank_varlist', blank_vars);
 run;
%put &blank_varlist;

data Gm_promo_onekey_data_part1;
set dir_ge.Gm_promo_onekey_data;
drop &blank_varlist.;
run;

data Gm_promo_onekey_data_part2;
set dir_ge.Gm_promo_onekey_data;
keep &blank_varlist.;
run;

data Gm_promo_onekey_data_part2;
set Gm_promo_onekey_data_part2;
array arr _all_;
do over arr;
	if arr = '0' then arr='';
end;
run;


proc contents
     data = promo_onekey_data_1obs
          noprint
          out = info_all
               (keep = name varnum);
run;

proc sort data = info_all; by varnum; run;

data _null_;
 length org_vars $32767;
 retain org_vars ' ';
 set info_all end=eof;
/* if add='blank' then do;*/
 	org_vars = trim(left(org_vars))||' '||left(name);
/* end;*/
 if eof then call symput('org_varslist', org_vars);
 run;
%put &org_varslist;


data Gm_promo_onekey_transf;
retain &org_varslist;
merge Gm_promo_onekey_data_part1 Gm_promo_onekey_data_part2;
run;

data dir_ge.Gm_promo_onekey_transf;
set Gm_promo_onekey_transf;
if _n_ > 1;
run;

data dir_ge.Gm_promo_onekey_transf;
length MSD_seg $20;
set dir_ge.Gm_promo_onekey_transf;
if Gesamtergebnis = 0 then MSD_seg="no_1k";
else if Gesamtergebnis <=3 and Gesamtergebnis >=1 then MSD_seg="low_2d5k";
else if Gesamtergebnis <=129 and Gesamtergebnis >=45 then MSD_seg="top_4k";
else MSD_seg="middle_2d5k";
run;

proc freq data=dir_ge.Gm_promo_onekey_transf(where=(MSD_seg = "middle_2d5k"));
table Gesamtergebnis;
run;

/*middle_2d5k  (12~25)  is it the middle_10k?*/




proc transpose data=dir_ge.Gm_promo_onekey_transf(obs = 10) out=temp1;
ID CUST_ID_EXT_GENESYS;
VAR IDENT -- erm_chtigt; 
run;

data add_spec_flag;
set temp1;
re = PRXPARSE("/spec/i");
if prxmatch(re, _name_) then is_spec = 1;else is_spec=0;
run;

data _null_;
 length spec_vars $32767;
 retain spec_vars ' ';
 set add_spec_flag end=eof;
 if is_spec=1 then do;
 	spec_vars = trim(left(spec_vars))||' '||left(_name_);
 end;
 if eof then call symput('spec_varlist', spec_vars);
 run;
%put &spec_varlist;

ods output OneWayFreqs=freqs;
ods trace on;
proc freq data=dir_ge.gm_promo_onekey_transf;
tables &spec_varlist /out=freqs;
run;
ods output close;

ODS HTML BODY = "&datapath.\frq.htm"; 
PROC FREQ DATA=dir_ge.gm_promo_onekey_transf; 
TABLES &spec_varlist ;
ODS OUTPUT OneWayFreqs(match_all)=freqs; 
RUN; 
ODS HTML ;

proc sql;
create table freq_sum(where=(sum > 1000 and sum ^=.)) as
select table, sum(Frequency) as sum
from freqs
group by Table;
quit;

data NULL;
 length spec_vars_valid $32767;
 retain spec_vars_valid ' ';

set freq_sum end=eof;
re = PRXPARSE("/Table (.+)/");
if prxmatch(re, table) then do;
var=prxposn(re, 1, Table);
spec_vars_valid = trim(left(spec_vars_valid))||' '||left(var);
output;
end;
 if eof then call symput('spec_valid_varlist', spec_vars_valid);
run;

%put &spec_valid_varlist.; 

ODS HTML BODY = "&datapath.\spec_frq_valid.htm"; 
PROC FREQ DATA=dir_ge.gm_promo_onekey_transf; 
TABLES &spec_valid_varlist specialty1*MSD_seg
/*Nanobrick_Text Nanobrick_Text*MSD_seg*/
;
ODS OUTPUT OneWayFreqs(match_all)=freqs; 
RUN; 
ODS HTML ;

/*summarize the promo data*/
data for_promo;
set dir_ge.gm_promo_onekey_transf;
besuch = sum(of Besuch_2014_1--Besuch_2015_9);
pers_email=sum(of var27-var42);
Telefonkontakt=sum(of Telefonkontakt_2014_1--Telefonkontakt_2015_9);
VA_Teilnahme_Diabetes=sum(of VA_Teilnahme_Diabetes_2014_1--VA_Teilnahme_Diabetes_2015_9);
VA_Teilnahme_Produkt_bergreifen=sum(of VA_Teilnahme_Produkt_bergreifend--VA_Teilnahme_Produkt_bergreife23);
Veranstaltungskontakt=sum(of Veranstaltungskontakt_2014_1--Veranstaltungskontakt_2015_9);
Webinar=sum(of Webinar_2014_1--Webinar_2015_9);
run;

ODS HTML BODY = "&datapath.\brick_related_frq.htm"; 
PROC FREQ DATA=for_promo; 
TABLES city location_name address
;
ODS OUTPUT OneWayFreqs(match_all)=freqs; 
RUN; 
ODS HTML ;

ODS HTML BODY = "&datapath.\brick_related_frq.htm"; 
PROC FREQ DATA=for_promo; 
TABLES city location_name address
;
ODS OUTPUT OneWayFreqs(match_all)=freqs; 
RUN; 
ODS HTML ;

/*check distribution*/
proc univariate data = for_promo;
/*HISTOGRAM  besuch pers_email Telefonkontakt VA_Teilnahme_Diabetes*/
/*VA_Teilnahme_Produkt_bergreifen Veranstaltungskontakt Webinar*/
HISTOGRAM  besuch ;
/*  /NORMAL CFILL = ltgray  */;
/*INSET N = 'Number of Physicians' MEDIAN (8.2) MEAN (8.2) STD='Standard Deviation' (8.3)/ POSITION = ne;*/
run;

proc sql;
create table promo_by_msd as
select MSD_seg, sum(besuch) as besuch, sum(pers_email) as pers_email, sum(Telefonkontakt) as Telefonkontakt
, sum(VA_Teilnahme_Diabetes) as VA_Teilnahme_Diabetes, sum(VA_Teilnahme_Produkt_bergreifen) as VA_Teilnahme_Produkt_bergreifen
, sum(Veranstaltungskontakt) as Veranstaltungskontakt, sum(Webinar) as Webinar
/*VA_Teilnahme_Produkt_bergreifen Veranstaltungskontakt Webinar*/
from for_promo
group by MSD_seg;
quit;
proc export data=promo_by_msd
outfile="&datapath.\promo_by_msd_freq.csv"
dbms= csv replace;
run;
%put &datapath.;




/*cluster begin*/
data for_plot_1;
set for_promo;
newgrp=MSD_seg||specialty1;
array arr besuch Veranstaltungskontakt;
do over arr;
	if arr=. then arr = 0;
end;
keep MSD_seg specialty1 besuch Veranstaltungskontakt newgrp;
run;

proc sort data=for_plot_1 out=for_plot_1;
by newgrp;
run;

data for_plot_1;
set dir_ge.for_plot_1;
run;

/*plot*/
data for_plot_3;
set for_plot_3;
	length shape color $8.;
   if MSD_seg='low_2d5k' then do; shape="club"; color="blue"; end; 
   if MSD_seg='middle_2d5k' then do; shape="diamond"; color="red"; end;
   if MSD_seg='no_1k' then do; shape="spade"; color="green"; end;
   else do; shape="spade"; color="blue";end;
run;


goptions reset=all;
axis1 label=(a=90 'besuch');
symbol2 pointlabel = ("#statestatestat" h=1 font=swiss)  value=none;
/*symbol i=spline v=circle h=2;*/
proc gplot data=for_plot_1;
  plot MSD_seg*besuch / vaxis=axis1;
run;
quit;


proc G3D data=for_plot_3;
	scatter  besuch*Veranstaltungskontakt=cluster/
	color=color
	shape=shape
	size=2
	rotate=-36
	grid;
run;

/*plot end*/

/*try hierarchical clustering using both calls and meetings_cost*/
%macro freq_macro(data,var,out);
	proc freq data=&data.;
		table &var. /out=&out.;
	run;
%mend;

%macro turn_to_flag0(data,var, tb2, out);
	data &tb2.;
		set &tb2.;
/*		if _n_ = 1 then &var.flag=1;else &var.flag=&var.flag+1;*/
		&var._flag=_n_;
	run;
	proc sql;
	create table &out. as
	select a.* , b.&var._flag
	from &data. a left join &tb2. b
	on a.&var. = b.&var.;
	quit;
%mend;

%macro turn_to_flag00(data,vars2flag, out);
	%local i var;
	%do i = 1 %to  %sysfunc(countw(&vars2flag.));
		%let var = %scan(&vars2flag.,&i.," ");
		%if &i. = 1 %then %do;
			data &out.;
				set &data.;
			run;
		%end;
		%if &i. > 1 %then %do;
		data &out.;
			set &out.;
			by &var.;
			retain &var._flag 0;
			if first.&var. then &var._flag = &var._flag+1;
		run;
		%end;
	%end;
%mend;

%macro turn_to_flag(data,vars2flag, out);
	%local i var;
	data &out.;
		set &data.;
	run;

	%do i = 1 %to  %sysfunc(countw(&vars2flag.));
		%let var = %scan(&vars2flag.,&i.," ");
		proc sort data=&out. out=&out.;
			by &var.;
		run;
		data &out.;
			set &out.;
			by &var.;
			retain &var._flag 0;
			if first.&var. then &var._flag = &var._flag+1;
		run;
	%end;
%mend;


%let vars2flag=MSD_seg specialty1;
%put %sysfunc(countw(&vars2flag.));
%put %scan(&vars2flag.,1," ");
%turn_to_flag(data=for_plot_1, vars2flag=&vars2flag, out=for_plot_2);

%macro get_cluster_output(data,varlist,out,n_clst, method, outfile);
	proc cluster data = &data. outtree = output_tree method = &method. noprint;
var  &varlist.;

run;

proc tree data =output_tree   out=out n=&n_clst.dis;
copy &varlist.;
run;

	proc sql;
	create table &out. as
	select 	cluster, count(*) as cnt
	from out
	group by cluster
	quit;

	proc sort data=&out.;
		by cnt;
	run;
	
	proc export data=&out.
	outfile="&datapath.\GM\Dec06\&outfile..csv"
	dbms=csv replace;
	run;
%mend;
%freq_macro(for_plot_1, specialty1, freq_spec1);
%turn_to_flag(for_plot_1,specialty1, freq_spec1, for_plot_2)
%let varlist=MSD_seg_flag besuch Veranstaltungskontakt specialty1_flag;
%get_cluster_output(data=for_plot_2,varlist=&varlist.,out=cluster_out1,n_clst=300, method=EML);


%freq_macro(for_plot_2, MSD_seg, freq_msd);
%turn_to_flag(data=,vars2flag, out)
%let varlist=besuch Veranstaltungskontakt specialty1_flag msd_seg_flag;
%get_cluster_output(data=for_plot_3,varlist=&varlist.,out=cluster_out2,n_clst=500);

%macro my_sgplot(data, varlist, out,  y, x, group);
data &out.;
set &data.;
sumgrp = sum(of &varlist.);
run;
proc sgplot data=&out. ;
scatter y=&y.  x=&x. / group=&group.;
run;

%mend;
%let varlist2sumgrp = specialty1_flag msd_seg_flag;
%my_sgplot(out, &varlist2sumgrp., out_1, x=Veranstaltungskontakt, y=besuch, group=sumgrp);

data out_1;
set out_1;
	length shape color $8.;
   if MSD_seg_flag=1 then do; shape="club"; color="blue"; end; 
   if MSD_seg_flag=2 then do; shape="diamond"; color="red"; end;
   if MSD_seg_flag=3 then do; shape="spade"; color="green"; end;
   if MSD_seg_flag=4 then do; shape='club';color='orange';end;
run;

proc G3D data=out_1;
	scatter  besuch*Veranstaltungskontakt=cluster/
	color=color
	shape=shape
	size=2
	rotate=-36
	grid;
run;



proc sgplot data=out_std ;
scatter y=meeting_cost  x=calls / group=cluster;
run;

proc template;
  define statgraph surface;
  begingraph;
    layout overlay3d;
      surfaceplotparm x=gxc y=gyc z=estimate;
    endlayout;
  endgraph;
end;
run;

proc sgrender data=input template=surface;
run;

/*standarization*/
%let mtd = std;
 %macro Std(data, mtd, varlist);
   title2 "Data are standardized by PROC STDIZE with METHOD= &mtd";
      proc stdize data=&data. out=&data._std method=&mtd;
         var &varlist.;
      run;
	  proc means data=&data._std;
		 var &varlist.;
	  run;
   %mend Std;
%let varlist2std=besuch Veranstaltungskontakt specialty1_flag msd_seg_flag ;
%std(data=for_plot_2, mtd=std, varlist=&varlist2std);
%let varlist2cluster=besuch Veranstaltungskontakt specialty1_flag msd_seg_flag;
%get_cluster_output(data=for_plot_2,varlist=&varlist2cluster.,out=out_1_std_clst,n_clst=300, method=ward, outfile=cluster_out1_1_std);

data test;
set for_plot_1;
if _n_ =1 then do;
call symput('last_spec', specialty1);
spec_flag=1;
call symput('last_flag', spec_flag);
end;
if _n_ >1 then do;
last_spec=sysget('last_spec');
last_flag=sysget('last_flag');
if last_spec=specialty1 then spec_flag=last_flag;else spec_flag=last_flag+1;
call symput('last_spec', specialty1);
call symput('last_flag', spec_flag);
end;
run;

data test;
set for_plot_1;
by specialty1;
retain spec_flag;
if _n_ = 1 then spec_flag=0;
if first.specialty1 then spec_flag=spec_flag+1;
run;






proc cluster data = for_plot_2 outtree = output_tree method = ward noprint;
var  besuch Veranstaltungskontakt specialty1_flag;

run;

proc tree data =output_tree  noprint out=out2 n=300;
copy besuch Veranstaltungskontakt specialty1_flag;
run;

proc sql;
create table cluster_output as
select cluster, count(*) as cnt
from out2
group by cluster;
quit;


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
proc tree data =output_tree_R  noprint out=out_R  n=3 dis;
copy call_decile  meeting_decile ;
run;
proc sgplot data=out_R ;
scatter y=meeting_decile  x=call_decile / group=cluster;
run;
proc freq data=out_R;tables CLUSTER * CLUSNAME/list missing;run;
 

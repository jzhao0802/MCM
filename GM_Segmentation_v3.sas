libname dir_ge "C:\work\working materials\MCM";
%let dataPath = C:\work\working materials\MCM;

%let date=Dec07;
libname out "C:\work\working materials\MCM\GM\&date.";
%let outpath = C:\work\working materials\MCM\GM\&date.;
%put &outpath.;

%let cellsize=30;
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

proc sql;
create table promo_by_msd as
select MSD_seg, sum(besuch) as besuch, sum(pers_email) as pers_email, sum(Telefonkontakt) as Telefonkontakt
, sum(VA_Teilnahme_Diabetes) as VA_Teilnahme_Diabetes, sum(VA_Teilnahme_Produkt_bergreifen) as VA_Teilnahme_Produkt_bergreifen
, sum(Veranstaltungskontakt) as Veranstaltungskontakt, sum(Webinar) as Webinar
/*VA_Teilnahme_Produkt_bergreifen Veranstaltungskontakt Webinar*/
from for_promo
group by MSD_seg;
quit;

data for_plot_1;
set for_promo;
newgrp=MSD_seg||specialty1;
array arr besuch Veranstaltungskontakt;
do over arr;
	if arr=. then arr = 0;
end;
keep MSD_seg specialty1 besuch Veranstaltungskontakt newgrp;
run;


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

/*data for_plot_3;*/
/*set for_plot_2;*/
/*msd_spec=sum(of msd_seg_flag specialty1_flag);*/
/*run;*/

proc sql;
create table grp_ref_tb as
select distinct MSD_seg, specialty1, count(*) as freq
from for_plot_2
group by MSD_seg, specialty1;
quit;

/*QC*/
proc sql;
select sum(freq) into:total from grp_ref_tb;
quit;
%put &total.;

data out.grp_ref_tb;
set grp_ref_tb;
Grp_flag=_n_;
subgrp_num = round(freq/&cellsize.);
run;

proc sql;
 select count(*) into: grp_num from out.grp_ref_tb;
quit;
%put &grp_num.;

%macro get_cluster_output(data,i,varlist,out,n_clst, method, outfile, lib);
	proc cluster data = &data. outtree = output_tree method = &method. noprint;
var  &varlist.;

run;

proc tree data =output_tree   out=out&i. n=&n_clst.dis;
copy &varlist.;
run;

	proc sql;
	create table &out. as
	select 	&i. as group, cluster, count(*) as cnt
	from out&i.
	group by cluster;
	quit;

	proc sort data=&out.;
		by cnt;
	run;
	
	%if &i.=1 %then %do;
	data for_large_cluster_check;
	set &out.(obs=0);
	run;
	%end;
	proc datasets library=&lib. nolist;
	   append base=for_large_cluster_check data=&out. force;
	run;
	proc export data=&out.
	outfile="&outpath.\&outfile..csv"
	dbms=csv replace;
	run;
%mend;

 %macro Std(data, mtd, varlist);
   title2 "Data are standardized by PROC STDIZE with METHOD= &mtd";
      proc stdize data=&data. out=&data. method=&mtd;
         var &varlist.;
      run;
	  proc means data=&data.;
		 var &varlist.;
	  run;
   %mend Std;

%macro cluster_by_grp(input, ref_tb, vars2cluster,lib, std);
%global m_subgrp_num m_specialty1 m_msd_seg m_b_no_1k;
%do i = 1 %to &grp_num.;
	data temp;
		set &ref_tb.;
		if _n_=&i. then do;
			re=PRXPARSE("/no_1k/i");
			b_no_1k = prxmatch(re, msd_seg);

			call symput('m_specialty1', specialty1);
			call symput('m_msd_seg', msd_seg);
			call symput('m_subgrp_num', subgrp_num);
			call symput('m_b_no_1k', b_no_1k);
		end;
	run;
	%let m_subgrp_num = %sysfunc(putn(&m_subgrp_num, 3.0));
	%put &m_subgrp_num. &m_specialty1 &m_msd_seg;
	%put &m_b_no_1k;

/*	%let re = %sysfunc(PRXPARSE("/no_1k/i"));*/
/*	%let m_b_no_1k = %sysfunc(prxmatch(&re, &m_msd_seg));*/
/*	%put &m_b_no_1k.;*/

/*	%if &m_msd_seg="no_1k" %then %let no_1k=1 %else %let no_1k=0;*/
/**/
/*	%put &no_1k.;*/
	%if &m_subgrp_num > 1 and &m_b_no_1k ^= 1 %then %do;
		data dt_grp_&i.;
			set &input.;
			if Specialty1="&m_specialty1." and msd_seg="&m_msd_seg.";
			msd_test=symget("m_msd_seg");
			flag=(msd_test=msd_seg);
		run;

		%if &std.= 'yes' %then %do;
			%std(data=dt_grp_&i., mtd=std, varlist=&vars2cluster.);

		%end;
		%get_cluster_output(
				data=dt_grp_&i.
				,i=&i.
				,varlist=&vars2cluster.
				,out=cluster_out_grp_&i.
				,n_clst=&m_subgrp_num.
				, method=ward
				, outfile=cluster_out_grp_&i._&m_msd_seg._&m_specialty1.
				,lib=&lib.
				); 
	%end;	
%end;
%mend;
%let lib=work;
%let std='yes';
%let vars2cluster=besuch Veranstaltungskontakt;
%cluster_by_grp(input=for_plot_3
				, ref_tb=out.grp_ref_tb
				, vars2cluster=&vars2cluster.
				, lib=&lib.
				, std=&std.
				);

%macro check_large_cluster(input, threshold, outfile, std);
data tb_for_large;
set &input.;
if cnt > &threshold.;
run;

proc sql;
select count(*) into: n from tb_for_large;
quit;

%do i = 1 %to &n.;
data _null_;
set tb_for_large;
if _n_=&i. then do;
	call symput("group", group);
	call symput('cluster', cluster);
	call symput('freq', cnt);
end;
run;
%let group=%sysfunc(putn(&group., 3.0));
%let cluster=%sysfunc(putn(&cluster., 3.0));
%let freq=%sysfunc(putn(&freq., 3.0));
%put &group. &cluster. &freq.;
data dt2check_grp&group._freq&freq.;
set out&group.;
if cluster=&cluster.;
run;

proc freq data=dt2check_grp&group._freq&freq.;
table besuch*Veranstaltungskontakt/out=check_crossTb_&group._freq&freq.;
run;

data check_crossTb_&group._freq&freq.;
retain group freq ;
set check_crossTb_&group._freq&freq.;
group=&group.;
freq=&freq.;
run;

%if &i.=1 %then %do;
	data out.check_crossTb_summary;
	set check_crossTb_&group._freq&freq.(obs=0);
	run;
%end;
proc datasets library=work nolist;
	append base=out.check_crossTb_summary data=check_crossTb_&group._freq&freq. force;
run;

%if &std.='yes' %then %do;
	proc export data=out.check_crossTb_summary
	outfile="&outpath.\&outfile.&threshold._std.csv"
	dbms=csv replace;
	run;

%end;
%if &std.='no' %then %do;
	proc export data=out.check_crossTb_summary
	outfile="&outpath.\&outfile.&threshold..csv"
	dbms=csv replace;
run;

%end;
%end;
%mend;

%check_large_cluster(
						input=for_large_cluster_check
						, threshold=50
						, outfile=large_cluster_check_gt
						, std=&std.
						);


/*for histogram of promo type*/
proc means data=for_promo;
var besuch--Webinar;
output out=temp1;
run;

proc transpose data=temp1 out=temp2;

run;
data temp2;
set temp2;
if _n_ >2;
run;

data _null_;
 length all_promo_vars $32767;
 retain all_promo_vars ' ';
 set temp2 end=eof;
 all_promo_vars = trim(left(all_promo_vars))||' '||left(_name_);
 if eof then call symput('all_promo_vars', all_promo_vars);
 run;
%put &all_promo_vars;


data for_univ_1;
set for_promo;
newgrp=MSD_seg||specialty1;
array arr &all_promo_vars.;
do over arr;
	if arr=. then arr = 0;
end;
keep MSD_seg specialty1 &all_promo_vars.;
run;


%macro my_univariate(by_var);
ODS HTML path="&outpath."
BODY = "promo_univariate_by_&by_var..htm"; 
/*ods graphics on;*/
proc univariate data = for_univ_1;
HISTOGRAM &all_promo_vars.  /*  /NORMAL CFILL = ltgray  */;
class &by_var.;
INSET N = 'Number of Physicians' MEDIAN (8.2) MEAN (8.2) STD='Standard Deviation' (8.3)/ POSITION = ne;
run;
ODS HTML close;
/*ods graphics off;*/
%mend;
%my_univariate(msd_seg);
%my_univariate(specialty1);


%macro my_freq(by_var);
ods graphics on;
ODS HTML BODY = "&outpath.\histogram_by_&by_var..htm"; 
proc sort data=for_univ_1;
by &by_var.;
run;
proc freq data = for_univ_1;
tables &all_promo_vars.  / plots=freqplot /*  /NORMAL CFILL = ltgray  */;
by &by_var.;
/*INSET N = 'Number of Physicians' MEDIAN (8.2) MEAN (8.2) STD='Standard Deviation' (8.3)/ POSITION = ne;*/
run;
ODS HTML ;
ods graphics off;
%mend;


%my_freq(msd_seg);
%my_freq(specialty1);

Proc sql;
     select *
     from sashelp.vmember
     where libname = "work " and memname like "Temp%"
  ;
quit;

ods output Members=Members11;
proc datasets library=work memtype=data;
run;
quit;

%let x=%sysevalf(1.4+2.3);
%put &x.;

%let title3= "Using SYSFUNC %sysfunc(left(%qsysfunc(date(),worddate18.)))";
%put &title3;

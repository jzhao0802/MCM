libname dir_ge "C:\work\working materials\MCM\GM\data";
%let dataPath = C:\work\working materials\MCM\GM\data;

%let date=Apr28_1;
libname out "C:\work\working materials\MCM\GM\&date.";
%let outpath = C:\work\working materials\MCM\GM\&date.;
%put &outpath.;

%let cellsize=200;
%include "C:\work\bayes\Macro_Yan\foreach.sas";
%include "C:\work\bayes\Macro_Yan\decile.sas";


%let allPromo=Besuch Digital Telefonkont VA_Teilnahm Veranstaltu;
proc contents data=dir_ge.for_cluster;run;
data temp1;
set dir_ge.for_cluster;
array month_count %foreach(v, &allPromo., %nrstr(count_sum_with_month_&v.));
array promo_flag %foreach(v, &allPromo., %nrstr(flag_&v.));
do i = 1 to 5;
	if month_count[i]>=1 then promo_flag[i]=1;else promo_flag[i]=0;
end;
drop i;
run;

data temp1 dir_ge.for_cluster_v2;
set temp1;
num_rcv_promo=sum(of flag:);
run;

proc freq data=temp1;
table num_rcv_promo;
run;

/*proc format ;*/
/*value num_rcv_promo*/
/*1 = "One Channel"*/
/*2 = "Two Channel"*/
/*3-5 = "Multiple Channel";*/
/*run;*/
/**/
/*data temp1;*/
/*set temp1;*/
/*format num_rcv_promo num_rcv_promo.;*/
/*run;*/
data dir_ge.for_cluster_v2;
length num_rcv_promo_merge $ 50;
set temp1;
if num_rcv_promo=1 then num_rcv_promo_merge="1 Channel";else if num_rcv_promo=2 then num_rcv_promo_merge="2 Channel"; else num_rcv_promo_merge=">2 Channel";
run;


proc sql;
create table grp_ref_tb as
select distinct specialty1_merge, num_rcv_promo_merge, count(*) as freq
from dir_ge.for_cluster_v2
group by specialty1_merge, num_rcv_promo_merge;
quit;

/*QC*/
proc sql;
select sum(freq) into:total from grp_ref_tb;
quit;
%put &total.;

data out.grp_ref_tb;
set grp_ref_tb;
Grp_flag=_n_;
subgrp_num = ceil(freq/&cellsize.);
run;

proc sql;
 select count(*) into: grp_num from out.grp_ref_tb;
quit;
%put &grp_num.;

/**/
/*proc sql;*/
/*create table grp_ref_tb2 as*/
/*select distinct specialty1_merge , county_text , num_rcv_promo_merge, count(*) as freq*/
/*from dir_ge.for_cluster_v2*/
/*group by specialty1_merge , county_text, num_rcv_promo_merge;*/
/*quit;*/

/*QC*/
/*proc sql;*/
/*select sum(freq) into:total from grp_ref_tb2;*/
/*quit;*/
/*%put &total.;*/

/*data out.grp_ref_tb2;*/
/*set grp_ref_tb2;*/
/*Grp_flag=_n_;*/
/*subgrp_num = ceil(freq/&cellsize.);*/
/*run;*/

/*proc sql;*/
/*create table out.grp_ref_tb3 as*/
/*select a.specialty1_merge, a.county_text, a.freq as count_in_step1, b.num_rcv_promo_merge, b.freq as count_in_step2 */
/*from out.grp_ref_tb a left join out.grp_ref_tb2 b*/
/*on a.specialty1_merge=b.specialty1_merge and a.county_text=b.county_text and a.subgrp_num>1*/
/*order by a.specialty1_merge, a.county_text, b.num_rcv_promo_merge;*/
/*quit;*/

/*data out.grp_ref_tb3;*/
/*set out.grp_ref_tb3;*/
/*if count_in_step2=. then count_in_step2=count_in_step1;*/
/*subgrp_num=ceil(count_in_step2/&cellsize.);*/
/*grp_flag=_N_;*/
/*run;*/
/**/
/*proc sql;*/
/* select count(*) into: grp_num from out.grp_ref_tb3;*/
/*quit;*/
/*%put &grp_num.;*/

proc sql;
select max(freq) as max from out.grp_ref_tb /*12057*/
union
select min(freq) as min from out.grp_ref_tb /*868*/
union
select max(freq) as max from out.grp_ref_tb where freq<=200 /*200*/
union 
select min(freq) as min from out.grp_ref_tb where freq<=200 /*18*/
union
select count(*) as count from out.grp_ref_tb where freq<=200 /*64*/
union
select sum(freq) from out.grp_ref_tb where subgrp_num > 1 /*32330*/
;
quit;

%macro my_uni(input, var, where);
proc univariate data = &input.(where=(&where.));
HISTOGRAM &var.    /NORMAL CFILL = ltgray  ;
/*class &by_var.;*/
var &var.;
INSET N = 'Number of Physicians' MEDIAN (8.2) MEAN (8.2) STD='Standard Deviation' (8.3)/ POSITION = ne;
run;
%mend;
%my_uni(input=out.grp_ref_tb, var=freq, where=%nrstr(freq>1200));
/*%my_uni(input=out1.grp_ref_tb3, var=count_in_step2);*/

/*get the spend per doctor for step 2 cluster*/
/*proc transpose data=dir_ge.gm_promo_onekey_data out=temp2;*/
/*run;*/
/**/
/*data temp3;*/
/*set temp2;*/
/*re=prxparse("/(\w+) (2014|2015)/i");*/
/*if prxmatch(re, _label_) then promo_name=prxposn(re, 1, _label_);*/
/*run;*/
%let org_promo=Besuch pers_email Telefonkontakt VA_Teilnahme_Diabetes VA_Teilnahme_Produkt_berg Veranstaltungskontakt Webinar;
%let org_promo_cost= %foreach(v, &org_promo., %nrstr(c_&v.));
%let c_besuch=8182;
%let c_pers_email=160;
%let c_Telefonkontakt=2052;
%let c_VA_Teilnahme_Diabetes=30510;
%let c_VA_Teilnahme_Produkt_berg=30510;
%let c_Veranstaltungskontakt=10982;
%let c_Webinar=4922;
proc contents data=dir_ge.gm_promo_onekey_data;run;
data temp2;
set dir_ge.gm_promo_onekey_data;
besuch=sum(of Besuch_2014_1--Besuch_2015_9);
pers_email=sum(of VAR27--VAR42);
Telefonkontakt=sum(of Telefonkontakt_2014_1--Telefonkontakt_2015_9);
VA_Teilnahme_Diabetes=sum(of VA_Teilnahme_Diabetes_2014_1--VA_Teilnahme_Diabetes_2015_9);
VA_Teilnahme_Produkt_berg=sum(of VA_Teilnahme_Produkt_bergreifend--VA_Teilnahme_Produkt_bergreife23);
Veranstaltungskontakt=sum(of Veranstaltungskontakt_2014_1--Veranstaltungskontakt_2015_9);
Webinar=sum(of Webinar_2014_1--Webinar_2015_9);
array promo &org_promo.;
do over promo;
if promo=. then promo=0;
end;

spend=sum(Besuch*&c_Besuch.+pers_email*&c_pers_email. +Telefonkontakt*&c_Telefonkontakt. 
+VA_Teilnahme_Diabetes*&c_VA_Teilnahme_Diabetes. +VA_Teilnahme_Produkt_berg*&c_VA_Teilnahme_Produkt_berg.
+Veranstaltungskontakt*&c_Veranstaltungskontakt. +Webinar*&c_Webinar.);

if spend>0;
keep id spend;
run;

proc sql;
create table dir_ge.for_cluster_v2 as
select a.* , b.spend 
from
dir_ge.for_cluster_v2 a left join temp2 b
on a.id=b.id;
quit;



proc sql;
select count(*) from dir_ge.for_cluster_v2
where spend=.;
quit;

%my_uni(input=dir_ge.for_cluster_v2, var=spend, where=%nrstr(spend>0));

/*break into buckets of spend variable*/
%macro my_decile(i, input, var, outvar, breaks, prefix, output);
%decile(DATASET=&input.,VAR=&var.,BREAKS=&breaks.,OUTVAR=decile_&var.);
data &prefix._out&i.;
set &input.;
run;
proc sql;
create table temp as
select &i. as group, &outvar., count(*) as cnt
from &input.
group by &outvar.;
quit;
%if &i=1 %then %do;
data &output.;
set temp(obs=0);
run;
%end;
proc datasets library=work nolist;
append base=&output. data=temp force;
run;

%mend;

%macro my_rank_test(i, input, var, outvar, breaks, ties, output);
proc rank data=&input. groups=&breaks. out=&input. ties=&ties.;
var &var.;
ranks &outvar.;
run;
proc freq data=&input.;table &outvar.;run;
%mend;

%my_rank_test(&i, &input., &var., &outvar., &breaks., hight, &output.);
%my_rank_test(&i, &input., &var., &outvar., &breaks., mean, &output.);
%my_rank_test(&i, &input., &var., &outvar., &breaks., low, &output.);

%macro my_rank(i, input, var, outvar, breaks, ties, prefix, output);
proc rank data=&input. groups=&breaks. out=&input. ties=&ties.;
var &var.;
ranks &outvar.;
run;
data &prefix._out&i.;
set &input.;
run;

proc sql;
create table temp as
select &i. as group, &outvar., count(*) as cnt
from &input.
group by &outvar.;
quit;
%if &i=1 %then %do;
data &output.;
set temp(obs=0);
run;
%end;
proc datasets library=work nolist;
append base=&output. data=temp force;
run;


%mend;


%macro get_cluster_output(data,i,varlist,out,n_clst, method, outfile, lib, output);
	proc cluster data = &data. outtree = output_tree method = &method. noprint;
var  &varlist.;
id id;

run;

proc tree data =output_tree   out=out&i. n=&n_clst. dis;
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
	data &output.;
	set &out.(obs=0);
	run;
	%end;
	proc datasets library=&lib. nolist;
	   append base=&output. data=&out. force;
	run;
/*	proc export data=&out.*/
/*	outfile="&outpath.\&outfile..csv"*/
/*	dbms=csv replace;*/
/*	run;*/
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

%macro cluster_by_grp(input, ref_tb, vars2cluster,lib, std, method, output);
%global m_subgrp_num m_specialty1_merge m_num_rcv_promo_merge;
%do i = 1 %to 
&grp_num.
;
	data temp;
		set &ref_tb.;
		if _n_=&i. then do;
/*			re=PRXPARSE("/no_1k/i");*/
/*			b_no_1k = prxmatch(re, msd_seg);*/

			call symput('m_specialty1_merge', specialty1_merge);
/*			call symput('m_county_text', county_text);*/
			call symput('m_subgrp_num', subgrp_num);
			call symput('m_rcv_num', num_rcv_promo_merge);
		end;
	run;
	%let m_subgrp_num = %sysfunc(putn(&m_subgrp_num, 3.0));
	%put &m_subgrp_num. &m_specialty1_merge  &m_rcv_num;
/*	%put &m_b_no_1k;*/

/*	%let re = %sysfunc(PRXPARSE("/no_1k/i"));*/
/*	%let m_b_no_1k = %sysfunc(prxmatch(&re, &m_msd_seg));*/
/*	%put &m_b_no_1k.;*/

/*	%if &m_msd_seg="no_1k" %then %let no_1k=1 %else %let no_1k=0;*/
/**/
/*	%put &no_1k.;*/
	%if &m_subgrp_num > 1 
/*	and &m_b_no_1k ^= 1 */
	%then %do;
		data dt_grp_&i.;
			set &input.;
			if Specialty1_merge="&m_specialty1_merge." 
/*and county_text="&m_county_text."*/
			and num_rcv_promo_merge="&m_rcv_num."
;
/*			msd_test=symget("m_msd_seg");*/
/*			flag=(msd_test=msd_seg);*/
		run;

		%if &std.= 'yes' %then %do;
			%std(data=dt_grp_&i., mtd=std, varlist=&vars2cluster.);

		%end;
		%if &method.='decile' %then %do;
		%my_decile(i=&i., input=dt_grp_&i., var=&vars2cluster., outvar=decile_&vars2cluster., breaks=&m_subgrp_num., prefix=dcl, output=&output.);
/*		%let var4uni=decile_&vars2cluster.;*/
		%end;
		%if &method.='rank' %then %do;
		%my_rank(i=&i., input=dt_grp_&i., var=&vars2cluster., outvar=rank_&vars2cluster., breaks=&m_subgrp_num., ties=low, prefix=rnk, output=&output.);
/*		%let var4uni=decile_&vars2cluster.;*/
		%end;

		%if &method.='cluster' %then %do;
		%get_cluster_output(
				data=dt_grp_&i.
				,i=&i.
				,varlist=&vars2cluster.
				,out=cluster_out_grp_&i.
				,n_clst=&m_subgrp_num.
				, method=ward
				, outfile=cluster_out_grp_&i.
				,lib=&lib.
				,output=&output.
				); 
/*		%let var4uni=cnt;*/
		%end;
		
	%end;	
%end;
%my_uni(input=&output., var=cnt, where=%nrstr(cnt>0));

%mend;


proc contents data=temp3 noprint
          out = promo_info
               (keep = name varnum);

run;

data _NULL_;
 length allvars $32767;
 retain allvars ' ';
 set promo_info end=eof;
 allvars = trim(left(allvars))||' '||left(name);
 if eof then call symput('allVars2cluster', allvars);
run;

%put &allVars2cluster.;
%let allVars2cluster=
spend
/*count_sum_with_month_Besuch*/
/*count_sum_with_month_Digital */
/*count_sum_with_month_Telefonkont */
/*count_sum_with_month_VA_Teilnahm*/
/*count_sum_with_month_Veranstaltu */
/*sum_Besuch */
/*sum_Digital */
/*sum_Telefonkontakt*/
/*sum_VA_Teilnahme */
/*sum_Veranstaltungskontakt*/
;

%let lib=work;
%let std='no';
%let vars2cluster=&allVars2cluster ;
%cluster_by_grp(input=dir_ge.for_cluster_v2
				, ref_tb=out.grp_ref_tb
				, vars2cluster=&vars2cluster.
				, lib=&lib.
				, std=&std.
				, method='decile'
/*				, breaks=10*/
				, output=for_large_cluster_check_decile
				);

%cluster_by_grp(input=dir_ge.for_cluster_v2
				, ref_tb=out.grp_ref_tb
				, vars2cluster=&vars2cluster.
				, lib=&lib.
				, std=&std.
				, method='rank'
/*				, breaks=10*/
				, output=for_large_cluster_check_rank
				);
/*merge some small decile*/
%macro merge_decile_rank(input, output, var, prefix);
proc sql;
select count(distinct group) into: n from &input.;
quit;
%put &n.;
%do i=1 %to 
&n.
;
data temp1;
set &input.;
if group=&i.;
run;

data temp2;
   recno=_n_+1;
   set temp1 end=last;
   if not last 
           then set temp1 (keep=cnt rename=(cnt=next_row_cnt)) point=recno;
      else call missing(next_row_cnt);
run;

data temp3;
retain &var._re 1;
retain cum_cnt 0;
retain merge_end 0;
set temp2;
sum_with_next=next_row_cnt+cnt;
if merge_end=1 then &var._re=&var._re+1;
cum_cnt=cum_cnt+cnt;
cum_cnt_add_next=cum_cnt+next_row_cnt;
if cum_cnt<100 and cum_cnt_add_next<=200 then do;
	merge_end=0;
end;
if cum_cnt<100 and cum_cnt_add_next>200 then do;
	merge_end=1;
	cum_cnt=0;
end;
if cum_cnt<=200 and cum_cnt>=100 and cum_cnt_add_next<=200 then do;
	merge_end=0;
end;
if cum_cnt<=200 and cum_cnt>=100 and cum_cnt_add_next>200 then do;
	merge_end=1;
	cum_cnt=0;
end;
if cum_cnt>200 then do;
	merge_end=1;
	cum_cnt=0;
end;
/*if merge_end=0 then decile_spend_re=decile_spend_re;*/
/*keep group decile_spend_re cnt;*/
run;

/*change the decile_spend in data dcl_out1-dcl_out9*/
proc sql;
create table &prefix._out_merge&i. as
select a.*, b.&var._re
from &prefix._out&i. a left join temp3 b
on a.&var.=b.&var.;
quit;
data &prefix._out_merge&i.;
set &prefix._out_merge&i.;
drop &var.;
rename &var._re=&var.;
run;
%if &i=1 %then %do;
data temp4;
set temp3(obs=0);
run;
%end;
proc datasets library=work nolist;
append base=temp4 data=temp3 force;
run;
%end;
data temp4;
set temp4(drop=&var.);
keep group &var._re cnt;
rename &var._re=&var.;

run;
proc sql;
create table &output. as
select group, &var., sum(cnt) as cnt 
from temp4
group by group, &var.;
quit;

%mend;
%merge_decile_rank(input=For_large_cluster_check_decile
, output=For_large_cls_ck_dcl_merge
, var=decile_spend
, prefix=dcl);

%merge_decile_rank(input=For_large_cluster_check_rank
, output=For_large_cls_ck_rnk_merge
, var=rank_spend
, prefix=rnk);

%my_uni(input=For_large_cluster_check_decile, var=cnt, where=%nrstr(cnt<100));
%my_uni(input=For_large_cls_ck_dcl_merge, var=cnt, where=%nrstr(cnt<100));
%my_uni(input=For_large_cls_ck_dcl_merge, var=cnt, where=%nrstr(cnt>=0));
%my_uni(input=For_large_cls_ck_rnk_merge, var=cnt, where=%nrstr(cnt>=0));
/*%cluster_by_grp(input=dir_ge.for_cluster_v2*/
/*				, ref_tb=out.grp_ref_tb*/
/*				, vars2cluster=&vars2cluster.*/
/*				, lib=&lib.*/
/*				, std=&std.*/
/*				, method='cluster'*/
/*				, breaks=10*/
/*				, output=for_large_cluster_check_cluster*/
/*				);*/
proc sql;
select min(cnt), max(cnt) , sum(cnt) from For_large_cls_ck_dcl_merge
union
select min(cnt), max(cnt), sum(cnt) from For_large_cls_ck_rnk_merge
/*union*/
/*select min(cnt), max(cnt), sum(cnt) from For_large_cls_ck_dcl_merge*/
;
quit;
				
/*proc freq data=dir_ge.for_cluster;*/
/*table &allVars2cluster /cumcol;*/
/*run;*/

%macro getStatTb4clsRst(input, input1, cluster_var, var, ref_tb, outfile);
proc sql;
select count(*) into: n from &input.;
quit;


%do i = 1 %to 
&n.
;
data _null_;
set &input.;
if _n_=&i. then do;
	call symput("group", group);
	call symput('cluster', &cluster_var.);
	call symput('freq', cnt);
end;
run;
%let group=%sysfunc(putn(&group., 3.0));
%let cluster=%sysfunc(putn(&cluster., 3.0));
%let freq=%sysfunc(putn(&freq., 5.0));
%put &group. &cluster. &freq.;
data temp1;
set &input1.&group.;
if &cluster_var.=&cluster.;
run;

proc freq data=temp1;
table &var./cumcol
/*count_sum_with_month_Besuch */
/*count_sum_with_month_Digital **/
/*count_sum_with_month_Telefonkont **/
/*count_sum_with_month_VA_Teilnahm**/
/*count_sum_with_month_Veranstaltu **/
/*sum_Besuch**/
/*sum_Digital **/
/*sum_Telefonkontakt**/
/*sum_VA_Teilnahme **/
/*sum_Veranstaltungskontakt*/
out=temp2;
run;

data temp2;
retain group cluster freq ;
set temp2;
group=&group.;
cluster=&cluster.;
freq=&freq.;
run;

proc means data=temp2 mean std min max;
  id group cluster freq;
  var &var.;
  output out=temp3(drop=_FREQ_ _TYPE_) mean=mean std=std min=min max=max;
run;

%if &i.=1 %then %do;
	data out.&outfile.;
		set temp3(obs=0);
	run;
%end;
proc datasets library=work nolist;
	append base=out.&outfile. data=temp3 force;
run;

%end;

data out.&outfile.;
set out.&outfile.;
array rnd std mean;
do over rnd;
if rnd=. then rnd=0;
rnd=round(rnd, .01);
end;
run;
proc sort data=out.&outfile.;by group cluster;run;

proc sql;
create table out.&outfile. as
select b.specialty1_merge, b.num_rcv_promo_merge, a.* from
out.&outfile. a left join &ref_tb. b
on a.group=b.grp_flag;
quit;
	proc export data=out.&outfile.
	outfile="&outpath.\&outfile..csv"
	dbms=csv replace;
	run;

%mend;
%getStatTb4clsRst(input=For_large_cls_ck_rnk_merge
, input1=rnk_out_merge
, cluster_var=rank_spend
, var=spend
, ref_tb=out.grp_ref_tb
, outfile=stat_table_spend
)
;
/*qc for step3*/
proc sql;
select min(freq), max(freq), sum(freq) from out.stat_table_spend;
quit;
/*76 3085 39431  */

%macro check_large_cluster(input, input1, cluster_var, var, threshold, out, outfile, std);
data tb_for_large;
set &input.;
if cnt > &threshold.;
run;

proc sql;
select count(*) into: n from tb_for_large;
quit;

%do i = 1 %to 
&n.
;
data _null_;
set tb_for_large;
if _n_=&i. then do;
	call symput("group", group);
	call symput('cluster', &cluster_var.);
	call symput('freq', cnt);
end;
run;
%let group=%sysfunc(putn(&group., 3.0));
%let cluster=%sysfunc(putn(&cluster., 3.0));
%let freq=%sysfunc(putn(&freq., 5.0));
%put &group. &cluster. &freq.;
data dt2check_grp&group._cls&cluster.;
set &input1.&group.;
if &cluster_var.=&cluster.;
run;

proc freq data=dt2check_grp&group._cls&cluster.;
table &var. /cumcol
/*count_sum_with_month_Digital **/
/*count_sum_with_month_Telefonkont **/
/*count_sum_with_month_VA_Teilnahm**/
/*count_sum_with_month_Veranstaltu **/
/*sum_Besuch**/
/*sum_Digital **/
/*sum_Telefonkontakt**/
/*sum_VA_Teilnahme **/
/*sum_Veranstaltungskontakt*/
out=check_crossTb_&group._cls&cluster.;
run;

data check_crossTb_&group._cls&cluster.;
retain group cluster freq ;
set check_crossTb_&group._cls&cluster.;
group=&group.;
cluster=&cluster.;
freq=&freq.;
run;

%if &i.=1 %then %do;
	data out.&out.;
/*	retain cumfreq;*/
	set check_crossTb_&group._cls&cluster.(obs=0);
/*	cumfreq=freq;*/
	run;
%end;
proc datasets library=work nolist;
	append base=out.&out. data=check_crossTb_&group._cls&cluster. force;
run;
%end;
data out.&out.;
set out.&out.;
group_cls=catx('_', group, cluster);
run; 

proc sort data=out.&out.;by group_cls;run;

data out.&out.;
retain cumfreq 0;
set out.&out.;
by group_cls;
if first.group_cls then cumfreq=count;else cumfreq=cumfreq+count;
drop group_cls;
run;
/*proc sort data=out.check_crossTb_summary;by group freq;run;*/

%if &std.='yes' %then %do;
	proc export data=out.&out.
	outfile="&outpath.\&outfile.&threshold._std.csv"
	dbms=csv replace;
	run;

%end;
%if &std.='no' %then %do;
	proc export data=out.&out.
	outfile="&outpath.\&outfile.&threshold..csv"
	dbms=csv replace;
run;

%end;

%mend;

%check_large_cluster(
						input=For_large_cls_ck_rnk_merge
						, input1=rnk_out_merge
						, cluster_var=rank_spend
						, var=spend
						, threshold=&cellsize.
						, out=check_crossTb_summary
						, outfile=large_cluster_check_gt
						, std=&std.
						);


%macro merge_small_subgrp_step1(parse, output1, output2, output3);
data vmember_flag;
set sashelp.vmember;
re = PRXPARSE(&parse.);
flag=prxmatch(re, left(trim(memname)));
flag1=prxmatch(PRXPARSE("/^work$/i"), left(trim(libname)));
run;
Proc sql;
	create table dataset_list as
     select memname
     from vmember_flag
     where flag=1 and flag1=1
  ;
quit;

proc sql;
select count(*) into: n from dataset_list;
quit;
%put &n.;

%do i = 1 %to 
&n.
;
%put &i.;
data _null_;
set dataset_list;
if _n_=&i. then do;
	call symput("dataset_name", left(trim(memname)));
end;
run;
%put &dataset_name.;
proc sql;
select count(*) into: n_obs from &dataset_name;
run;
%put &n_obs.;
%if &n_obs.=1 %then %do;
data &dataset_name._merge;
set &dataset_name.;
new_subgrp=1;
run;
%end;
%if &n_obs.>1 %then %do;

proc sort data=&dataset_name;by freq;run;


data &dataset_name._merge;
   recno=_n_+1;
   set &dataset_name. end=last;
   if not last 
           then set &dataset_name. (keep=count rename=(count=next_row_cnt)) point=recno;
      else call missing(next_row_cnt);
run;

data &dataset_name._merge;
retain new_subgrp 1;
retain subgrp_end 0;
retain cumfreq 0;
set &dataset_name._merge;
if subgrp_end=1 then new_subgrp=new_subgrp+1;
cumfreq=cumfreq+count;
cumfreq_add_next=cumfreq+next_row_cnt;

if cumfreq<100 and cumfreq_add_next<=200 then do;
	subgrp_end=0;
end;
if cumfreq<100 and cumfreq_add_next>200 then do;
	subgrp_end=1;
	cumfreq=0;
end;
if cumfreq<=200 and cumfreq>=100 and cumfreq_add_next<=200 then do;
	subgrp_end=0;
end;
if cumfreq<=200 and cumfreq>=100 and cumfreq_add_next>200 then do;
	subgrp_end=1;
	cumfreq=0;
end;
if cumfreq>200 then do;
	subgrp_end=1;
	cumfreq=0;
end;
drop cumfreq subgrp_end cumfreq_add_next next_row_cnt;
run;

proc sql;
create table temp as
select group, cluster, new_subgrp, sum(count) as sumby_subgrp
from &dataset_name._merge
group by group, cluster, new_subgrp;
quit;

data temp;
set  temp;
cnt_lag=lag(sumby_subgrp);
if sumby_subgrp<100 then sum_with=sumby_subgrp+cnt_lag;
if sum_with <=200 and sum_with ^=. then grp_b2merge=catx("*", new_subgrp-1, new_subgrp);else grp_b2merge="";
if grp_b2merge ^="";
run;
%end;

%if &i.=1 %then %do;
	data out.&output1.;
	set &dataset_name._merge(obs=0);
	run;
	data out.&output3.;
	set temp(obs=0);
	run;
%end;
proc datasets library=work nolist;
	append base=out.&output1. data=&dataset_name._merge force;
run;

proc datasets library=work nolist;
	append base=out.&output3. data=temp force;
run;

%end;

proc sql;
create table out.&output2. as
select group, cluster, new_subgrp, sum(count) as sumby_subgrp
from out.&output1.
group by group, cluster, new_subgrp;
quit;
proc export data=out.&output2.
outfile="&outpath./&output2.(need to merge small subgrp to neighbour).csv"
dbms=csv replace;
run;

/*proc export data=out.&output3.*/
/*outfile="&outpath./&output2.(with merging for small subgrp to neighbour).csv"*/
/*dbms=csv replace;*/
/*run;*/

%mend;
%merge_small_subgrp_step1(
parse=%nrstr("/^check_crosstb_\d+_cls\d+$/i")
, output1=check_crossTb_summary_merge
, output2=Check_crosstb_sumby_newsubgrp
, output3=tb4revise
);

/*qc*/
proc sql;
select min(sumby_subgrp), max(sumby_subgrp), sum(sumby_subgrp) from out.check_crosstb_sumby_newsubgrp;
quit;


%macro merge_small_subgrp_step2(input, parse1, parse2, var);
proc import out=subgrp_after_manu_revise
datafile="&outpath./&input.(with merging for small subgrp to neighbour).csv"
dbms=csv replace;
run;

proc sql;
select count(*) into: n from subgrp_after_manu_revise;
quit;

%do i = 1 %to 
&n.
;

data test;
set subgrp_after_manu_revise;
drop new_subgrp;
run;
data test;
set test(rename=(&var.=new_subgrp));
if _n_ = &i. then do;
	call symput('group', left(trim(group)));
/*	call symput('freq', left(trim(freq)));*/
	call symput('cluster', left(trim(cluster)));

	new_subgrp=left(trim(new_subgrp));
	re=prxparse(&parse1.);
	re1=prxparse(&parse2.);
	bmatch1=prxmatch(re, new_subgrp);
	bmatch2=prxmatch(re1, new_subgrp);

	if bmatch1 then do;
		subgrp1=input(prxposn(re, 1, new_subgrp), 3.);
		subgrp2=input(prxposn(re, 2, new_subgrp), 3.);
		subgrp3=input(prxposn(re, 3, new_subgrp), 3.);

		call symput('subgrp1', subgrp1);
		call symput('subgrp2', subgrp2);
		call symput('subgrp3', subgrp3);
	end;
	else do;
		if bmatch2 then do;
			subgrp1=input(prxposn(re1, 1, new_subgrp), 3.);
			subgrp2=input(prxposn(re1, 2, new_subgrp), 3.);
			call symput('subgrp1', subgrp1);
			call symput('subgrp2', subgrp2);
			call symput('subgrp3', subgrp2);
		end;
	end;

end;
run;
%let group=%sysfunc(trim(%sysfunc(left(&group.))));
/*%let freq=%sysfunc(trim(%sysfunc(left(&freq.))));*/
%let cluster=%sysfunc(trim(%sysfunc(left(&cluster.))));

%put &group &cluster &subgrp1 &subgrp2 &subgrp3;

data check_crosstb_&group._cls&cluster._merge;
set check_crosstb_&group._cls&cluster._merge;
if new_subgrp in (&subgrp1. &subgrp2. &subgrp3.) then new_subgrp=&subgrp1.;
run;



%end;


%mend;

%merge_small_subgrp_step2(
input=Check_crosstb_sumby_newsubgrp
, parse1=%nrstr("/(\d+)\*(\d+)\*(\d+)/i")
, parse2=%nrstr("/(\d+)\*(\d+)/i")
, var=grp_b2merge);


%macro extract_obs_foreachsubgrp(parse, out, id_var, var);
data vmember_flag;
set sashelp.vmember;
re = PRXPARSE(&parse.); /*CHECK_CROSSTB_42_FREQ308_MERGE*/
flag=prxmatch(re, left(trim(memname)));
flag1=prxmatch(PRXPARSE("/^work$/i"), left(trim(libname)));

if flag=1 then do;
	group=input(prxposn(re, 1, left(trim(memname))), 3.);
	cluster=input(prxposn(re, 2, left(trim(memname))), 3.);
/*	call symput("group", group);*/
/*	call symput("freq", freq);*/
end;

run;

Proc sql;
	create table dataset_list as
     select memname, group, cluster
     from vmember_flag
     where flag=1 and flag1=1
  ;
quit;

proc sql;
select count(*) into: n from dataset_list;
quit;
%put &n. ;

%do i = 1 %to 
&n.
;
data _null_;
set dataset_list;
if _n_=&i. then do;
	call symput("dataset_name", left(trim(memname)));
	call symput("group", group);
	call symput("cluster", cluster);

end;
run;
%let group=%sysfunc(left(%sysfunc(trim(&group))));
%let cluster=%sysfunc(left(%sysfunc(trim(&cluster))));

%put &group. &cluster.;

	%if &i.=1 %then %do;
	data out.&out.;
	set &dataset_name(obs=0);
	run;
	%end;
	proc datasets library=work nolist;
	   append base=out.&out. data=&dataset_name. force;
	run;


/*%let re=%sysfunc(PRXPARSE("/^check_crosstb_(\d+)_freq(\d+)_merge$/i"));*/
/*%let group=%sysfunc(prxposn(&re., 1, &dataset_name.));*/
/*%let freq=%sysfunc(prxposn(&re., 2, &dataset_name.));*/

proc freq data=&dataset_name.;
table group*cluster*new_subgrp/out=freq_tb;
run;

proc sql;
select count(*) into: n_subgrp from freq_tb;
quit;

%do j = 1 %to &n_subgrp;
data _null_;
set freq_tb;
if _n_=&j. then do;
call symput('subgrp', new_subgrp);
end;
run;
%let subgrp=%sysfunc(left(%sysfunc(trim(&subgrp))));
%put &subgrp.;
proc sql;
create table dt_grp&&group._cls&&cluster._subgrp&subgrp. as
select &id_var. as obs_id, &var.
from dt2check_grp&&group._cls&cluster.
where &var. in
(select &var. from &dataset_name. where new_subgrp in (&subgrp.))
;
quit;
%end;
%end;

data out.&out.;
set out.&out.;
group_cls_subgrp=catx('_', group, cluster, new_subgrp);
run;

proc sort data=out.&out.;by group_cls_subgrp;run;
data out.&out.;
retain new_subgrp_re 0;
set out.&out.;
by group_cls_subgrp;
if first.group_cls_subgrp then new_subgrp_re=new_subgrp_re+1;
run;

proc sql;
create table test_conflict as
select *, sum(count) as sum_in_subgrp from out.&out.
group by new_subgrp_re
having percent < 100 and sum(count)>200;
quit;

/*proc sql;*/
/*create table test_conflict1 as*/
/*select a.* from out.ck_crosstb_summary_merge_revise a inner join test_conflict b*/
/*on a.new_subgrp=b.new_subgrp;*/
/*quit;*/
	proc export data=out.&out.
	outfile="&outpath.\&out..csv"
	dbms=csv replace;
	run;

%mend;
%extract_obs_foreachsubgrp(
parse=%nrstr("/^check_crosstb_(\d+)_cls(\d+)_merge$/i")
, out=Ck_crosstb_summary_merge_revise
, id_var=id
, var=spend
);

/*qc*/
proc sql;
create table temp as
select group, cluster, new_subgrp, sum(count) as count_in_subgrp
from out.Ck_crosstb_summary_merge_revise
group by group, cluster, new_subgrp;
quit;
proc sql;
select min(count_in_subgrp) from temp;
quit;

%my_uni(input=temp, var=count_in_subgrp, where=%nrstr(count_in_subgrp>0));



%macro statTb4revised(input, var, out);
proc sql;
select max(new_subgrp_re) into: n from &input.;
quit;

%do new_subgrp = 1 %to
&n.
;
%put &new_subgrp.;
data dt;
set &input.;
where new_subgrp_re=&new_subgrp.;
run;
proc sql;
create table temp1 as
select a.*, b.sum_count as count_in_subgrp
from &input.(where=(new_subgrp_re=&new_subgrp.)) a left join 
(select group, cluster, new_subgrp_re, sum(count) as sum_count from &input. group by group, cluster, new_subgrp_re) b
on a.group=b.group and a.cluster=b.cluster and a.new_subgrp_re=b.new_subgrp_re;
quit;

proc means data=temp1 nway;
  id group cluster freq new_subgrp_re new_subgrp count_in_subgrp;
  var &var.;
  output out=temp2(drop=_FREQ_ _TYPE_) mean=mean std=std min=min max=max;

run;
%if &new_subgrp.=1 %then %do;
data out.&out.;
set temp2(obs=0);
run;	

%end;
	proc datasets library=work nolist;
	   append base=out.&out. data=temp2 force;
	run;
%end;
proc sort data=out.&out.;by group cluster new_subgrp_re;run;

data out.&out.;
retain group cluster new_subgrp;
set out.&out.;
if std=. then std=0;
std=round(std, .01);
/*subgrp=new_subgrp;*/
new_subgrp_re=_n_;
/*drop new_subgrp_re;*/
run;

	proc export data=out.&out.
	outfile="&outpath.\&out..csv"
	dbms=csv replace;
	run;

/*qc*/
proc sql;
create table check_conflict_f as
select * from out.&out.
where std >0 and count_in_subgrp>200;

quit;

proc sql;
select min(count_in_subgrp) from out.&out.
where count_in_subgrp<100;
quit;
%mend;

%statTb4revised(input=out.Ck_crosstb_summary_merge_revise
				  , var=spend
				  , out=statTb4merge_revise
)

/*check the two qc table to find the conflict new_subgrp_re (group- 18  cluster-1  subgrp_re-40)*/
/*proc sql;*/
/*select * from out.Ck_crosstb_summary_merge_revise*/
/*where group=18 and cluster=1;*/
/*quit;*/

/*data out.Ck_crosstb_summary_merge_revise1 test;*/
/*retain new_subgrp_re group cluster freq;*/
/*set out.Ck_crosstb_summary_merge_revise;*/
/*if group=18 and cluster=1 and new_subgrp_re=40 and count_sum_with_month_Besuch=1 then do;*/
/*new_subgrp=new_subgrp+1 and new_subgrp_re=new_subgrp_re+1;*/
/*group_cls_subgrp=.;*/
/*new_subgrp=new_subgrp+1;*/
/*new_subgrp_re=new_subgrp_re+1;*/
/*output;*/
/*end;*/
/*drop group_cls_subgrp;*/
/*run;*/

/*proc sql;*/
/*select * from out.Ck_crosstb_summary_merge_revise1*/
/*where group=18 and cluster=1;*/
/*quit;*/

/*now manually change the new_subgrp_re and retrun the macro statTb4revise again*/
/*%statTb4revised(input=out.Ck_crosstb_summary_merge_revise1*/
/*				  , out=statTb4merge_revise1*/
/*)*/


%macro merge_all_part(promo_data, var2grp, var, input1, input2, input3, output1, output, out4freq);
data ttt;
set &input2.;
/*if _n_ in (108 109) then delete;*/
/*if _n_ = 107 then freq=freq+2;*/
run;
proc sql;
create table temp1 as
select b.specialty1_merge, b.&var2grp., b.group, b.cluster,  b.freq, a.new_subgrp, a.new_subgrp_re, a.count_in_subgrp
,b.mean as mean_in_cluster, b.std as std_in_cluster, b.min as min_in_cluster, b.max as max_in_cluster
,a.mean as mean_in_subgrp, a.std as std_in_subgrp, a.min as min_in_subgrp, a.max as max_in_subgrp
 
from &input2. b left join &input1. a
on a.group=b.group and a.cluster=b.cluster;
quit;

/*data temp2;*/
/*set &input3.;*/
/*if subgrp_num < 2;*/
/*run;*/

proc sql;
select count(*) into: n from &input3.;
quit;

%do i=1 %to &n.;
data _null_;
set &input3.;
if _n_=&i.then do;
	call symput("group", grp_flag);
	call symput("spec", specialty1_merge);
	call symput("county", &var2grp.);
end;
run;
%let spec=%sysfunc(left(%sysfunc(trim(&spec))));
%let county=%sysfunc(left(%sysfunc(trim(&county))));
%put &spec. &county.;

proc means data=&promo_data.(where=(specialty1_merge="&spec." and  &var2grp.="&county.")) nway;
  var &var.;
  output out=temp3(drop=_FREQ_ _TYPE_) mean=mean std=std min=min max=max;
run;

data temp3;
retain group;
set temp3;
group=&group.;
run;

%if &i.=1 %then %do;
data &output1.;
set temp3(obs=0);
run;

%end;
	proc datasets library=work nolist;
	   append base=&output1. data=temp3 force;
	run;

%end;
data &output1.;
set &output1.;
array stat mean std min max;
do over stat;
	if stat=. then stat=0;
	stat=round(stat, .01);
end;
run;

proc sql;
create table &output1. as
select a.specialty1_merge, a.&var2grp., b.group, a.freq, b.mean, b.std, b.min, b.max
from &input3. a left join &output1. b
on a.grp_flag = b.group;
quit;

proc sql;
create table &output. as
select a.specialty1_merge, a.&var2grp., a.group, b.cluster, b.new_subgrp, b.new_subgrp_re, a.freq as count_in_group, b.freq as count_in_cluster, b.count_in_subgrp
, a.mean as mean_in_group, a.std as std_in_group, a.min as min_in_group, a.max as max_in_group
, b.mean_in_cluster, b.std_in_cluster, b.min_in_cluster, b.max_in_cluster
, b.mean_in_subgrp, b.std_in_subgrp, b.min_in_subgrp, b.max_in_subgrp
from &output1. a full join temp1 b
on a.group=b.group;
quit;

data &out4freq.;
retain specialty1_merge &var2grp. segment count mean std min max;
set &output.;
array for_seg count mean std min max;
array for_grp count_in_group mean_in_group std_in_group min_in_group max_in_group;
array for_cls count_in_cluster mean_in_cluster std_in_cluster min_in_cluster max_in_cluster;
array for_subgrp count_in_subgrp mean_in_subgrp std_in_subgrp min_in_subgrp max_in_subgrp;
do over for_seg;
	if cluster=. then for_seg=for_grp;
	if cluster^=. and new_subgrp=. then for_seg=for_cls;
	if cluster^=. and new_subgrp^=. then for_seg=for_subgrp;
end;
segment = _n_;
keep specialty1_merge &var2grp. segment count mean std min max group cluster new_subgrp new_subgrp_re;
run;

	proc export data=&out4freq.
	outfile="&outpath.\&out4freq..csv"
	dbms=csv replace;
	run;

	proc export data=&output.
	outfile="&outpath.\&output..csv"
	dbms=csv replace;
	run;


%mend;
/*revise the table "out.Stat_table_for_cluster_result"*/
/*data out.stat_table_for_cluster_revise;*/
/*set out.stat_table_for_cluster_result;*/
/*if _n_ in (108 109) then delete;*/
/*if _n_ = 107 then freq=freq+2;*/
/*run;*/

%merge_all_part(promo_data=dir_ge.for_cluster_v2
				, var2grp=num_rcv_promo_merge
				, var=spend
				, input1=out.stattb4merge_revise
				, input2=out.stat_table_spend
				, input3=out.grp_ref_tb
				, output1=out.stat_table_for_grp
				, output=out.stat_table
				, out4freq=out.final_freq_in_seg);

/*qc*/
ods html;	

/*data out.final_freq_in_seg;*/
/*set out.final_freq_in_seg;*/
/*if _n_ in (128 129) then delete;*/
/*if _n_ =127 then count=count+2;*/
/*run;	*/
proc sql;
select max(count), min(count) from out.final_freq_in_seg /*1373 11 */
union
select max(count), min(count) from out.final_freq_in_seg where std>0 /*984 11*/
union
select max(count), min(count) from out.final_freq_in_seg where cluster>0 and std>0 /*984 11 */
union
select max(count), min(count) from out.final_freq_in_seg where new_subgrp>0 and std>0 /*984 11*/
union
select sum(count) from out.final_freq_in_seg;/*39431*/
quit;
/*184 71 new_subgrp>0 and std>0
199 18 std>0
199 71 cluster>0 and std>0
510 18 
39431 .
*/
proc sql;
select max(count), min(count) from out.final_freq_in_seg where new_subgrp^=. and std>0 /**/
;
run;
%my_uni(out.final_freq_in_seg, var=count, where=%nrstr(count>0));
%my_uni(out.final_freq_in_seg, var=count, where=%nrstr(count>200 and std=0));
%my_uni(out.final_freq_in_seg, var=count, where=%nrstr(count>200 and std>0));
%my_uni(out.final_freq_in_seg, var=count, where=%nrstr(count>0 and count<250));
%my_uni(out.final_freq_in_seg, var=count, where=%nrstr(count>=100 and count<=200)); /*26896*/


ods html;
proc sql;
select * from out.final_freq_in_seg
where std>0 and count>200;
quit;
proc chart data=out.final_freq_in_seg;
vbar segment;
run;

%macro check_cluster_4overlap(parse, cluster_var, var, output);
data vmember_flag;
set sashelp.vmember;
re = PRXPARSE(&parse.); /*CHECK_CROSSTB_42_FREQ308_MERGE*/
flag=prxmatch(re, left(trim(memname)));
flag1=prxmatch(PRXPARSE("/^work$/i"), left(trim(libname)));

if flag=1 then do;
	group=input(prxposn(re, 1, left(trim(memname))), 3.);
end;

run;

Proc sql;
	create table dataset_list as
     select memname, group
     from vmember_flag
     where flag=1 and flag1=1
  ;
quit;

proc sql;
select count(*) into: n from dataset_list;
quit;
%put &n. ;

%do i=1 %to 
&n.
;
data _null_;
set dataset_list;
if _n_=&i. then do;
	call symput('dataset_name', memname);
	call symput('group', group);
end;
run;
%let group=%sysfunc(left(%sysfunc(trim(&group.))));
proc sql;
create table test_cluster&group. as
select &group. as group, &cluster_var., max(&var.) as max
, min(&var.) as min from &dataset_name. 
group by &cluster_var.
order by max;
quit;

data overlap;
set test_cluster&group;
max_previous=lag(max);
if max_previous>=min;
run;

%if i=1 %then %do;
	data test_cluster_overlap;
	set overlap(obs=0);
	run;
%end;
	proc datasets library=work nolist;
	   append base=test_cluster_overlap data=overlap force;
	run;
%end;

%mend;

%check_cluster_4overlap(
parse=%nrstr("/^rnk_out(\d+)$/i")
, cluster_var=rank_spend
, var=spend
, output=test_cluster_overlap
);

proc sql;
select max(count) from out.final_freq_in_seg
where new_subgrp=.;
quit;
%macro futher_split_step4(input, input1, output, var, size_cutoff, in_prefix);
proc sql;
create table temp1 as
select *
from &input.
where count>&size_cutoff.;
quit;

proc sql;
select count(*) into: n from temp1;
quit;
%put &n.;

%do i=1 %to 
&n.
;
data _null_;
set temp1;
if _n_=&i. then do;
	call symput("spec", specialty1_merge);
	call symput("num_channel", num_rcv_promo_merge);
	call symput("group", group);
	call symput("cluster", cluster);
	call symput("subgrp", new_subgrp);
end;
run;

%let spec=%sysfunc(left(%sysfunc(trim(&spec))));
%let num_channel=%sysfunc(left(%sysfunc(trim(&num_channel))));
%let group=%sysfunc(left(%sysfunc(trim(&group))));
%let cluster=%sysfunc(left(%sysfunc(trim(&cluster))));
%let subgrp=%sysfunc(left(%sysfunc(trim(&subgrp))));
%put &group. &cluster &subgrp.;
proc sql;
create table temp2 as
select &group as group, &cluster as cluster, &subgrp as subgrp, &var., count(*) as count_in_&var.
from &input1.
where id in
(select obs_id from &in_prefix.&group._cls&cluster._subgrp&subgrp.)
group by &var.
;
quit;

%if &i=1 %then %do;
data &output.;
set temp2(obs=0);
run;
%end;
proc datasets library=work nolist;
append base=&output. data=temp2 force;
run;
%end;

%mend;
%futher_split_step4(
input=out.final_freq_in_seg
, input1=dir_ge.for_cluster_v2
, output=split_by_county_result
, var=county
, size_cutoff=500
, in_prefix=dt_grp
);

%my_uni(input=split_by_county_result, var=count_in_county, where=%nrstr(count_in_county>0));

%macro split_step4_random(input, output, in_prefix, size_cutoff);
proc sql;
create table temp1 as
select *, ceil(count/&size_cutoff) as num_grp_rnd
from &input.
where ceil(count/&size_cutoff)>1;
quit;

proc sql;
select count(*) into: n from temp1;
quit;
%put &n.;
%do i=1 %to 
&n.
;

data _null_;
set temp1;
if _n_=&i. then do;
	call symput("spec", specialty1_merge);
	call symput("num_channel", num_rcv_promo_merge);
	call symput("group", group);
	call symput("cluster", cluster);
	call symput("subgrp", new_subgrp);
	call symput("num2split", num_grp_rnd);
	call symput("cnt", count);
end;
run;

%let spec=%sysfunc(left(%sysfunc(trim(&spec))));
%let num_channel=%sysfunc(left(%sysfunc(trim(&num_channel))));
%let group=%sysfunc(left(%sysfunc(trim(&group))));
%let cluster=%sysfunc(left(%sysfunc(trim(&cluster))));
%let subgrp=%sysfunc(left(%sysfunc(trim(&subgrp))));
%let num2split=%sysfunc(left(%sysfunc(trim(&num2split))));
%let cnt=%sysfunc(left(%sysfunc(trim(&cnt))));
	

%put &group. &cluster &subgrp.&num2split.&cnt.;

      proc multtest data=&in_prefix.&group._cls&cluster._subgrp&subgrp. perm n=1 outsamp=multperms seed=36607 noprint;
         class spend;
         test mean(obs_id);
         run;

data temp2;
do i=1 to &num2split.;
	do j=1 to ceil(&cnt/&num2split);
		output;
	end;
end;
run;
data temp3 ;
merge multperms temp2;
keep obs_id i;
/*where obs_id ^=.;*/
run;

data temp3;
set temp3;
where obs_id ^=.;
run;

proc sql;
create table &in_prefix.&group._cls&cluster._subgrp&subgrp. as
select &group as group, &cluster as cluster, &subgrp as subgrp, a.spend, a.obs_id, b.i
from &in_prefix.&group._cls&cluster._subgrp&subgrp. a left join temp3 b
on a.obs_id=b.obs_id;
quit;
/*proc freq data=temp4; table i;run;*/

proc means data=&in_prefix.&group._cls&cluster._subgrp&subgrp. noprint;
  id group cluster subgrp;
  var spend;
  class i;
  output out=temp5(drop=_FREQ_ _TYPE_ where=(i ^=.)) mean=mean std=std min=min max=max;
run;
proc sql;
create table temp6 as
select group, cluster, subgrp, i, count(*) as count_in_rnd
from &in_prefix.&group._cls&cluster._subgrp&subgrp.
group by group, cluster, subgrp, i;
quit;
proc sql;
create table temp7 as
select a.*, b.count_in_rnd
from temp5 a left join temp6 b
on a.group=b.group and a.cluster=b.cluster and a.subgrp=b.subgrp and a.i=b.i;
quit;

/*test*/
proc sql;
select sum(count_in_rnd) into: n_split from temp7;
quit;
data temp8;
retain org split;
set temp7(obs=1);
org=&cnt.;
split=&n_split;
if org=split then eq=1;else eq=0;
keep org split eq;
run;

/*test end*/
%if &i.=1 %then %do;
data out.&output.;
set temp7(obs=0);
run;

data check;
set temp8(obs=0);
run;
%end;
proc datasets library=work nolist;
append base=out.&output. data=temp7 force;
run;

proc datasets library=work nolist;
append base=check data=temp8 force;
run;

%end;
data out.&output.;
set out.&output.;
array vars mean std;
do over vars;
vars=round(vars, .01);
end;
rename i=subgrp_rnd;
run;
%mend;
%split_step4_random(
input=out.final_freq_in_seg
, output=stattb4rnd
, in_prefix=dt_grp
, size_cutoff=200
);

%macro merge_with_rnd_part(input1, input2, output);
proc sql;
create table out.&output. as
select a.*, b.subgrp_rnd, b.count_in_rnd, b.mean as mean_rnd, b.std as std_rnd, b.min as min_rnd, b.max as max_rnd
from &input1 a left join &input2. b
on a.group=b.group and a.cluster=b.cluster and a.new_subgrp=b.subgrp;
quit;
data out.&output.;
retain specialty1_merge num_rcv_promo_merge segment group cluster new_subgrp subgrp_rnd count;
set out.&output.(drop=segment);
array org mean std min max;
array rnd mean_rnd std_rnd min_rnd max_rnd;
if subgrp_rnd ne . and subgrp_rnd > 0 then do;
	do i=1 to 4;
		org[i]=rnd[i];
	end;
	count=count_in_rnd;
end;
segment=_n_;
drop i new_subgrp_re mean_rnd std_rnd min_rnd max_rnd count_in_rnd;
run;

proc export data=out.&output.
outfile="&outpath.\&output..csv"
dbms=csv replace;
run;
%mend;
%merge_with_rnd_part(input1=out.Final_freq_in_seg, input2=out.stattb4rnd, output=final_freq_in_seg_v2);
/*qc*/
proc sql;
select sum(count) from out.final_freq_in_seg_v2;
quit;
/*39431*/
%my_uni(input=out.final_freq_in_seg_v2, var=count, where=%nrstr(count>0));

proc sql;
select sum(count) from out.final_freq_in_seg
where count>200
union
select sum(count_in_rnd) from out.stattb4rnd
;
quit;
/*10394*/


%macro extract_obs_for_seg(input1, input2, prefix, var, output, output1);
proc sql;
select count(*) into: n from &input1.;
quit;

%do i=1 %to 
&n.
;
data _null_;
set &input1.;
if _n_=&i. then do;
	call symput("seg", segment);
	call symput("spec", specialty1_merge);
	call symput("county", county_text);
	call symput("group", group);
	call symput("cluster", cluster);
	call symput("subgrp", new_subgrp);
	call symput("subgrp_rnd", subgrp_rnd);

end;
	
run;
%let spec=%sysfunc(left(%sysfunc(trim(&spec))));
%let county=%sysfunc(left(%sysfunc(trim(&county))));
%let seg=%sysfunc(left(%sysfunc(trim(&seg))));
%let group=%sysfunc(left(%sysfunc(trim(&group))));
%let cluster=%sysfunc(left(%sysfunc(trim(&cluster))));
%let subgrp=%sysfunc(left(%sysfunc(trim(&subgrp))));
%let subgrp_rnd=%sysfunc(left(%sysfunc(trim(&subgrp_rnd))));

%put &spec. &county. &seg. &group. &cluster. &subgrp. &subgrp_rnd;

%if &cluster=. %then %do;
	data temp1;
/*	length obs  20;*/
	set &input2.;
	obs=id;
	segment=&seg.;
	where specialty1_merge="&spec" and county_text="&county.";
	keep obs segment;
	run;
%end;
%if &subgrp=. and &cluster^=. %then %do;
	data temp1;
/*	length obs 12;*/
	set &prefix.&group./*(rename=(_name_=obs))*/;
	segment=&seg.;
	obs=id;
/*	rename _name_=obs;*/
	where &var.=&cluster.;
	keep obs segment;
	run;
%end;
%if &subgrp^=. and &subgrp_rnd = . %then %do;
	data temp1;
/*	length obs  8;*/
	set dt_grp&group._cls&cluster._subgrp&subgrp./*(rename=(obs_id=obs))*/;
	obs=obs_id;
/*	rename obs_id=obs;*/
	segment=&seg.;
	keep obs segment;
	run;
%end;
%if &subgrp_rnd^=. %then %do;
	data temp1;
/*	length obs  8;*/
	set dt_grp&group._cls&cluster._subgrp&subgrp./*(rename=(obs_id=obs))*/;
	obs=obs_id;
/*	rename obs_id=obs;*/
	segment=&seg.;
	where i=&subgrp_rnd.;
	keep obs segment;
	run;


%end;
%if &i=1 %then %do;
	data &output.;
	set temp1(obs=0);
	run;
%end;
proc datasets library=work nolist;
	append base=&output. data=temp1 force;
run;
%end;
/*data &output.;*/
/*set &output.;*/
/*re=prxparse("/^\wb(\d+)$/i");*/
/*flag=prxmatch(re, left(trim(obs)));*/
/*if flag then do;*/
/*obs_num=input(prxposn(re, 1, left(trim(obs))), 5.);*/
/*end;*/
/*keep obs_num segment;*/
/*run;*/
proc sql;
create table out.&output1. as
select a.*, b.segment 
from &input2. a left join &output. b
on a.id=b.obs;
quit;

proc export data=out.&output1.
outfile="&outpath.\&output1..csv"
dbms=csv replace;
run;
%mend;

%extract_obs_for_seg(input1=out.final_freq_in_seg_v2
, input2=dir_ge.for_cluster_v2
, prefix=rnk_out_merge
, var=rank_spend
, output=out.id_seg
, output1=addSeg
);

/*qc*/
proc sql;
/*create table a as*/
select count(distinct obs) from out.id_seg
where obs ^=.;
quit;
/*39431*/
proc sql;
create table out.addSeg as
select a.*, b.segment 
from dir_ge.for_cluster_v2 a left join out.id_seg b
on a.id=b.obs;
quit;



/*qc*/
proc sql;
select count(*) from out.addSeg
where segment=.;
quit;
/*0*/


/*further split the large segments into small ones using other promo variables*/
%let size_cutoff=307;

%macro further_split_largesize(size_cutoff, input, input1, output);
data temp;
set &input.;
if count >= &size_cutoff.;
run;
proc sql;
select count(*) into: n from temp;
quit;
%do i=1 %to &n.;
	data _null_;
	set temp;
	if _n_=&i. then do;
		call symput("segment", segment);
		call symput("group", group);
		call symput("cluster", cluster);
		call symput("subgrp", new_subgrp);
	end;
	run;
	%let segment=%sysfunc(left(%sysfunc(trim(&segment))));
	%let group=%sysfunc(left(%sysfunc(trim(&group))));
	%let cluster=%sysfunc(left(%sysfunc(trim(&cluster))));
	%let subgrp=%sysfunc(left(%sysfunc(trim(&subgrp))));
	data temp1;
	set dt_grp&group._cls&cluster._subgrp&subgrp.;
	obs=input(obs_id, 12.);

	run;

	proc sql;
	create table dt4seg&segment. as
	select &segment. as segment, * from &input1.
	where  id in (select obs from temp1);
	quit;
/*	*/
/*	%my_univariate(input=dt4seg&segment., var=&allPromoVars.)*/
	%if &i.=1 %then %do;
	data out.&output.;
	set dt4seg&segment.(obs=0);
	run;
	%end;
	proc datasets library=work nolist;
	append base=out.&output. data=dt4seg&segment. force;
	run;

%end;

%mend;
%let allPromoVars=
count_sum_with_month_Besuch
count_sum_with_month_Digital 
count_sum_with_month_Telefonkont 
count_sum_with_month_VA_Teilnahm
count_sum_with_month_Veranstaltu 
sum_Besuch 
sum_Digital 
sum_Telefonkontakt
sum_VA_Teilnahme 
sum_Veranstaltungskontakt
;

%further_split_largesize(
size_cutoff=&size_cutoff
, input=out.final_freq_in_seg
, input1=dir_ge.for_cluster
, output=dt4furtherCls
)
;
/*qc*/
ods html;
proc freq data=out.dt4furtherCls;
table segment;
run;

%macro my_univariate(input,var,class);
ODS HTML path="&outpath."
BODY = "distribution of segmentation size.htm"; 
/*ods graphics on;*/
proc univariate data = &input.;
HISTOGRAM &var.   /NORMAL(color=red) CFILL = ltgray CTEXT = blue ;
class &class.;
INSET N = 'Number of Physicians' MEDIAN (8.2) MEAN (8.2) STD='Standard Deviation' (8.3)/ POSITION = ne;
run;

/*PROC SGPLOT DATA = &input.;*/
/* HISTOGRAM &var.;*/
/* TITLE "Olympic Men's Swimming Freestyle 100";*/
/*RUN; */

ODS HTML close;
/*ods graphics off;*/
%mend;

%my_univariate(input=out.dt4furtherCls,var=&allPromoVars.,class=segment);



%macro further_split_4largeseg(data,cellsize,varlist,out,out1, out2,method);
proc freq data=&data.;
table segment /out=freq_tb;
run;
data freq_tb;
set freq_tb;
n_cls=ceil(count/&cellsize.);
run;
proc sql;
select count(*) into: n from freq_tb;
quit;
%put &n.;
%do i=1 %to 
&n.
;
data _null_;
set freq_tb;
if _n_=&i. then do;
	call symput("seg", segment);
	call symput("n_cls", n_cls);
end;
run;
%let seg=%sysfunc(left(%sysfunc(trim(&seg.))));
%put &seg. &n_cls.;

data temp;
set &data.;
if segment=&seg.;
run;

	proc cluster data = temp outtree = output_tree1 method = &method. noprint;
var  &varlist.;
id id;

run;

proc tree data =output_tree1   out=out_seg&seg. n=&n_cls. dis;
copy &varlist.;
run;

proc means data=out_seg&seg. nway;
var &varlist.;
class cluster;
output out=temp5(drop=_type_ rename=(_freq_=count_in_cluster)) 
mean=/*%foreach(v, &varlist., %nrstr(&v._m)) */
std=/*%foreach(v, &varlist., %nrstr(&v._std))*/
min=/*%foreach(v, &varlist., %nrstr(&v._min))*/
max=/*%foreach(v, &varlist., %nrstr(&v._max))*/ 
/autoname
;
run;

data temp5;
retain segment cluster count_in_cluster 
:Mean :stddev :min :max
/*%foreach(v, &varlist., %nrstr(&v._m))*/
/*%foreach(v, &varlist., %nrstr(&v._std))*/
/*%foreach(v, &varlist., %nrstr(&v._min))*/
/*%foreach(v, &varlist., %nrstr(&v._max))*/

;
set temp5;
segment=&seg.;
run;
%do j=1 %to &n_cls.;
proc freq data=out_seg&seg.(where=(cluster=&j.));
table 
count_sum_with_month_Digital 
*count_sum_with_month_Telefonkont 
*count_sum_with_month_VA_Teilnahm
*count_sum_with_month_Veranstaltu 
/*sum_Digital */
/**sum_Telefonkontakt*/
/**sum_VA_Teilnahme */
/**sum_Veranstaltungskontakt*/
/out=temp3;
run;
data temp3;
set temp3;
segment=&seg.;
cluster=&j.;
run;
%if &j=1 %then %do;
	data temp4;
	set temp3(obs=0);
	run;
%end;
proc datasets library=work nolist;
append base=temp4 data=temp3 force;
run;
%end;
	proc sql;
	create table temp2 as
	select 	&seg. as segment, cluster, count(*) as cnt
	from out_seg&seg.
	group by cluster;
	quit;

	proc sort data=temp2;
		by cnt;
	run;
	
	%if &i.=1 %then %do;
	data out.&out.;
	set temp2(obs=0);
	run;

	data out.&out1.;
	set temp4(obs=0);
	run;

	data out.&out2.;
	set temp5(obs=0);
	run;
	%end;
	proc datasets library=work nolist;
	   append base=out.&out. data=temp2 force;
	run;
	proc datasets library=work nolist;
	append base=out.&out1. data=temp4 force;
	run;
	proc datasets library=work nolist;
	append base=out.&out2. data=temp5 force;
	run;
%end;
	data out.&out1.;
	retain segment cluster;
	set out.&out1.;
	run;
	data out.&out2.;
	set out.&out2.;
/*	array v_mean :_mean  ;*/
/*	array v_std :_stddev ;*/
/*	do over v_mean;*/
/*	v_mean=round(v_mean, .01);*/
/*	end;*/
/*	do over v_std;*/
/*	v_std=round(v_std, .01);*/
/*	end;*/
	array vars _all_;
	do over vars;
	vars=round(vars, .01);
	end;
	run;
	proc export data=out.&out.
	outfile="&outpath.\&out..csv"
	dbms=csv replace;
	run;
		proc export data=out.&out1.
	outfile="&outpath.\&out1._for_large_segment.csv"
	dbms=csv replace;
	run;
			proc export data=out.&out2.
	outfile="&outpath.\&out2..csv"
	dbms=csv replace;
	run;

%mend;
%let promo4furtherCls=
count_sum_with_month_Digital 
count_sum_with_month_Telefonkont 
count_sum_with_month_VA_Teilnahm
count_sum_with_month_Veranstaltu 
/*sum_Digital */
/*sum_Telefonkontakt*/
/*sum_VA_Teilnahme */
/*sum_Veranstaltungskontakt*/
;
%further_split_4largeseg(
data=out.dt4furtherCls
,cellsize=&cellsize.
,varlist=&promo4furtherCls.
,out=for_large_segment
,out1=freq_check
,out2=stat_for_further_cluster
, method=ward
)
;

proc means data=out.dt4furtherCls nway;
var &promo4furtherCls.;
class segment cluster;
output out=stat_table_4largeSeg mean= std= min= max= /autoname;
run;

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
	create table dataset_list as
     select memname
     from sashelp.vmember
     where libname = "WORK " and memname like "CHECK_CROSSTB_%"
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

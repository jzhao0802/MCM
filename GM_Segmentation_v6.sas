libname dir_ge "C:\work\working materials\MCM\GM\data";
%let dataPath = C:\work\working materials\MCM\GM\data;

%let date=Jan11_1;
libname out "C:\work\working materials\MCM\GM\&date.";
%let outpath = C:\work\working materials\MCM\GM\&date.;
%put &outpath.;

%let cellsize=200;

proc import out=dir_ge.gm_promo_onekey_data
datafile="&dataPath./MSD_Promo_OneKey_GE_cmp.xlsx"
dbms=xlsx replace;
GETNAMES=YES;
/*sheet="selected";*/
RANGE='complete data act+OK$A1:LP51023';
/*datarow=10001;*/
run;

data temp82;
set dir_ge.gm_promo_onekey_data(obs=2);
keep IDENT -- erm_chtigt;
run;
/*proc transpose data=temp82 out=temp81;*/
/*id ident;*/
/*run;*/

data temp1;
set temp;
re = PRXPARSE("/(.+) (2014|2015)-(\d+)/");
if prxmatch(re, _label_) then do;
promo=prxposn(re, 1, _label_);
year=input(prxposn(re, 2, _label_), 10.);
month=input(prxposn(re, 3, _label_), 10.);
end;
run;

proc sort data=temp1 out=temp2;
by promo year month;
run;

data temp3;
set temp2;
re=PRXPARSE("/VA/i");
if prxmatch(re, promo) then do;
promo = 'VA-Teilnahme';
end;
re1=PRXPARSE("/E-Mail|webinar/i");
if prxmatch(re1, promo) then do;
promo = 'Digital';
end;
/*keep promo re1;*/
run;

proc sql;
create table temp4 as
select col: from temp3(where=(promo ^= .))
group by promo year month; 
quit;

proc sort data=temp3 out=temp3;
by promo year month;
run;
proc means data=temp3 sum noprint;
var col:;
by promo year month;
output out=temp4(where=(promo^='')) sum=;
run;

data temp5;
length promo_date $ 50.;
set temp4;
promo_date=catx('_', promo, year, month);
/*drop promo year month promo_date;*/
run;

proc transpose data=temp5 out=temp6(drop=_name_);
id promo_date;
run;


data temp7;
set temp6;
if _n_>4;
run;

data temp8;
set dir_ge.gm_promo_onekey_data;
keep CUST_ID_EXT_GENESYS Gesamtergebnis IDENT--erm_chtigt; 
run;

proc transpose data=dir_ge.gm_promo_onekey_data(obs=2) out=temp_allonekey;
run;

data promo_onekey_merged dir_ge.promo_onekey_merged;
merge temp8 temp7;
run;

data promo_onekey_merged_0act promo_onekey_merged_act;
set dir_ge.promo_onekey_merged;
if Gesamtergebnis=0 then output promo_onekey_merged_0act;
else output  promo_onekey_merged_act;
run;
/*11591 vs 39431
*/

data temp11;
set temp7;
array vars _all_;
do over vars;
if vars=. then vars=0;else vars=1;
end;
run;

/*get the promo count with month*/
data temp21;
set temp5;
array vars col:;
do over vars;
if vars>1 then vars=1;
if vars=. then vars=0;
end;
run;

proc means data=temp21 sum noprint;
var col:;
by promo;
output out=temp22(where=(promo ^= '')) sum=;
run;

data temp22;
set temp22;
promo=catx('-', 'count sum with month', promo);
keep promo col:;
run;

proc transpose data=temp22 out=temp23;
id promo;
run;
/*get the promo sum across all months*/
data temp31;
set temp5;
array vars col:;
do over vars;
if vars=. then vars=0;
end;

run;

proc means data=temp31 sum noprint;
var col:;
by promo;
output out=temp32(where=(promo^='')) sum=;
run;

data temp32;
set temp32;
promo=catx('-', 'sum', promo);
keep promo col:;
run;

proc transpose data=temp32 out=temp33;
id promo;
run;

proc transpose data=temp8(obs=2) out=temp81;
run;

data for_cluster dir_ge.for_cluster;
retain id;
length specialty1_merge $ 50;
merge temp8(keep=id Gesamtergebnis county county_text IMS_Brick_Landscape specialty1) temp23(drop=_name_) temp33(drop=_name_);
re = PRXPARSE("/Allgemeinmedizin|Innere Medizin|Praktischer Arzt/i");
if ^prxmatch(re, specialty1) then specialty1_merge='Rest';else specialty1_merge=specialty1;
if Gesamtergebnis > 0;
run;

proc freq data=for_cluster;
table specialty1 specialty1_merge;
run;

proc sql;
select count(distinct id) from temp8;
quit;
/*51022*/

proc sql;
create table grp_ref_tb as
select distinct specialty1_merge, county_text, count(*) as freq
from dir_ge.for_cluster
group by specialty1_merge, county_text;
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

%macro get_cluster_output(data,i,varlist,out,n_clst, method, outfile, lib);
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
%global m_subgrp_num m_specialty1_merge m_county_text;
%do i = 1 %to 
&grp_num.
;
	data temp;
		set &ref_tb.;
		if _n_=&i. then do;
/*			re=PRXPARSE("/no_1k/i");*/
/*			b_no_1k = prxmatch(re, msd_seg);*/

			call symput('m_specialty1_merge', specialty1_merge);
			call symput('m_county_text', county_text);
			call symput('m_subgrp_num', subgrp_num);
/*			call symput('m_b_no_1k', b_no_1k);*/
		end;
	run;
	%let m_subgrp_num = %sysfunc(putn(&m_subgrp_num, 3.0));
	%put &m_subgrp_num. &m_specialty1_merge &m_county_text;
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
			if Specialty1_merge="&m_specialty1_merge." and county_text="&m_county_text.";
/*			msd_test=symget("m_msd_seg");*/
/*			flag=(msd_test=msd_seg);*/
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
				, outfile=cluster_out_grp_&i._&m_specialty1_merge._&m_county_text.
				,lib=&lib.
				); 
	%end;	
%end;
%mend;

proc contents data=dir_ge.for_cluster noprint
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
count_sum_with_month_Besuch
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
%cluster_by_grp(input=dir_ge.for_cluster
				, ref_tb=out.grp_ref_tb
				, vars2cluster=&vars2cluster.
				, lib=&lib.
				, std=&std.
				);

				
proc freq data=dir_ge.for_cluster;
table &allVars2cluster /cumcol;
run;

%macro getStatTb4clsRst(input, outfile);
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
	call symput('cluster', cluster);
	call symput('freq', cnt);
end;
run;
%let group=%sysfunc(putn(&group., 3.0));
%let cluster=%sysfunc(putn(&cluster., 3.0));
%let freq=%sysfunc(putn(&freq., 3.0));
%put &group. &cluster. &freq.;
data temp1;
set out&group.;
if cluster=&cluster.;
run;

proc freq data=temp1;
table count_sum_with_month_Besuch /cumcol
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
  var count_sum_with_month_Besuch;
  output out=temp3(drop=_FREQ_ _TYPE_) mean=mean std=std min=min max=max;
run;

%if i=1 %then %do;
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
if std=. then std=0;
std=round(std, .01);
run;
proc sort data=out.&outfile.;by group cluster;run;

proc sql;
create table out.&outfile. as
select b.specialty1_merge, b.county_text, a.* from
out.&outfile. a left join out.grp_ref_tb b
on a.group=b.grp_flag;
quit;
	proc export data=out.&outfile.
	outfile="&outpath.\&outfile..csv"
	dbms=csv replace;
	run;

%mend;
%getStatTb4clsRst(input=For_large_cluster_check, outfile=stat_table_for_cluster_result);

%macro check_large_cluster(input, threshold, outfile, std);
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
	call symput('cluster', cluster);
	call symput('freq', cnt);
end;
run;
%let group=%sysfunc(putn(&group., 3.0));
%let cluster=%sysfunc(putn(&cluster., 3.0));
%let freq=%sysfunc(putn(&freq., 3.0));
%put &group. &cluster. &freq.;
data dt2check_grp&group._cls&cluster.;
set out&group.;
if cluster=&cluster.;
run;

proc freq data=dt2check_grp&group._cls&cluster.;
table count_sum_with_month_Besuch /cumcol
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
retain group freq ;
set check_crossTb_&group._cls&cluster.;
group=&group.;
cluster=&cluster.;
freq=&freq.;
run;

%if &i.=1 %then %do;
	data out.check_crossTb_summary;
/*	retain cumfreq;*/
	set check_crossTb_&group._cls&cluster.(obs=0);
/*	cumfreq=freq;*/
	run;
%end;
proc datasets library=work nolist;
	append base=out.check_crossTb_summary data=check_crossTb_&group._cls&cluster. force;
run;
%end;
data out.check_crossTb_summary;
set out.check_crossTb_summary;
group_cls=catx('_', group, cluster);
run; 

proc sort data=out.check_crossTb_summary;by group_cls;run;

data out.check_crossTb_summary;
retain cumfreq 0;
set out.check_crossTb_summary;
by group_cls;
if first.group_cls then cumfreq=count;else cumfreq=cumfreq+count;
drop group_cls;
run;
proc sort data=out.check_crossTb_summary;by group freq;run;

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

%mend;

%check_large_cluster(
						input=for_large_cluster_check
						, threshold=&cellsize.
						, outfile=large_cluster_check_gt
						, std=&std.
						);


%macro merge_small_subgrp_step1();
data vmember_flag;
set sashelp.vmember;
re = PRXPARSE("/^check_crosstb_\d+_cls\d+$/i");
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
retain new_subgrp 1;
retain subgrp_end 0;
retain cumfreq 0;
set &dataset_name.;
if subgrp_end=1 then new_subgrp=new_subgrp+1;
cumfreq=cumfreq+count;
if cumfreq < 100 then do;
/*new_subgrp=new_subgrp;*/
subgrp_end=0;
end;
if cumfreq>=100 and cumfreq<200 then do;
/*new_subgrp=new_subgrp;*/
cumfreq=0;
subgrp_end=1;

end;
if cumfreq>200 then do;
/*new_subgrp=new_subgrp+1;*/
subgrp_end=1;
cumfreq=count;
end;
drop cumfreq subgrp_end;
run;


%end;

%if &i.=1 %then %do;
	data out.check_crossTb_summary_merge;
	set &dataset_name._merge(obs=0);
	run;
%end;
proc datasets library=work nolist;
	append base=out.check_crossTb_summary_merge data=&dataset_name._merge force;
run;

%end;

proc sql;
create table out.check_crossTb_sumby_newsubgrp as
select group, cluster, new_subgrp, sum(count) as sumby_subgrp
from out.check_crossTb_summary_merge
group by group, cluster, new_subgrp;
quit;
proc export data=out.check_crossTb_sumby_newsubgrp
outfile="&outpath./check_crossTb_sumby_newsubgrp(need to merge small subgrp to neighbour).csv"
dbms=csv replace;
run;


%mend;
%merge_small_subgrp_step1();

%macro merge_small_subgrp_step2_error();
proc import out=subgrp_after_manu_revise
datafile="&outpath./check_crossTb_sumby_newsubgrp(with merging for small subgrp to neighbour).csv"
dbms=csv replace;
run;

proc sql;
select count(*) into: n from subgrp_after_manu_revise;
quit;

%do i = 1 %to 1
/*&n.*/
;
data _null_;
set subgrp_after_manu_revise;
if _n_ = &i. then do;
	call symput('group', left(trim(group)));
	call symput('freq', left(trim(freq)));
	call symput('subgrp', left(trim(new_subgrp)));
end;
run;
%let group=%sysfunc(inputn(&group., 3.));
%let freq=%sysfunc(inputn(&freq., 3.));
%put &group &freq &subgrp;
%let re=PRXPARSE(/(\d+)*(\d+)/i);
%put &re.;
%if %sysfunc(prxmatch(&re., &subgrp.)) %then %do;
/*	%let mt=%sysfunc(prxmatch(&re., ));*/
	%let subgrp1=%sysfunc(prxposn(&re, 1, &subgrp.));
	%let subgrp2=%sysfunc(prxposn(&re, 2, &subgrp.));
	%put &subgrp1 &subgrp2;

%end;
/*%put &re.;*/
/*data check_crosstb_&group._freq&freq._merge_1;*/
/*set check_crosstb_&group._freq&freq._merge;*/
/*if new_subgrp in (subgrp1 subgrp2) then new_subgrp=subgrp1;*/
/*run;*/
%end;
%mend;


%macro merge_small_subgrp_step2();
proc import out=subgrp_after_manu_revise
datafile="&outpath./check_crossTb_sumby_newsubgrp(with merging for small subgrp to neighbour).csv"
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
if _n_ = &i. then do;
	call symput('group', left(trim(group)));
/*	call symput('freq', left(trim(freq)));*/
	call symput('cluster', left(trim(cluster)));

	new_subgrp=left(trim(new_subgrp));
	re=prxparse("/(\d+)\w+(\d+)/i");
	if prxmatch(re, new_subgrp) then do;
		subgrp1=input(prxposn(re, 1, new_subgrp), 3.);
		subgrp2=input(prxposn(re, 2, new_subgrp), 3.);
		call symput('subgrp1', subgrp1);
		call symput('subgrp2', subgrp2);
	end;

end;
run;
%let group=%sysfunc(trim(%sysfunc(left(&group.))));
/*%let freq=%sysfunc(trim(%sysfunc(left(&freq.))));*/
%let cluster=%sysfunc(trim(%sysfunc(left(&cluster.))));

%put &group &cluster &subgrp1 &subgrp2;

data check_crosstb_&group._cls&cluster._merge;
set check_crosstb_&group._cls&cluster._merge;
if new_subgrp in (&subgrp1. &subgrp2.) then new_subgrp=&subgrp1.;
run;



%end;


%mend;

%merge_small_subgrp_step2();


%macro extract_obs_foreachsubgrp();
data vmember_flag;
set sashelp.vmember;
re = PRXPARSE("/^check_crosstb_(\d+)_cls(\d+)_merge$/i"); /*CHECK_CROSSTB_42_FREQ308_MERGE*/
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
	data out.Ck_crosstb_summary_merge_revise;
	set &dataset_name(obs=0);
	run;
	%end;
	proc datasets library=work nolist;
	   append base=out.Ck_crosstb_summary_merge_revise data=&dataset_name. force;
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
if _n_=&j then do;
call symput('subgrp', new_subgrp);
end;
run;
%let subgrp=%sysfunc(left(%sysfunc(trim(&subgrp))));
%put &subgrp.;
proc sql;
create table dt_grp&&group._cls&&cluster._subgrp&subgrp. as
select _name_ as obs_id, count_sum_with_month_Besuch
from dt2check_grp&&group._cls&cluster.
where count_sum_with_month_Besuch in
(select count_sum_with_month_Besuch from &dataset_name. where new_subgrp in (&subgrp.))
;
quit;
%end;
%end;

data out.ck_crosstb_summary_merge_revise;
set out.ck_crosstb_summary_merge_revise;
group_cls_subgrp=catx('_', group, cluster, new_subgrp);
run;

data out.ck_crosstb_summary_merge_revise;
retain new_subgrp_re 0;
set out.ck_crosstb_summary_merge_revise;
by group_cls_subgrp;
if first.group_cls_subgrp then new_subgrp_re=new_subgrp_re+1;
run;

proc sql;
create table test_conflict as
select *, sum(count) as sum_in_subgrp from out.ck_crosstb_summary_merge_revise
group by new_subgrp_re
having percent < 100 and sum(count)>200;
quit;

/*proc sql;*/
/*create table test_conflict1 as*/
/*select a.* from out.ck_crosstb_summary_merge_revise a inner join test_conflict b*/
/*on a.new_subgrp=b.new_subgrp;*/
/*quit;*/
	proc export data=out.Ck_crosstb_summary_merge_revise
	outfile="&outpath.\Check_crosstb_summary_merge_revise.csv"
	dbms=csv replace;
	run;

%mend;
%extract_obs_foreachsubgrp();







%macro statTb4revised(input, out);
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
  var count_sum_with_month_Besuch;
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
				  , out=statTb4merge_revise
)

/*check the two qc table to find the conflict new_subgrp_re (group- 18  cluster-1  subgrp_re-40)*/
proc sql;
select * from out.Ck_crosstb_summary_merge_revise
where group=18 and cluster=1;
quit;
data out.Ck_crosstb_summary_merge_revise1 test;
retain new_subgrp_re group cluster freq;
set out.Ck_crosstb_summary_merge_revise;
if group=18 and cluster=1 and new_subgrp_re=40 and count_sum_with_month_Besuch=1 then do;
/*new_subgrp=new_subgrp+1 and new_subgrp_re=new_subgrp_re+1;*/
group_cls_subgrp=.;
new_subgrp=new_subgrp+1;
new_subgrp_re=new_subgrp_re+1;
/*output;*/
end;
drop group_cls_subgrp;
run;
proc sql;
select * from out.Ck_crosstb_summary_merge_revise1
where group=18 and cluster=1;
quit;

/*now manually change the new_subgrp_re and retrun the macro statTb4revise again*/
%statTb4revised(input=out.Ck_crosstb_summary_merge_revise1
				  , out=statTb4merge_revise1
)


%macro merge_all_part(promo_data, input1, input2, input3, output1, output, out4freq);
data ttt;
set &input2.;
/*if _n_ in (108 109) then delete;*/
/*if _n_ = 107 then freq=freq+2;*/
run;
proc sql;
create table temp1 as
select b.specialty1_merge, b.county_text, b.group, b.cluster,  b.freq, a.new_subgrp, a.new_subgrp_re, a.count_in_subgrp
,b.mean as mean_in_cluster, b.std as std_in_cluster, b.min as min_in_cluster, b.max as max_in_cluster
,a.mean as mean_in_subgrp, a.std as std_in_subgrp, a.min as min_in_subgrp, a.max as max_in_subgrp
 
from &input2. b left join &input1. a
on a.group=b.group and a.cluster=b.cluster and a.freq=b.freq;
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
	call symput("county", county_text);
end;
run;
%let spec=%sysfunc(left(%sysfunc(trim(&spec))));
%let county=%sysfunc(left(%sysfunc(trim(&county))));
%put &spec. &county.;

proc means data=&promo_data.(where=(specialty1_merge="&spec." and  county_text="&county.")) nway;
  var count_sum_with_month_Besuch;
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
select a.specialty1_merge, a.county_text, b.group, a.freq, b.mean, b.std, b.min, b.max
from &input3. a left join &output1. b
on a.grp_flag = b.group;
quit;

proc sql;
create table &output. as
select a.specialty1_merge, a.county_text, a.group, b.cluster, b.new_subgrp, b.new_subgrp_re, a.freq as count_in_group, b.freq as count_in_cluster, b.count_in_subgrp
, a.mean as mean_in_group, a.std as std_in_group, a.min as min_in_group, a.max as max_in_group
, b.mean_in_cluster, b.std_in_cluster, b.min_in_cluster, b.max_in_cluster
, b.mean_in_subgrp, b.std_in_subgrp, b.min_in_subgrp, b.max_in_subgrp
from &output1. a full join temp1 b
on a.group=b.group;
quit;

data &out4freq.;
retain specialty1_merge county_text segment count mean std min max;
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
keep specialty1_merge county_text segment count mean std min max group cluster new_subgrp new_subgrp_re;
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

%merge_all_part(promo_data=dir_ge.for_cluster
				, input1=out.stattb4merge_revise1
				, input2=out.stat_table_for_cluster_result
				, input3=out.grp_ref_tb
				, output1=out.stat_table_for_grp
				, output=out.stat_table
				, out4freq=out.final_freq_in_seg);

/*qc*/
proc sql;
select max(count), min(count) from out.final_freq_in_seg
union
select max(count), min(count) from out.final_freq_in_seg where std>0
union
select max(count), min(count) from out.final_freq_in_seg where cluster>0 and std>0
union
select max(count), min(count) from out.final_freq_in_seg where new_subgrp>0 and std>0
union
select sum(count) from out.final_freq_in_seg;
quit;
/*184 71 new_subgrp>0 and std>0
199 18 std>0
199 71 cluster>0 and std>0
510 18 
39431 .
*/

%macro my_univariate(input,var);
ODS HTML path="&outpath."
BODY = "distribution of segmentation size.htm"; 
/*ods graphics on;*/
proc univariate data = &input.;
HISTOGRAM &var.   /NORMAL(color=red) CFILL = ltgray CTEXT = blue ;
/*class &by_var.;*/
INSET N = 'Number of Physicians' MEDIAN (8.2) MEAN (8.2) STD='Standard Deviation' (8.3)/ POSITION = ne;
run;

/*PROC SGPLOT DATA = &input.;*/
/* HISTOGRAM &var.;*/
/* TITLE "Olympic Men's Swimming Freestyle 100";*/
/*RUN; */

ODS HTML close;
/*ods graphics off;*/
%mend;
%my_univariate(out.final_freq_in_seg, var=count);

proc chart data=out.final_freq_in_seg;
vbar segment;
run;

%macro check_cluster_from_tree();
data vmember_flag;
set sashelp.vmember;
re = PRXPARSE("/^out(\d+)$/i"); /*CHECK_CROSSTB_42_FREQ308_MERGE*/
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
select &group. as group, cluster, max(count_sum_with_month_Besuch) as max
, min(count_sum_with_month_Besuch) as min from &dataset_name. 
group by cluster
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

%check_cluster_from_tree();


%macro extract_obs_for_seg(input1, input2, input3, output);/*not complete debug add a column of original new_subgrp*/
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

end;
	
run;
%let spec=%sysfunc(left(%sysfunc(trim(&spec))));
%let county=%sysfunc(left(%sysfunc(trim(&county))));
%let seg=%sysfunc(left(%sysfunc(trim(&seg))));
%let group=%sysfunc(left(%sysfunc(trim(&group))));
%let cluster=%sysfunc(left(%sysfunc(trim(&cluster))));
%let subgrp=%sysfunc(left(%sysfunc(trim(&subgrp))));

%put &spec. &county. &seg. &group. &cluster. &subgrp.;

%if &cluster=. %then %do;
	data temp1;
	length obs $ 200;
	set &input2.;
	obs=catx("","OB",_n_);
	segment=&seg.;
	where specialty1_merge="&spec" and county_text="&county.";
	keep obs segment;
	run;
%end;
%if &subgrp=. and &cluster^=. %then %do;
	data temp1;
	length obs $ 200;
	set out&group.(rename=(_name_=obs));
	segment=&seg.;
/*	rename _name_=obs;*/
	where cluster=&cluster.;
	keep obs segment;
	run;
%end;
%if &subgrp^=. %then %do;
	data temp1;
	length obs $ 200;
	set dt_grp&group._cls&cluster._subgrp&subgrp.(rename=(obs_id=obs));
/*	rename obs_id=obs;*/
	segment=&seg.;
	keep obs segment;
	run;
%end;
%if &i=1 %then %do;
	data &output.;
	length obs $ 200;
	set temp1(obs=0);
	run;
%end;
proc datasets library=work nolist;
	append base=&output. data=temp1 force;
run;
%end;
data &output.;
set &output.;
re=prxparse("/^\wb(\d+)$/i");
flag=prxmatch(re, left(trim(obs)));
if flag then do;
obs_num=input(prxposn(re, 1, left(trim(obs))), 5.);
end;
keep obs_num segment;
run;

%mend;

%extract_obs_for_seg(input1=out.final_freq_in_seg
, input2=dir_ge.for_cluster
, input3=out.Ck_crosstb_summary_merge_revise1
, output=out.obs_seg);

/*qc*/
proc sql;
create table a as
select segment, obs_num from out.obs_seg
where obs_num=.;
quit;

/*further split the large segments into small ones using other promo variables*/
%let size_cutoff=307;

%macro further_split_largesize(size_cutoff, input, output);
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
	re=prxparse("/^\wb(\d+)$/i");
	flag=prxmatch(re, left(trim(obs_id)));
	if flag then do;
	obs_num=input(prxposn(re, 1, left(trim(obs_id))), 5.);
	end;

	run;

	proc sql;
	create table dt4seg&segment. as
	select &segment. as segment, * from &input1.
	where  monotonic() in (select obs_num from temp1);
	quit;
	
%end;

%mend;
%further_split_largesize(
size_cutoff=&size_cutoff
, input=out.final_freq_in_seg
, input1=dir_ge.for_cluster
, output=
)







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

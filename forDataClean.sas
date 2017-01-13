libname dir_ge "C:\work\working materials\MCM";
%let dataPath = C:\work\working materials\MCM;

libname dir_ge "C:\work\working materials\MCM";
%let dataPath = C:\work\working materials\MCM;

proc import out=dir_ge.Gm_promo_data
datafile = "&dataPath/MSD_Promo_OneKey_GE .xlsx"
dbms=excel replace;
GETNAMES=YES;
RANGE='selected$A1:EZ10002';
/*datarow=10001;*/
run;

proc import out=dir_ge.onekey_data
datafile = "&dataPath/OneKey_GE_for MSD add one obs.xlsx"
dbms=excel replace;
GETNAMES=YES;
RANGE='Tabelle1$A1:FS63342';
/*datarow=10001;*/
run;

data dir_ge.Gm_promo_data;
set dir_ge.Gm_promo_data;
if _n_ > 1;
run;

data dir_ge.onekey_data;
set dir_ge.onekey_data;
if _n_ > 1;
run;

%include "C:\work\working materials\MCM\From_Yan_Yahua\summarizer_post_enchanced.sas";
%include "C:\work\working materials\MCM\From_Yan_Yahua\write_missing_values.sas";

%summarizer_post_enhanced(in_data = dir_ge.onekey_data,
                                                 missing_value =%str("<<ohne titel>>"), 
                                                  out_path = &dataPath.,
                                                 out_table_name = onekey_data_summary);

%summarizer_post_enhanced(in_data = dir_ge.Gm_promo_data,
                                                 missing_value =%str(), 
                                                  out_path = &dataPath.,
                                                 out_table_name = promo_data_summary);


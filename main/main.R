

rm(list=ls(all=TRUE))

library(plyr)
library(dplyr)
library(reshape2)
library(lattice)
library(coda)
library("arm")
library("R2WinBUGS")
library(sas7bdat)
library(xlsx)
require(lme4)
require(car)
library(snowfall)

data_path <- "../Data/for_test/"
data_path_1 <- data_path
data_path_2 <- "../Data/FR/"
path_fun <- "./func/"
source(paste0(path_fun, "PromoMix Functions v3.txt"))
source(paste0(path_fun, "function.R"))


timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
# setwd(resultDir)


prod <- "prolia"
min_date <- "2015/01/01"
max_date <- "2015/12/01"
IDs_var <- 'final_segment'
T1_var <- 'date'                       # Number of total Time Periods
random = ""

salesVars2adj <- c('prescriptions', 'units_sales', 'eur_sales')
salesVar4revenue <- 'eur_sales'
promo_var <- c("call", "meeting_epu", "meeting_national", "meeting_international", "meeting_other")
price_vct <- c(238, 238, 238, 238, 238) #nrx price
unitCosts_vct <- c(121.4, 1,1,1,1)
promo_var_size_adj <- rep(c("cohort_count_fromseg"), length(promo_var))

# set up nrx variable and size adjustment
nrx_var <- c('prescriptions')
nrx_var_size_adj <-c('cohort_count_fromsales')

bStd <- T

firstmon <- 3
Retain <- c(0.8, 0.8, 0.8, 0.8, 0.8) # Change

otherVars_inModel <- c('prescriptions_adj', 'final_segment', 'date')

n.cpu <- 4



df <- model_data_prepare1(inPath=data_path_2
                          , inFile="for_model_data_0329.xlsx"
                          , T1_var='date'
)

control_df <- data.frame(feb=ifelse(df$month %in% c(2), 1, 0)
                         , aug=ifelse(df$month %in% c(8), 1, 0)
                         , log_trend=log(1+df$month)
                         , trend=df$month
                         , pos=ifelse(df$month==6, 1, ifelse(df$month==9, 3, 0))
                         , neg=ifelse(df$month==2, 1.5, ifelse(df$month==8, 2, ifelse(df$month %in% c(5, 11), 1, 0)))
                         , pos_77=ifelse(df$final_segment==77 & df$month %in% c(6, 12), 1, 0)
)

model_data_list <- model_data_prepare2(df=df
                                       , control_df=control_df
                                       , bStd=bStd
                                       , nrx_var=nrx_var
                                       , rt_test=c(0.5, 0.5, 0.5, 0.5, 0.5)
                                       , salesVar4revenue=salesVar4revenue
)



baseLine_output_list <- run_baseLine(model_data=model_data_list$mod_data4BaseLine
                                     , nrx_var = nrx_var
                                     , promo_var_inBl=c('call')
                                     , ctrl_var_inBl=c('log_trend', 'pos', 'neg', 'pos_77')
                                     , promo_var_inBl_fixed=c('call')
                                     , ctrl_var_inBl_fixed=c("pos_77")

)

ctrl_var_inBys <- c('log_trend', 'pos', 'neg', 'pos_77')

result_retentionLoop <- run_retention_loop(inPath=data_path_2
                                           , path_fun=path_fun
                                           , file="Retention Rates_5vars"
                                           , model_data_list=model_data_list
                                           , n.cpu=n.cpu
                                           , promo_var=promo_var
                                           , ctrl_var=ctrl_var_inBys
                                           , iters=30, p=rep(0.5, 5)
                                           , d1=1, d2=c(0, 0, 0, 0, 0, 1, 1, 1,0)
                                           , nrx_var=nrx_var
                                           , mu1=c(0.040105695,	0.005538787,	0.012136681,	0.023471172,	0.001431081,0,0)
                                           , prec1=c(1061.49395,	125222.6303,	26080.28162,	6973.368051,	1875787.238, 0.9604, 0.9604)
                                           , M1=c(500, 500, 500, 500, 500)
                                           , T1_var='date'
                                           , IDs_var='final_segment'
                                           , bStd=bStd
                                           , bTest=T
                                           , resultDir=resultDir
                                           , traceFile='traceFile_runRet'
                                           , bPar=F
                                           , outFile='retentionLoop_output'
                                           , b4RtLoop=T
)


# setwd("C:\\work\\working materials\\MCM\\R part\\Code\\")
run_bayes(X4Bayes=model_data_list$X4Bayes, model_data4BaseLine=model_data_list$mod_data4BaseLine
          , prod=prod, IDs_var=IDs_var, ctrl_var=ctrl_var_inBys, promo_var=promo_var
          , nrx_var = nrx_var
          , iters=30, p=rep(0.5, 5), d1=1, d2=c(0, 0, 0, 0, 0, 1, 1, 1,0)
          , mu1=c(0.040105695,	0.005538787,	0.012136681,	0.023471172,	0.001431081, rep(0, length(ctrl_var_inBys)))
          , prec1=c(1061.49395,	125222.6303,	26080.28162,	6973.368051,	1875787.238, rep(0.9604, length(ctrl_var_inBys)))
          , M1=c(500, 500, 500, 500, 500)
          , bStd=bStd
          , resultDir=resultDir
          , traceFile="traceFile_bayes"
          , b4RtLoop=F
          )




roi_result <- run_roi(inPath=resultDir, outPath=resultDir, prod=prod, dt_name="_Means.csv" 
                      , promo_var=promo_var
                      , price_vct=price_vct
                      , unitCosts_vct=unitCosts_vct
                      , ctrl_var=ctrl_var_inBys
                      , rt_test = c(0.5085009, 0.4999830, 0.4991948, 0.4938537, 0.5000273)
                      , model_data_list=model_data_list
                      , otherVars_inModel=otherVars_inModel
)

     
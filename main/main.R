

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
year2Rm <- "2015"
IDs_var <- 'final_segment'
T1_var <- 'date'                       # Number of total Time Periods
random = ""

salesVars2adj <- c('prescriptions', 'units_sales', 'eur_sales')

promo_var <- c("call", "meeting_epu", "meeting_national", "meeting_international", "meeting_other")
# promo_var_size_adj <- c("hcp_innano", "hcp_innano", "hcp_innano")
promo_var_size_adj <- rep(c("cohort_count_fromseg"), length(promo_var))

# set up nrx variable and size adjustment
nrx_var <- c('prescriptions')
nrx_var_size_adj <-c('cohort_count_fromsales')

firstmon <- 3
Retain <- c(0.8, 0.3, 0.3, 0.3, 0.3)  # Change
adj_var <- paste0(promo_var, "_adj")
stk_var <- paste0(adj_var, "_stk")
rt_var <- paste0(stk_var, '_rt')

ctrl_var_inModel <- c('event1', 'event2')
otherVars_inModel <- c('prescriptions', 'prescriptions_adj', 'final_segment', 'date')

rt_test <- rep(0.5, length(stk_var))


# year2Rm, IDs_var, T1_var, salesVars2adj, promo_var, promo_var_size_adj, nrx_var, nrx_var_size_adj
# , Retain, ctrl_var_inModel, otherVars_inModel, rt_test
model_data_list <- model_data_prepare()


baseLine_output_list <- run_baseLine(model_data=model_data_list$mod_data4BaseLine
             , var_inModel = paste0(promo_var, '_adj_stk_rt')
             , nrx_var = 'prescriptions_adj'
             )




setwd("C:\\work\\working materials\\MCM\\R part\\Code\\")
run_bayes(X4Bayes=model_data_list$X4Bayes, model_data4BaseLine=model_data_list$mod_data4BaseLine
          , prod=prod, IDs_var=IDs_var, ctrl_var=c('event1', 'event2'), var_inModel=paste0(promo_var, '_adj_stk')
          , iters=30, p=rep(0.5, 5), d1=1, d2=c(rep(0, 6), 1), nrx_var=paste0(nrx_var, '_adj')
          , mu1=c(0.080211391, 0.005538787, 0.012136681, 0.015647448, 0.001431081,0,0)
          , prec1=c(265.3734876, 55654.50234, 11591.23628, 6973.368051, 833683.217, 0.9604, 0.9604)
          , M1=c(50, 50, 50, 50, 50)
          )




roi_result <- run_roi(inPath=resultDir, outPath=resultDir, prod=prod, dt_name="_Means.csv", vars4rt=stk_var
                      , price_df=data.frame(238, 238, 238, 238, 238) %>% setNames(paste0('price_', promo_var, '_adj_stk'))
                      , unitCosts_df=data.frame(121.4, 1,1,1,1) %>% setNames(paste0('uniCost_', promo_var, '_adj_stk'))
                      , ctrl_var=ctrl_var_inModel
                      , rt_test=c(0.3527636, 0.4703400, 0.4925533, 0.4612156, 0.4914622 )
                      , model_data_list=model_data_list
                      , otherVars_inModel=otherVars_inModel
)

     
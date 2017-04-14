
##########################################################
###Author: MArio Müller
###Projekt: AMGEN MCM Sales Modeling on Nanobrick-Level
##########################################################
# setwd("Z:\\Departments\\STO\\PROJECTS\\AdvancedAnalytics\\Functions\\")
# getwd()

##empty workspace
rm(list=ls(all=TRUE))

############
## 1) DATA MANIPULATION TO GET FINAL DF
############



##Libraries
library(plyr)
library(dplyr)
library(reshape2)
library(lattice)
library(coda)
library("arm")
library("R2WinBUGS")
library(sas7bdat)
library(xlsx)

##Directories and functions

# R codes directory
# wk_dir = "N:\\MCM\\Amgen\\Data\\MCM_Amgen_DE\\"
# Data read and save path
# data_path = "N:\\MCM\\Amgen\\Data\\MCM_Amgen_DE\\"
# data_path_1 = "N:\\MCM\\Amgen\\Data\\MCM_Amgen_DE\\Xponent\\sales_einzeln\\"
# path_fun <- "Z:\\Departments\\STO\\PROJECTS\\AdvancedAnalytics\\Functions\\"
data_path <- "../Data/for_test/"
data_path_1 <- data_path
data_path_2 <- "../Data/FR/"
path_fun <- "./"
source(paste0(path_fun, "PromoMix Functions v3.txt"))

timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
# setwd(resultDir)
setwd("C:\\work\\working materials\\MCM\\R part\\Code\\")


#################################################
# 2. DATA MANIPULATION WITH FINAL DF
#################################################


#################################################
# read in model data from yahua
#################################################
df <- read.xlsx(file=paste0(data_path_2, 'for_model_data_0329.xlsx')
                , sheetIndex=1
                , header = T
) %>% #[1] 3182   13 
{
      dtLastStep <- .
      names(dtLastStep) <- tolower(names(dtLastStep))
      dtLastStep
} 
#[1] 3182   13
      # filter(!is.na(eur_sales) & !is.na(actual_promo_id_count)) %>%#[1] 1401   13

records2rm <- which(gsub("^(\\d{4})(.+$)", "\\1", df$date, perl=T)!='2015')
# records2rm <- which(is.na(df$eur_sales) | is.na(df$actual_promo_id_count)) #[1] 2507
df_final <- df
#################################################
# Set Some Constants
#################################################



prod <- "prolia"
IDs <- length (unique(df_final$final_segment))                   # Number of Nanobricks
T1 <- 37                       # Number of total Time Periods
random = ""

##########
## adjust nrx and sales using "cohort_count_fromsales" "cohort_count_fromseg"
##########
salesVars2adj <- c('prescriptions', 'units_sales', 'eur_sales')
temp1 <- lapply(salesVars2adj, function(v){
      vct <- df[, v]
      vct_adj <- vct/df[, 'cohort_count_fromsales']*df[, 'cohort_count_fromseg']
      return(vct_adj)
}) %>%
      # data.frame(do.call(cbind.data.frame, .))
      do.call(cbind, .) %>%
      as.data.frame() %>%
      tbl_df() %>%
      {
            dtLastStep <- .
            temp2 <- df %>% dplyr::select(-one_of(salesVars2adj))
            temp3 <- cbind(temp2, dtLastStep)
            names(temp3) <- c(setdiff(names(df), salesVars2adj), salesVars2adj)
            temp3
      }
df_final <- temp1 %>% 
      mutate(month=gsub('\\d{4}-(\\d{2})-\\d+', "\\1", date, perl=T)) %>%
      bind_cols(data.frame(event1=ifelse(.$month %in% c('02'), 1, 0)
                           , event2=ifelse(.$month %in% c('08'), 1, 0)))      

##########
## Normalize data to brick size
##########

# set up promo var and size adjustment
promo_var <- c("call", "meeting_epu", "meeting_national", "meeting_international", "meeting_other")
# promo_var_size_adj <- c("hcp_innano", "hcp_innano", "hcp_innano")
promo_var_size_adj <- rep(c("cohort_count_fromseg"), length(promo_var))

# set up nrx variable and size adjustment
nrx_var <- c('prescriptions')
nrx_var_size_adj <-c('cohort_count_fromsales')
# size adjust promo variables
for( i in 1:length(c(promo_var, nrx_var))) {
  v <- paste0(c(promo_var, nrx_var)[i], "_adj")
  df_final[, v] <- df_final[, c(promo_var, nrx_var)[i] ] / df_final[, c(promo_var_size_adj, nrx_var_size_adj)[i]]
}

#########
## Build adstocks
########

# promo_var1 <- c("Prolia_calls_norm", "Prolia_dmail_norm", "Prolia_email_norm")
firstmon <- 3
Retain <- c(0.8, 0.3, 0.3, 0.3, 0.3)  # Change
stk_var <- paste0(promo_var, "_adj")

## replace NA as 0 for stock variables before stocking
X1 <- df_final[, c( stk_var)] %>%
{
      dtLastStep <- .
      temp <- lapply(dtLastStep, function(x)ifelse(is.na(x), 0, x))
      temp
} %>%
      
      # lapply(stk_var, function(v)ifelse(is.na(.[, v]), 0, x)) %>%
      do.call(cbind.data.frame, .) %>%
      bind_cols(dplyr::select(df_final, one_of(c("final_segment","date"))))


X2 <- X1  # Intialize stocked variables X2
atts1 <- length(stk_var)
Start <- 0*c(1:atts1) # Intialize start value before first month



## loop through IDs to initalize stocks and carry out stocking
for(i in 1:IDs){
  row_idx <- (1+T1*(i-1)):(T1+T1*(i-1))  # row index for current IDs
  Start <- colMeans( X1[row_idx[1:firstmon], stk_var] )
  X2[row_idx, stk_var] <- Stock(X1[row_idx, stk_var], Start, Retain)## Stock and standardize Variables (Note set retention rates)

}


# X3 <- X2[, c( 5:7)]
# X3[is.na(X3)] <- 0
X3 <- X2[, stk_var]
X3[is.na(X3)] <- 0

##########
## Final dfs as input for model
##########

# ctrl <- df_final [ , c("lfnd_date", "bevorratung", "summerbreak_1", "summerbreak_2", "qu1", "qu2", "qu3","proliavo_wettb_norm","google_prolia",  "google_prolia_movavg_1",  "google_prolia_movavg_2", "proliavo_avg6", "proliavo_avg2", "proliavo_avg4", "proliavo_avg6",	"proliavo_avg8",	"proliavo_movavg1",	"proliavo_movavg2",	"proliavo_movavg3" )]
# ctrl_var <- c("lfnd_date", "proliavo_wettb_norm","summerbreak_1","proliavo_avg6, summerbreak_1", "summerbreak_2", "qu1", "qu2" )
ctrl <- df_final[, c('event1', 'event2')]
ctrl_var <- c('event1', 'event2')
X <- as.matrix (X3)
# y <- df_final [ , c("proliavo", "proliavo_norm", "brick", "datum")]
y <- df_final[, c('prescriptions', 'prescriptions_adj', 'final_segment', 'date')] %>%
      lapply(., function(x)ifelse(is.na(x), 0, x))
# ctrl$proliavo_wettb_norm_scale <- scale( ctrl$proliavo_wettb_norm, center = F, scale = max(ctrl$proliavo_wettb_norm) )


############
## 2) MODELING
############

######################
## 2.1) Simple mixed model to gain priors
#####################

require(lme4)

# raise to root
rt_test <- rep(0.5, 5)
X3_rt <- X3
X3_rt_1 <- X3_rt #added by Jie to initialize X3_rt_1
for(i in 1:length(rt_test)) {
  X3_rt_1[, i] <- X3_rt_1[, i]^rt_test[i]
}

mod_data <- data.frame(y, X3_rt_1,ctrl ) %>%
      .[-records2rm, ]
# [1] 1032 10


#Mixed Model
# lmm <- lmer(proliavo_norm ~  proliavo_wettb_norm  + lfnd_date+ summerbreak_1 + Prolia_calls_adj+Prolia_email_adj+Prolia_dmail_adj +( 1+lfnd_date+proliavo_wettb_norm+summerbreak_1|brick), data = mod_data)
# stk_var_inModel <- c("call_adj", "meeting_national_adj")
stk_var_inModel <- promo_var
lmm <- lmer(prescriptions_adj ~ call_adj + meeting_national_adj + meeting_epu_adj + meeting_international_adj + meeting_other_adj + (1 + ctrl | final_segment), data = mod_data)
summary(lmm)$coefficients
require(car)
Anova(lmm)
coef(lmm)

#Coefficients
coef <- data.frame (coef(lmm)[[1]])
coef$final_segment<-rownames(coef)
mod_data1 <- mod_data
mod_data1$final_segment <- as.character(mod_data1$final_segment)
# mod_data1$proliavo_wettb_norm_scale <- as.numeric (mod_data1$proliavo_wettb_norm_scale)
model_df <- left_join (mod_data1, coef, by= c("final_segment"="final_segment"))
# model_df$sum_fitted <- model_df$X.Intercept. + model_df$lfnd_date.x * model_df$lfnd_date.y+model_df$summerbreak_1.x *model_df$summerbreak_1.y + model_df$proliavo_wettb_norm.x*+ model_df$proliavo_wettb_norm.y+ model_df$Prolia_calls_adj.x*model_df$Prolia_calls_adj.y+ model_df$Prolia_email_adj.x*model_df$Prolia_email_adj.y+model_df$Prolia_dmail_adj.x*model_df$Prolia_dmail_adj.y
# model_df$sum_fitted <- model_df$X.Intercept. + model_df$call_adj.x + model_df$call_adj.y 
#        + model_df$meeting_national_adj.x + model_df$meeting_national_adj.y
#        + model_df$ctrl.x + model_df$ctrl.y
model_df$sum_fitted <- predict(lmm)

# find R-squared & MAPE
model_df$mape <- abs ((model_df$prescriptions_adj-model_df$sum_fitted)/model_df$prescriptions_adj)
mean (model_df$mape[model_df$mape != Inf])
1 - sum( (model_df$sum_fitted - model_df$prescriptions_adj)^2 ) / sum( (model_df$prescriptions_adj-mean(model_df$prescriptions_adj))^2 )




######################
## 2.2) MCMC approach
#####################

# Set Some Constants
path_fun <- "./"
source(paste0(path_fun, "PromoMix Functions v3.txt"))
ctrl_var_1 <- c("event1", 'event2')
ctrl <- df_final [ , c("event1", 'event2')]
X <-as.matrix(cbind(X3, ctrl)) %>% .[-records2rm, ]
stk_var_inModel <- paste0(promo_var, '_adj')
atts1 <- length(stk_var_inModel)                      # Number of Promotional Variables
atts2 <-  length (ctrl_var_1)                   # Number of Non Promotional Variables (not intercept)
dimbeta <- atts1+atts2           # Total Number of Variables
T_mod <- 12                    # Number of Time Periods for modeling
iters<-30                    # Number of MCMC iterations
p=rep(0.5, 5) # Set prior for roots (Point estimate and number of prior observations)
d1<-1 # Intercept #  Set Indicators for random effects (1 --> Random effects,0-->no random effects, d1 is only for intercept, d2 is for promo and nopromo vars)
d2<-c(rep(0, 6), 1)   # Remaining Model parameters
y1 <-mod_data [ , c("prescriptions_adj")]

## Function for MCMC
myloop <- function(Rname ,Rmu,Rprec,RM, vars, path_fun, resultDir) {
  name = Rname              # Name of Output File
  mu=Rmu
  prec=Rprec
  M=RM
  
  X <- X[, vars]
  mean(X[,1:dimbeta]) # Check Standardization again
  data <- list(y=y1,X=X,d1=d1,d2=d2,atts1=atts1,atts2=atts2, mu=mu,prec=prec,p=p,M=M,dimbeta=dimbeta,T=T_mod,IDs=IDs)# Setup "data" For Winbugs
  parameters <- c("root","betam","a","alpha","beta","Xb","stdevint","stdevdata","stdevbeta")# Set Parameters to Monitor
  betatemp <-     0*c(1:dimbeta) #->0 # set initial values for MCMC (should have no influence)
  precm    <- 1 + 0*c(1:dimbeta) #->1
  precdata <- 1 + 0*c(1:IDs) #->1
  inits1=list(betam=betatemp,precm=precm,precdata=precdata)  #-> starting point for every chain
  inits2=list(betam=betatemp,precm=precm,precdata=precdata)
  inits3=list(betam=betatemp,precm=precm,precdata=precdata)
  inits=list(inits1,inits2,inits3)

  # Run PromoMix Model
  out<-PromoMix(data=data,inits=inits,parameters=parameters,n.chains=3,n.iter=iters,debug=T,atts1=atts1,atts2=atts2,T=T_mod,name=name, path_fun=path_fun, resultDir=resultDir)
  out
}


#Model 02
name1="Ipsen"
mu1=c(0.080211391, 0.005538787, 0.012136681, 0.015647448, 0.001431081,0,0)   # Change
prec1=c(265.3734876, 55654.50234, 11591.23628, 6973.368051, 833683.217, 0.9604, 0.9604) # Change
M1=c(50, 50, 50, 50, 50, 50, 50)  # Change

a1R<-myloop(Rname=name1 ,Rmu=mu1,Rprec=prec1,RM=M1, vars=c(stk_var_inModel, ctrl_var_1), path_fun=path_fun, resultDir=resultDir)
cbind(colnames(X), a1R$Mbeta)
a1R$Roots
# root[1]   root[2]   root[3]   root[4]   root[5] 
# 0.3527636 0.4703400 0.4925533 0.4612156 0.4914622 


means <- read.csv(paste0(resultDir, name1, '_Means.csv'), stringsAsFactors = F)
betam <- means[grep("^betam.+$", means$X, perl=T), 'Mean']
beta <- means[grep("^beta\\W", means$X, perl = T), 'Mean']

coefs <- lapply(1:length(betam), function(i)beta[((i-1)*IDs+1):(i*IDs)]) %>%
      do.call(cbind, .) %>%
      as.data.frame()
names(coefs) <- paste0('beta_',c(stk_var_inModel, ctrl_var_1))
coefs$seg <- 1:IDs

# ##Transform decayed promotions with roots from mcmcm
rt_test <- c(0.3527636, 0.4703400, 0.4925533, 0.4612156, 0.4914622 )
X3_rt_3 <- X3[, stk_var_inModel]
for(i in 1:length(rt_test)) {
  X3_rt_3[, i] <- X3_rt_3[, i]^rt_test[i]
}
mod_data3 <- data.frame(y, X3_rt_3, ctrl)

price_df <- data.frame(rep(238, nrow(mod_data3))
                       , rep(238, nrow(mod_data3))
                       , rep(238, nrow(mod_data3))
                       , rep(238, nrow(mod_data3))
                       , rep(238, nrow(mod_data3))
                       ) %>%
      setNames(paste0('price_', stk_var_inModel))
unitCosts_df <- data.frame(rep(121.4, nrow(mod_data3))
                           , rep(121.4, nrow(mod_data3))
                           , rep(121.4, nrow(mod_data3))
                           , rep(121.4, nrow(mod_data3))
                           , rep(121.4, nrow(mod_data3))
) %>%
      setNames(paste0('uniCost_', stk_var_inModel))

mod_data_beta <- left_join(mod_data3, coefs, by=c('final_segment'='seg')) %>%
      cbind(., price_df, unitCosts_df)
mod_data_beta_cont <- 
      lapply(c(stk_var_inModel, ctrl_var_1), function(v)mod_data_beta[, v]*mod_data_beta[, paste0('beta_', v)]) %>%
      do.call(cbind, .) %>% 
      as.data.frame()
names(mod_data_beta_cont) <- paste0('cont_', c(stk_var_inModel, ctrl_var_1))

contributions <- colSums(mod_data_beta_cont)

mod_data_beta_ums <- lapply(c(stk_var_inModel), function(v)mod_data_beta[, v]*mod_data_beta[, paste0('beta_', v)]*mod_data_beta[, paste0('price_', v)]) %>%
      do.call(cbind, .) %>%
      as.data.frame()
names(mod_data_beta_ums) <- paste0('ums_', stk_var_inModel, '_norm')

mod_data_beta_costs <- lapply(c(stk_var_inModel), function(v)mod_data_beta[, v]*mod_data_beta[, paste0('uniCost_', v)]) %>%
      do.call(cbind, .) %>%
      as.data.frame()
names(mod_data_beta_costs) <- paste0('costs_', stk_var_inModel)

model_data_beta_agg <- cbind(mod_data_beta, mod_data_beta_ums, mod_data_beta_costs) %>%
      dplyr::select(one_of(c(grep('^(ums_|costs_)', names(.), value = T, perl = T), 'final_segment'))) %>%
      dplyr::group_by(final_segment) %>%
      # dplyr::summarise_each(funs(mean), one_of(grep('^(ums_|costs_)', names(.), value = T, perl = T)))
      dplyr::summarise_each(funs(mean)) %>%
      {
            dtLastStep <- .
            roi_df <- lapply(stk_var_inModel, function(v)dtLastStep[, paste0('ums_', v, '_norm')]/dtLastStep[, paste0('costs_', v)]) %>%
                  do.call(cbind, .) %>%
                  as.data.frame() %>%
                  setNames(paste0('roi_', stk_var_inModel)) %>%
                  bind_cols(dtLastStep)
            roi_df
      }

write.csv(model_data_beta_agg, paste0(resultDir, 'for_roi_qc.csv'), row.names = F)
##Calculate contributions
## Read in Mean-file of contributions
# coefs <- read.csv (paste0(data_path, "coef_input_clean.csv"), header=T, sep=";", quote = "", row.names = NULL, stringsAsFactors = FALSE)
# 
# ##Transform decayed promotions with roots from mcmcm
# rt_test <- c(0.824605777777778 ,0.79929, 0.817096222222222)
# X3_rt_3 <- tail (X3, n=23958)
# for(i in 1:length(rt_test)) {
#   X3_rt_3[, i] <- X3_rt_3[, i]^rt_test[i]
# }
# 
# ##transform due to german ,=.
# coefs$beta_call<- as.numeric(gsub(",", ".", gsub("\\.", "", coefs$beta_call)))
# coefs$beta_dmail<-as.numeric(gsub(",", ".", gsub("\\.", "", coefs$beta_dmail)))
# coefs$beta_email<-as.numeric(gsub(",", ".", gsub("\\.", "", coefs$beta_email)))
# coefs$beta_trend<-as.numeric(gsub(",", ".", gsub("\\.", "", coefs$beta_trend)))
# coefs$beta_comp<-as.numeric(gsub(",", ".", gsub("\\.", "", coefs$beta_comp)))
# coefs$beta_summerbreak<-as.numeric(gsub(",", ".", gsub("\\.", "", coefs$beta_summerbreak)))
# coefs$int<-as.numeric(gsub(",", ".", gsub("\\.", "", coefs$int)))
# 
# #Betas on brick level
# mod_data <- data.frame(y, X3_rt,ctrl )
# mod_data_beta <- left_join (mod_data [,-c(27)],coefs, by = c("brick"="brick") ) %>%
# 	               left_join (.,hcp_innano, by = c("brick"="Nanobrick") )%>%
# 	               left_join (.,df_final [,c(1:2, 6, 8:9)], by = c("brick"="brick", "datum"="datum") )

#Contributions
# mod_data_beta_cont <- mod_data_beta
# mod_data_beta_cont$cont_call <- mod_data_beta_cont$beta_call*mod_data_beta_cont$Prolia_calls_adj
# mod_data_beta_cont$cont_email <- mod_data_beta_cont$beta_email*mod_data_beta_cont$Prolia_email_adj
# mod_data_beta_cont$cont_dmail <- mod_data_beta_cont$beta_dmail*mod_data_beta_cont$Prolia_dmail_adj
# mod_data_beta_cont$cont_int <- mod_data_beta_cont$int
# mod_data_beta_cont$cont_comp <- mod_data_beta_cont$beta_comp*mod_data_beta_cont$proliavo_wettb_norm
# mod_data_beta_cont$cont_summerbreak <- mod_data_beta_cont$beta_summerbreak*mod_data_beta_cont$summerbreak_1
# mod_data_beta_cont$cont_trend <- mod_data_beta_cont$lfnd_date*mod_data_beta_cont$beta_trend
# contributions <- data.frame (colSums (mod_data_beta_cont [, c(38:44)], ))

#Revenue
# mod_data_beta$ums_call_norm <- mod_data_beta$beta_call*mod_data_beta$Prolia_calls_adj *238 ##-> price per package
# mod_data_beta$ums_dmail_norm <- mod_data_beta$beta_dmail*mod_data_beta$Prolia_dmail_adj*238
# mod_data_beta$ums_email_norm <- mod_data_beta$beta_email*mod_data_beta$Prolia_email_adj*238

#Costs
# mod_data_beta$cost_calls <- mod_data_beta$Prolia_calls_adj*86.21628584 ###-> Costs for 1 activity
# mod_data_beta$cost_dmail <- mod_data_beta$Prolia_dmail_adj*1.6
# mod_data_beta$cost_email <- mod_data_beta$Prolia_email_adj*1.6
# model_data_beta_agg <- data.frame (aggregate(mod_data_beta [ ,c(1, 2, 5:7,35:43)], by=list(mod_data_beta$brick), FUN=sum))
# model_data_beta_agg_1 <- left_join (model_data_beta_agg,hcp_innano, by = c("Group.1"="Nanobrick"))

#ROI
model_data_beta_agg_1$roi_calls <- model_data_beta_agg_1$ums_call_norm/model_data_beta_agg_1$cost_call
model_data_beta_agg_1$roi_dmail <- model_data_beta_agg_1$ums_dmail_norm/model_data_beta_agg_1$cost_dmail
model_data_beta_agg_1$roi_email <- model_data_beta_agg_1$ums_email_norm/model_data_beta_agg_1$cost_email


#write nanobrick ROIs to HCP
profmaster_sub <- hcp_profmaster_final [, c("Amgen_Customer_ID", "One_Key_ID", "Zip", "Address_Line_1", "City", "Primary_Specialty", "Nanobrick")]
names (profmaster_sub) <- c("Amgen_Customer_ID", "One_Key_ID", "Zip", "Address_Line_1", "City", "Primary_Specialty", "brick")

df_onekey_prolia_roi <- df_onekey [ ,c(38,2,11, 25, 34,45, 49)]
model_data_beta_hcp  <- data.frame (aggregate(df_onekey_prolia_roi [ ,c(3:5)], by=list(df_onekey_prolia_roi$One_Key_ID), FUN=sum))%>%
                        left_join (., profmaster_sub, by=c("Group.1"="One_Key_ID") )
                        left_join(., model_data_beta_agg_1, by=c("brick"="Group.1"))
model_data_beta_hcp$roi_calls_real <- ifelse(model_data_beta_hcp$Prolia_calls.x>0 & model_data_beta_hcp$roi_calls !="NaN",model_data_beta_hcp$roi_calls,NA)
model_data_beta_hcp$roi_dmail_real <- ifelse(model_data_beta_hcp$Prolia_dmail.x>0& model_data_beta_hcp$roi_dmail !="NaN",model_data_beta_hcp$roi_dmail,NA)
model_data_beta_hcp$roi_email_real <- ifelse(model_data_beta_hcp$Prolia_email.x>0& model_data_beta_hcp$roi_email !="NaN" ,model_data_beta_hcp$roi_email,NA)



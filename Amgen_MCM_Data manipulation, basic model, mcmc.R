
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
data_path <- "./Data/for_test/"
data_path_1 <- data_path
data_path_2 <- "./Data/FR/"
path_fun <- "./"
source(paste0(path_fun, "PromoMix Functions v3.txt"))

##Data Import
hcp_profmaster <- read.csv (paste0(data_path, "Amgen_DE_Professional_Master_20170114060100_withoutduplicates_top30.csv"), header=T, sep=",", quote = "", row.names = NULL, stringsAsFactors = FALSE)
hcp_brandta <- read.csv (paste0(data_path,"Amgen_DE_Brand_TA_20170116.psv"), header=T, sep="|", quote = "", row.names = NULL, stringsAsFactors = FALSE)
hcp_profile <- read.csv (paste0(data_path,"Amgen_DE_Professional_Profile_20170114060110_top30.psv"), header=T, sep=",", quote = "", row.names = NULL, stringsAsFactors = FALSE)
hcp_poa <- read.csv (paste0(data_path,"Amgen_DE_Professional_POA_20161230051237_top30.psv"), header=T, sep=",", quote = "", row.names = NULL, stringsAsFactors = FALSE)
hcp_nano<- read.csv (paste0(data_path,"reference_onekeyid_nanobrick_primaryflag.csv"), header=T, sep=";", quote = "", row.names = NULL, stringsAsFactors = FALSE)
date_mapping <- read.csv (paste0(data_path, "mapping_date_lfnd.csv"), header=T, sep=";", quote = "", row.names = NULL, stringsAsFactors = FALSE)
hcp_innano <- read.csv (paste0(data_path,"hcp_innano_arzt_primaryflag.csv"), header=T, sep=";", quote = "", row.names = NULL, stringsAsFactors = FALSE)
event_webtr_brandta <- read.csv (paste0(data_path, "Amgen_DE_WebTracking_BrandTA_Outbound_20161209070609_top30.psv"), header=T, sep=",", quote = "", row.names = NULL, stringsAsFactors = FALSE)
# event_webtr_all <- read.csv (paste0(data_path, "Amgen_DE_WebTracking_Outbound_20161214034610.csv"), header=T, sep=";", quote = "", row.names = NULL, stringsAsFactors = FALSE)
event_calls_input <- read.csv (paste0(data_path, "Amgen_DE_Professional_Call_20170115050151_top30.csv"), header=T, sep=",", quote = "", row.names = NULL, stringsAsFactors = FALSE)
event_call_details <- read.csv (paste0(data_path, "Amgen_DE_Professional_Call_Detail_20170115050147_top30.psv"), header=T, sep=",", quote = "", row.names = NULL, stringsAsFactors = FALSE)
event_email_subjectlines <- read.csv (paste0(data_path, "Amgen_DE_Emails_subjects.csv"), header=T, sep=";", quote = "", row.names = NULL, stringsAsFactors = FALSE)
event_email_dmail <- read.csv (paste0(data_path, "DE_Campaign Responses_20170116_top30.csv"), header=T, sep=",", quote = "", row.names = NULL, stringsAsFactors = FALSE)
google <- read.csv (paste0(data_path, "googletrnd.csv"), header=T, sep=";", quote = "", row.names = NULL, stringsAsFactors = FALSE)
quartal <- read.csv (paste0(data_path, "quartal.csv"), header=T, sep=";", quote = "", row.names = NULL, stringsAsFactors = FALSE)


###### 1.1)Transformation HCP data
############

#join brand information (brand_ta) to dfs
hcp_poa <- left_join(hcp_poa, hcp_brandta, by = c("X.Amgen_Brand_ID."="ï..Amgen_Brand_ID", "X.Amgen_TA_ID."="Amgen_TA_ID"))
hcp_profile <- left_join(hcp_profile, hcp_brandta, by = c("X.Amgen_Brand_ID."="ï..Amgen_Brand_ID", "X.Amgen_TA_ID."="Amgen_TA_ID"))


# hcp_profile wide by Amgen_Customer_ID
hcp_profile$Profile <- factor (hcp_profile$Profile)
hcp_profile$Profile_Type <- factor (hcp_profile$Profile_Type)
hcp_profile$Profile_Value <- factor (hcp_profile$Profile_Value)
hcp_profile_wide <- dcast(hcp_profile, Amgen_Customer_ID ~ Profile + brand_ta, value.var = "Profile_Value")
str (hcp_profile_wide)

#Join  hcp_profile_wide on hcp_profmaster
length (unique (hcp_profmaster$One_Key_ID))
hcp_profmaster_final <- left_join(hcp_profmaster, hcp_profile_wide, by = c("Amgen_Customer_ID"="Amgen_Customer_ID"))%>%
                        left_join(., hcp_nano, by = c("One_Key_ID"="Individual_ID_intl"))%>%
                        subset (., Customer_Type_Description == "Prescriber" & Nanobrick != "NA")

#Data Check
length (unique (hcp_profmaster_final$One_Key_ID))#99997

##-> Data loss due to filtering by prescriber and not existing nanobrick information




## 1.2) EVENT DATA AMGEN TOTAL (all products)

#email
event_email <- subset(event_email_dmail, channel == "Email" & event_type == "Viewed")%>%
               left_join(., event_email_subjectlines, by = c("wave_id"="EmailId"))%>%
               dcast(., External_Contact_Id + datum ~ campaign_brand, value.var = "campaign_brand", fun=length)%>%
               .[-which(.$External_Contact_Id == ""), ]
names(event_email) = c("Amgen_Customer_Id",   "datum", 	"NA_email", 	"Aranesp Nephrology_email", 	"Kyprolis_email", 	"Neulasta_email", "Nplate_email", 	"Prolia_email", 	"Repatha_email", 	"Vectibix_email", 	"XGEVA_email")

##Datacheck
cls_email <- colSums (event_email[ ,c(3:11)])
cls_email_sum <- sum(cls_email)
cls_email_sum
sum (event_email$Prolia_email)


#dmail
event_dmail <- subset(event_email_dmail, channel == "DirectMail" & event_type == "Sent")%>%
               dcast(., External_Contact_Id + datum ~ campaign_brand, value.var = "campaign_brand", fun=length)%>%
               .[-which(.$External_Contact_Id == ""), ]
names(event_dmail) = c("Amgen_Customer_Id",   "datum",   "NA_dmail", 	"Kyprolis_dmail", 	"Neulasta_dmail", 	"Nplate_dmail", 	"Prolia_dmail", 	"Repatha_dmail", 	"Vectibix_dmail", 	"XGEVA_dmail")

#Datacheck
cls_dmail <- colSums (event_dmail_sub_wide[ ,c(3:10)])
cls_dmail_sum <- sum(cls_dmail)
sum (event_dmail_sub_wide$Prolia_dmail)


#webtracking
event_webtr_1 <- subset(event_webtr_all, Amgen_Customer_Id!="")%>%
                 left_join(., event_webtr_brandta, by = c("Site_Name"="Site_Name"))%>%
                 left_join(., hcp_brandta, by = c("Amgen_Brand_ID"="ï..Amgen_Brand_ID", "Amgen_TA_ID"="Amgen_TA_ID"))
event_webtr <- event_webtr_1 [ ,c("Amgen_Customer_Id", "datum", "brand_ta")]%>%
               dcast(., Amgen_Customer_Id + datum ~ brand_ta, value.var = "brand_ta", fun=length)
names(event_webtr) = c("Amgen_Customer_Id", "datum", "Kyprolis_webtr", "Prolia_webtr", "Repatha_webtr", "XGEVA_webtr", "NA_webtr")

#Datacheck
cls_web <- colSums (event_webtr[ ,c(3:7)])
cls_web_sum <- sum(cls_web)

###Calls
event_call_details_1 <- left_join(event_call_details, hcp_brandta, by = c("Amgen_Brand_ID"="ï..Amgen_Brand_ID", "Amgen_TA_ID"="Amgen_TA_ID"))%>%
                        dcast(., Call_ID ~ brand_ta, value.var = "brand_ta", fun=length)
event_calls_1 <- left_join(event_calls_input, event_call_details_1, by = c("Call_ID"="Call_ID"))%>%
               subset(., Call_Type == "Call")
event_calls <- event_calls_1 [ , c(4, 34:47)]
event_calls_final <- data.frame (aggregate(. ~ Amgen_Customer_ID + datum, data = event_calls,FUN = sum))
names(event_calls_final) = c("Amgen_Customer_Id", "datum", "Aranesp.Oncology_calls",   "Blincyto_calls",   "IMLYGIC_calls", 	"Kyprolis_calls", 	"Mimpara_calls", 	"Neulasta_calls", 	"Neupogen_calls", 	"Nplate_calls", 	"Prolia_calls", 	"Repatha_calls", 	"Vectibix_calls", 	"XGEVA_calls", 	"NA._calls")


### 1.3) join all promo data

df <- join_all(list(event_calls_final ,event_webtr,event_dmail, event_email), by=c("Amgen_Customer_Id","datum"), type='full')
df[is.na(df)] <- 0
length (unique (df$Amgen_Customer_Id))


##write onekey information to promodata of Amgen
df_onekey <- left_join(df, hcp_profmaster_final [ ,c(1,2,3,5,8,9,11,17,19.20,27,28,33,128)], by = c("Amgen_Customer_Id"="Amgen_Customer_ID"))%>%
             subset (., Nanobrick != "NA")


#Data Check
cls_df_onekey <- colSums (df_onekey[ ,c(3:37)])
sum(cls_df_onekey) # 418.690 vs. 287.273

##-> Data loss due to filtering by prescriber and not existing nanobrick information




#############
## 1.4 DF Prolia
#############

# Extract Prolia Data from Amgen event data, group by brick+month
df_onekey_prolia <- df_onekey [ ,c(1,2,11, 17, 25, 34, 49)] %>%
                    subset(., Nanobrick != "NA")
df_onekey_prolia_agg <- data.frame (aggregate(df_onekey_prolia [ ,c(3:6)], by=list(df_onekey_prolia$Nanobrick, df_onekey_prolia$datum), FUN=sum))


#Salesdata prolia
proliavo <-read.csv (paste0(data_path_1,"proliavo.csv"), header=T, sep=";", quote = "", row.names = NULL, stringsAsFactors = FALSE)
proliavo_avg<-read.csv (paste0(data_path,"proliavo_avg.csv"), header=T, sep=";", quote = "", row.names = NULL, stringsAsFactors = FALSE)
proliavo_wettb <-read.csv (paste0(data_path_1,"proliavo_wettb.csv"), header=T, sep=";", quote = "", row.names = NULL, stringsAsFactors = FALSE)

names (proliavo) <- c("brick", "1-11-2016",  "1-10-2016",	"1-9-2016",	"1-8-2016",	"1-7-2016",	"1-6-2016",	"1-5-2016",	"1-4-2016",	"1-3-2016",	"1-2-2016",	"1-1-2016",	"1-12-2015",	"1-11-2015",	"1-10-2015",	"1-9-2015",	"1-8-2015",	"1-7-2015",	"1-6-2015",	"1-5-2015",	"1-4-2015",	"1-3-2015",	"1-2-2015",	"1-1-2015")
proliavo_1 <- melt(proliavo, id.vars=c("brick"), variable.name = "datum", value.name = "proliavo")
proliavo_1[is.na(proliavo_1)] <- 0
proliavo_1$datum <- as.character (proliavo_1$datum)


names (proliavo_wettb) <- c("brick", "1-11-2016",  "1-10-2016",  "1-9-2016",	"1-8-2016",	"1-7-2016",	"1-6-2016",	"1-5-2016",	"1-4-2016",	"1-3-2016",	"1-2-2016",	"1-1-2016",	"1-12-2015",	"1-11-2015",	"1-10-2015",	"1-9-2015",	"1-8-2015",	"1-7-2015",	"1-6-2015",	"1-5-2015",	"1-4-2015",	"1-3-2015",	"1-2-2015",	"1-1-2015")
proliavo_wettb_1 <- melt(proliavo_wettb, id.vars=c("brick"), variable.name = "datum", value.name = "proliavo_wettb")
proliavo_wettb_1[is.na(proliavo_wettb_1)] <- 0
proliavo_wettb_1$datum <- as.character (proliavo_wettb_1$datum)

## JOIN Prolia Sales with comp
prolia_tot <- left_join (proliavo_1, proliavo_wettb_1, by = c("brick"="brick", "datum"="datum"))

#Data Check
sum (prolia_tot$proliavo_wettb) #333894
sum (prolia_tot$proliavo) #421867
#tot:755761



#########start new modelings here with different brick numbers


#join sales+promo
df_final <- left_join (prolia_tot,df_onekey_prolia_agg, by = c("brick"="Group.1", "datum"="Group.2") )
df_final[is.na(df_final)] <- 0


###########
#Filtering: bricks without promo&sales, top bricks, relevant Months
############

#Filter bricks without VO&Promo
df_agg_brick <- data.frame (aggregate(df_final[ ,c(3, 5:8)], by=list(df_final$brick), FUN=sum))
df_agg_brick$vo_promo_sum <- df_agg_brick$proliavo + df_agg_brick$Prolia_calls + df_agg_brick$Prolia_webtr	+ df_agg_brick$Prolia_dmail	+df_agg_brick$Prolia_email
df_agg_brick<- subset (df_agg_brick, vo_promo_sum > 0 )
bricks_filter1 <- df_agg_brick$Group.1
df_final <- subset(df_final, brick %in% bricks_filter1)


##Top bricks
df_final <- left_join (df_final, hcp_innano, by= c("brick"="Nanobrick"))
df_final$proliavo_norm <- df_final$proliavo/df_final$hcp_innano   ###normalize by nr. HCPS in Nanobrick
df_final$proliavo_wettb_norm <- df_final$proliavo_wettb/df_final$hcp_innano ###normalize by nr. HCPS in Nanobrick
df_final$proliavo_norm[is.na(df_final$proliavo_norm)] <- 0
df_final$proliavo_wettb_norm[is.na(df_final$proliavo_wettb_norm)] <- 0
length(unique(df_final$brick))
df_final_filter <- subset  (df_final, df_final$hcp_innano > 2 )###Data privacy:at least 3 HCPs in nano
length(unique(df_final_filter$brick))
#6178 Bricks >2

df_final_filter<- df_final_filter[complete.cases(df_final_filter[,10]),]
summary (df_final_filter$hcp_innano)
length(unique(df_final$brick))

top_bricks_1 <- data.frame (aggregate(df_final_filter [ ,c(11)], by=list(df_final_filter$brick), FUN=sum))
names (top_bricks_1) <- c("brick", "proliavo_norm")
top_bricks_sort <- top_bricks_1[order(-top_bricks_1$proliavo_norm),]
#top50_sales <- head (top50_sort$proliavo_norm, n=260)
#top50_sales_1 <-tail (top50_sales, n=260)
top_bricks <- top_bricks_sort [ 1:30, c("brick")]    #-----> define top xx brciks for modeling
summary (df_final$proliavo_norm)


#df, filtered by top bricks
df_final_1 <- subset(df_final, brick %in% top_bricks)%>%
              left_join (., date_mapping, by= c("datum"="date"))%>%
              left_join (., proliavo_avg, by = c("lfnd_date"="lfnd"))%>%
              left_join (., google, by = c("lfnd_date"="lfnd"))%>%
              left_join (., quartal, by = c("lfnd_date"="lfnd_date"))
df_final <- df_final_1[order(-df_final_1$brick, df_final_1$lfnd_date),]%>%
            subset (., lfnd_date > 12)
df_final_orig <- df_final


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
} %>%
      filter(!is.na(eur_sales) & !is.na(actual_promo_id_count)) %>%#[1] 1401   13
      
df_final <- df
#################################################
# Set Some Constants
#################################################



prod <- "prolia"
IDs <- length (unique(df_final$final_segment))                   # Number of Nanobricks
T1 <- 12                       # Number of total Time Periods
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
df_final <- temp1 %>% bind_cols(data.frame(event=ifelse(df_final$date=="2015-12-01", 1, 0)))      

##########
## Normalize data to brick size
##########

# set up promo var and size adjustment
promo_var <- c("call", "meeting_epu", "meeting_national", "meeting_international", "meeting_other")
# promo_var_size_adj <- c("hcp_innano", "hcp_innano", "hcp_innano")
promo_var_size_adj <- c("cohort_count_fromseg")

# size adjust promo variables
for( i in 1:length(promo_var)) {
  v <- paste0(promo_var[i], "_adj")
  df_final[, v] <- df_final[, promo_var[i] ] / df_final[, promo_var_size_adj ]
}

#########
## Build adstocks
########

# promo_var1 <- c("Prolia_calls_norm", "Prolia_dmail_norm", "Prolia_email_norm")
firstmon <- 3
Retain <- c(0.4, 0.2, 0.2, 0.2, 0.2)  # Change
stk_var <- paste0(promo_var, "_adj")
X1 <- df_final[, c("final_segment","date", stk_var)]
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

ctrl <- df_final [ , c("lfnd_date", "bevorratung", "summerbreak_1", "summerbreak_2", "qu1", "qu2", "qu3","proliavo_wettb_norm","google_prolia",  "google_prolia_movavg_1",  "google_prolia_movavg_2", "proliavo_avg6", "proliavo_avg2", "proliavo_avg4", "proliavo_avg6",	"proliavo_avg8",	"proliavo_movavg1",	"proliavo_movavg2",	"proliavo_movavg3" )]
ctrl_var <- c("lfnd_date", "proliavo_wettb_norm","summerbreak_1","proliavo_avg6, summerbreak_1", "summerbreak_2", "qu1", "qu2" )
X <- as.matrix (X3)
# y <- df_final [ , c("proliavo", "proliavo_norm", "brick", "datum")]
y <- df_final[, c('prescriptions', 'final_segment', 'date')]
ctrl$proliavo_wettb_norm_scale <- scale( ctrl$proliavo_wettb_norm, center = F, scale = max(ctrl$proliavo_wettb_norm) )


############
## 2) MODELING
############

######################
## 2.1) Simple mixed model to gain priors
#####################

require(lme4)

# raise to root
rt_test <- c(0.695, 0.588, 0.607, 0.588, 0.607)
X3_rt <- X3
X3_rt_1 <- X3_rt #added by Jie to initialize X3_rt_1
for(i in 1:length(rt_test)) {
  X3_rt_1[, i] <- X3_rt_1[, i]^rt_test[i]
}

mod_data <- data.frame(y, X3_rt,ctrl )

#Mixed Model
lmm <- lmer(proliavo_norm ~  proliavo_wettb_norm  + lfnd_date+ summerbreak_1 + Prolia_calls_adj+Prolia_email_adj+Prolia_dmail_adj +( 1+lfnd_date+proliavo_wettb_norm+summerbreak_1|brick), data = mod_data)
summary(lmm)$coefficients
require(car)
Anova(lmm)
coef(lmm)

#Coefficients
coef <- data.frame (coef(lmm)[[1]])
coef$brick<-rownames(coef)
mod_data1 <- mod_data
mod_data1$brick <- as.character (mod_data1$brick)
mod_data1$proliavo_wettb_norm_scale <- as.numeric (mod_data1$proliavo_wettb_norm_scale)
model_df <- left_join (mod_data1, coef, by= c("brick"="brick"))
model_df$sum_fitted <- model_df$X.Intercept. + model_df$lfnd_date.x * model_df$lfnd_date.y+model_df$summerbreak_1.x *model_df$summerbreak_1.y + model_df$proliavo_wettb_norm.x*+ model_df$proliavo_wettb_norm.y+ model_df$Prolia_calls_adj.x*model_df$Prolia_calls_adj.y+ model_df$Prolia_email_adj.x*model_df$Prolia_email_adj.y+model_df$Prolia_dmail_adj.x*model_df$Prolia_dmail_adj.y

# find R-squared & MAPE
model_df$mape <- abs ((model_df$proliavo_norm-model_df$sum_fitted)/model_df$proliavo_norm)
mean (model_df$mape)
1 - sum( (model_df$sum_fitted - model_df$proliavo_norm)^2 ) / sum( (model_df$proliavo_norm-mean(model_df$proliavo_norm))^2 )




######################
## 2.2) MCMC approach
#####################

# Set Some Constants
path_fun <- "Z:\\Departments\\STO\\PROJECTS\\AdvancedAnalytics\\Functions\\"
source(paste0(path_fun, "PromoMix Functions v3.txt"))
ctrl_var_1 <- c("lfnd_date", "proliavo_wettb_norm", "summerbreak_1")
ctrl <- df_final [ , c("lfnd_date", "proliavo_wettb_norm", "summerbreak_1")]
X <-as.matrix(cbind(X3, ctrl))
atts1 <- length(stk_var)                      # Number of Promotional Variables
atts2 <-  length (ctrl_var_1)                   # Number of Non Promotional Variables (not intercept)
dimbeta <- atts1+atts2           # Total Number of Variables
T_mod <- 11                    # Number of Time Periods for modeling
iters<-300                    # Number of MCMC iterations
p=c(0.5, 0.5, 0.5) # Set prior for roots (Point estimate and number of prior observations)
d1<-1 # Intercept #  Set Indicators for random effects (1 --> Random effects,0-->no random effects, d1 is only for intercept, d2 is for promo and nopromo vars)
d2<-c(1, 1, 1, 1,0,0)   # Remaining Model parameters
y1 <-df_final [ , c("proliavo_norm")]

## Function for MCMC
myloop <- function(Rname ,Rmu,Rprec,RM) {
  name = Rname              # Name of Output File
  mu=Rmu
  prec=Rprec
  M=RM
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
  out<-PromoMix(data=data,inits=inits,parameters=parameters,n.chains=3,n.iter=iters,debug=T,atts1=atts1,atts2=atts2,T=T_mod,name=name)
  out
}


#Model 02
name1="Prolia"
mu1=c( 0.25,  0.05410439,	0.006220564,0.0143651868,0.1229733276,-0.1533202374,-0.6432366143)   # Change
prec1=c( 0.9604, 0.9604, 0.9604, 0.9604, 0.9604, 0.9604, 0.9604) # Change
M1=c(50, 50, 50)  # Change
a1R<-myloop(name1 ,mu1,prec1,M1)
cbind(colnames(X), a1R$Mbeta)
a1R$Roots




##Calculate contributions
## Read in Mean-file of contributions
coefs <- read.csv (paste0(data_path, "coef_input_clean.csv"), header=T, sep=";", quote = "", row.names = NULL, stringsAsFactors = FALSE)

##Transform decayed promotions with roots from mcmcm
rt_test <- c(0.824605777777778, ,0.79929, 0.817096222222222)
X3_rt_3 <- tail (X3, n=23958)
for(i in 1:length(rt_test)) {
  X3_rt_3[, i] <- X3_rt_3[, i]^rt_test[i]
}

##transform due to german ,=.
coefs$beta_call<- as.numeric(gsub(",", ".", gsub("\\.", "", coefs$beta_call)))
coefs$beta_dmail<-as.numeric(gsub(",", ".", gsub("\\.", "", coefs$beta_dmail)))
coefs$beta_email<-as.numeric(gsub(",", ".", gsub("\\.", "", coefs$beta_email)))
coefs$beta_trend<-as.numeric(gsub(",", ".", gsub("\\.", "", coefs$beta_trend)))
coefs$beta_comp<-as.numeric(gsub(",", ".", gsub("\\.", "", coefs$beta_comp)))
coefs$beta_summerbreak<-as.numeric(gsub(",", ".", gsub("\\.", "", coefs$beta_summerbreak)))
coefs$int<-as.numeric(gsub(",", ".", gsub("\\.", "", coefs$int)))

#Betas on brick level
mod_data <- data.frame(y, X3_rt,ctrl )
mod_data_beta <- left_join (mod_data [,-c(27)],coefs, by = c("brick"="brick") ) %>%
	               left_join (.,hcp_innano, by = c("brick"="Nanobrick") )%>%
	               left_join (.,df_final [,c(1:2, 6, 8:9)], by = c("brick"="brick", "datum"="datum") )

#Contributions
mod_data_beta_cont <- mod_data_beta
mod_data_beta_cont$cont_call <- mod_data_beta_cont$beta_call*mod_data_beta_cont$Prolia_calls_adj
mod_data_beta_cont$cont_email <- mod_data_beta_cont$beta_email*mod_data_beta_cont$Prolia_email_adj
mod_data_beta_cont$cont_dmail <- mod_data_beta_cont$beta_dmail*mod_data_beta_cont$Prolia_dmail_adj
mod_data_beta_cont$cont_int <- mod_data_beta_cont$int
mod_data_beta_cont$cont_comp <- mod_data_beta_cont$beta_comp*mod_data_beta_cont$proliavo_wettb_norm
mod_data_beta_cont$cont_summerbreak <- mod_data_beta_cont$beta_summerbreak*mod_data_beta_cont$summerbreak_1
mod_data_beta_cont$cont_trend <- mod_data_beta_cont$lfnd_date*mod_data_beta_cont$beta_trend
contributions <- data.frame (colSums (mod_data_beta_cont [, c(38:44)], ))

#Revenue
mod_data_beta$ums_call_norm <- mod_data_beta$beta_call*mod_data_beta$Prolia_calls_adj *238 ##-> price per package
mod_data_beta$ums_dmail_norm <- mod_data_beta$beta_dmail*mod_data_beta$Prolia_dmail_adj*238
mod_data_beta$ums_email_norm <- mod_data_beta$beta_email*mod_data_beta$Prolia_email_adj*238

#Costs
mod_data_beta$cost_calls <- mod_data_beta$Prolia_calls_adj*86.21628584 ###-> Costs for 1 activity
mod_data_beta$cost_dmail <- mod_data_beta$Prolia_dmail_adj*1.6
mod_data_beta$cost_email <- mod_data_beta$Prolia_email_adj*1.6
model_data_beta_agg <- data.frame (aggregate(mod_data_beta [ ,c(1, 2, 5:7,35:43)], by=list(mod_data_beta$brick), FUN=sum))
model_data_beta_agg_1 <- left_join (model_data_beta_agg,hcp_innano, by = c("Group.1"="Nanobrick"))

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



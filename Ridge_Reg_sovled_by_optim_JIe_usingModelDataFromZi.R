

##
## The purpose of this function is to try different optimization function, including optimr and optimrx; besides, also try to modify the functions to exclude 
## lambda (penalty factor) from the scope of optimization.


rm(list = ls())

#Load packages
library(doParallel)
library(dplyr)
library(xlsx)
# load in raw data
# root_path <- "D:/Project_Files3/EURO/MCM/BD"
root_path <- "../Data/RidgeReg/"

destination_folder <- "Ridge Reg"  #update
rawdata_file_name <- "mod_data_from_zi.csv"  #update
n_promo <- 5         # it is only for promotional variables  --  update
n_nonpromo <- 4      # it includes both the intercept and non-promo variables  -- update 
nonpromo_varnames <- c("log_trend", "neg", "pos")
y_position <- 2   # update according to rawdata_input below

# setwd(paste(root_path,destination_folder, sep="/"))

rawdata0 <- read.csv(paste0(root_path, "for_rr_stk_rt.csv"), stringsAsFactors = FALSE)
rawdata2 <- read.table(paste0(root_path, "df_trev_xep_adj_01.11.csv"), sep = ";", dec = ",", header = TRUE, stringsAsFactors = FALSE)
names(rawdata0) <- tolower(names(rawdata0))
names(rawdata2) <- tolower(names(rawdata2))

rawdata2 <- rawdata2 %>% mutate(date=as.character(as.Date(paste0(as.character(year_month), '01'), "%Y%m%d")))

promo_varnames <- grep("_stk_rt$", colnames(rawdata0), value = TRUE)
cohort_column_name <- "landscape"
sales_var_name <- "std_units_trev_xep_std"
date_var_name <- 'date'
# std channel using cohort mean
# varVal <- lazyeval::interp(~cohort
#                             , cohort=as.name(cohort_var))
newFml <- lazyeval::interp(~as.numeric(gsub('\\d{4}\\W(\\d{2})\\W\\d{2}', '\\1', tar_var, perl=T))
                           , tar_var=as.name(date_var_name)
)
meanBycohort <- rawdata0 %>%
      dplyr::select(one_of(c(cohort_column_name, promo_varnames))) %>%
      # group_by_(.dots=setNames(list(varVal), "cohort"))
      group_by_(.dots=setNames(list(cohort_column_name), cohort_column_name)) %>%
      summarise_all(c('mean', 'min'), na.rm=T) %>%
      select(-one_of(grep('_min$', names(.), value=T)))

rawdata <- rawdata0 %>% 
      
      left_join(select(rawdata2, one_of(c(cohort_column_name, 'date', sales_var_name))), by=c('landscape'='landscape', 'date'='date')) %>%
      # cbind.data.frame(select(rawdata2, one_of(sales_var_name))) %>%
      mutate_(.dots=setNames(list(newFml), 'month')) %>%
      mutate_(.dots=setNames(list(lazyeval::interp(~log(1+tar_var), tar_var=as.name('month'))), 'log_trend')) %>%
      #       mutate(log_trend=log(1+month))
      mutate(pos=ifelse(month %in% c(3,7), 1, 0)) %>%
      mutate(neg=ifelse(month %in% c(8,5,2), 1, 0)) %>%
      select(-x) %>% 
      {
            dtLastStep <- .
            
            
            tt_mean <<- select(dtLastStep, one_of(c(cohort_column_name, sales_var_name))) %>% 
                  group_by(.dots=setNames(list(cohort_column_name), cohort_column_name)) %>%
                  summarise_all('mean', na.rm=T) %>%
                  setNames(c(cohort_column_name, paste0(sales_var_name,'_mean'))) %>%
                  # select_(.dots=setNames(list(cohort_column_name), cohort_column_name)) %>%
                  left_join(data.frame(dtLastStep), by=cohort_column_name) 
            
            return(tt_mean)
            
      } %>%
      # mutate_(dots=setNames(list(lazyeval::interp(~v1/v2, v1=as.name(sales_var_name), v2=as.name(paste0(sales_var_name, '_mean')))), sales_var_name))
      mutate(std_units_trev_xep_adj=ifelse(std_units_trev_xep_adj_mean ==0, 0, std_units_trev_xep_adj/std_units_trev_xep_adj_mean)) %>%
      select(-one_of(c(paste0(sales_var_name, '_mean'))))

      

rawdataStd <- left_join(rawdata, meanBycohort, by=cohort_column_name) %>%
      select(one_of(grep('_rt$|_mean$', names(.), value=T))) %>% 
      {
            dtLastStep <- .
            std <- lapply(promo_varnames, function(v){
                  vct <- dtLastStep[[paste0(v, '_mean')]]
                  std <- ifelse(vct != 0
                        , dtLastStep[[v]]/vct
                        , 0)
                  return(std)
            }) %>%
                  do.call(cbind.data.frame,.) %>%
                  setNames(promo_varnames)
                  
            return(std)
            
      } %>%
      bind_cols(data.frame(select(rawdata, one_of(nonpromo_varnames, 'date', 'month', cohort_column_name, sales_var_name))))
#       cbind.data.frame(select(rawdata, one_of(c(cohort_column_name, 'date')))) %>%
#       cbind.data.frame(select(rawdata2, one_of(sales_var_name))) %>%
#       mutate_(.dots=setNames(list(newFml), 'month')) %>%
#       mutate_(.dots=setNames(list(lazyeval::interp(~log(1+tar_var), tar_var=as.name('month'))), 'log_trend')) %>%
# #       mutate(log_trend=log(1+month))
#       mutate(pos=ifelse(month %in% c(3,7), 1, 0)) %>%
#       mutate(neg=ifelse(month %in% c(8,5,2), 1, 0))

# trend2 <-1:T1
# for_model2$log_trend2<-log(1+for_model2$trend2)
# 
# for_model2$Pos<-ifelse((for_model2$month==3)|(for_model2$month==7),1,0)
# for_model2$Neg<-ifelse((for_model2$month==8)|(for_model2$month==5)|(for_model2$month==2),1,0)


n_cohort <- n_distinct(rawdata0[,cohort_column_name])

rawdataStd <- read.csv(file=paste0(root_path, 'mod_data_from_zi.csv')
                       , stringsAsFactors = FALSE) %>%
      select(-one_of(c('X', 'std_units_trev_xep_adj'))) %>%
      setNames(c(cohort_column_name, date_var_name, sales_var_name, promo_varnames, nonpromo_varnames)) %>%
      mutate_(.dots=setNames(list(newFml), 'month'))


# re-order columns
# rawdata_input <- rawdataStd[ , c(cohort_column_name, sales_var_name, nonpromo_varnames, promo_varnames)] 
rawdata_input <- rawdataStd[ , c(cohort_column_name, sales_var_name, nonpromo_varnames, promo_varnames, 'date')] 
# rawdata_input <- rawdataStd[ , c(cohort_column_name, sales_var_name, nonpromo_varnames, promo_varnames)] 


# cohort level priors
cohort_promo_priors <- read.xlsx(paste0(root_path, 'priorByCohort.xlsx'), sheetIndex = 1, stringsAsFactors = FALSE) %>%
      append(., data.frame(seg_id=1:nrow(.)), 0) %>%
      as.data.frame() %>%
      select(-seg_id) %>%
      setNames(c(cohort_column_name, promo_varnames))
 
# rawdataStd <- left_join(cohort_promo_priors, by=cohort_column_name)     
cohort_promo_priors<- matrix(rep(c(0.102, 0.0072, 0.00083, 0.0034, 0.0008), n_cohort)
                    , ncol=length(promo_varnames)
                    , nrow=n_cohort
                    , byrow=T) %>%
      as.data.frame %>%
      bind_cols(data.frame(unique(rawdata_input[[cohort_column_name]]))) %>%
      setNames(c(promo_varnames, cohort_column_name))

# 
# cohort_promo_priors <- matrix(nat_priors, nrow = n_cohort, ncol = n_promo, byrow = TRUE)
# cohort_promo_priors <- cbind(c(1:n_cohort), cohort_promo_priors)
# colnames(cohort_promo_priors) <- c(cohort_column_name, grep('_adj_stk_rt$', national_priors[,'varNm'], value = TRUE))


# define some functions... 

# the RidgeRSS is updated -- make shrinkage on intercept optinal, the default is NOT to shrink on intercept.
RidgeRSS <- function(    b_lambda  # b_lambda consists of penalty lambda, intercept, non-promo variables, promo variables. 
                         , X
                         , y
                         , b_prior   # b_prior includes intercept, non-promo variables, promo variables.
                         , intercept_shrinkage = FALSE
                         , weight=NULL 
){  
  
  if(is.null(weight)) {weight=rep(1,length(y))} # if no weight input, then evenly assign weight
  
  if(intercept_shrinkage){
    
    return( sum( weight*(y - X%*%(b_lambda[-1]))^2 ) + b_lambda[1] * sum(((b_lambda[-1])-b_prior)^2) )
    
  } else {
    
    return( sum( weight*(y - X%*%(b_lambda[-1]))^2 ) + b_lambda[1] * sum(((b_lambda[-c(1,2)])-b_prior[-1])^2) )  # removed penalty for intercept
    
  }
}

###
#@
nnridge <- function(   X
                       , y
                       , b_prior          # b_prior includes intercept, non-promo variables, promo variables.
                       , b_lambda_init    # b_lambda_init consists of initials for penalty lambda, intercept, non-promo variables, promo variables. 
                       , b_lambda_lower=-Inf
                       , b_lambda_upper=Inf
                       , intercept_shrinkage = FALSE
                       , weight=NULL
){
  
  p = length(b_prior)
  bfgsOptimConst = optim(par=b_lambda_init,  # put par in the first place according to the document
                         RidgeRSS, X=X, y=y, b_prior=b_prior, intercept_shrinkage= intercept_shrinkage, weight = weight,
                         lower=b_lambda_lower, upper=b_lambda_upper,
                         method = 'L-BFGS-B')
  par_est = bfgsOptimConst$par
  return(list(lambda=par_est[1], b=par_est[-1]))
  
}
#@@
nnridge2 <- function(   X
                       , y
                       , b_prior          # b_prior includes intercept, non-promo variables, promo variables.
                       , b_lambda_init    # b_lambda_init consists of initials for penalty lambda, intercept, non-promo variables, promo variables. 
                       , b_lambda_lower=-Inf
                       , b_lambda_upper=Inf
                       , intercept_shrinkage = FALSE
                       , weight=NULL
){
  
  p = length(b_prior)
  bfgsOptimConst = optimr(par=b_lambda_init,  # put par in the first place according to the document
                         RidgeRSS, X=X, y=y, b_prior=b_prior, intercept_shrinkage= intercept_shrinkage, weight = weight,
                         lower=b_lambda_lower, upper=b_lambda_upper,
                         method = 'L-BFGS-B')
  par_est = bfgsOptimConst$par
  return(list(lambda=par_est[1], b=par_est[-1]))
  
}


###

# test one cohort  ---changed for testing purpose.
# s1 <- One_Ridge_Reg(tpdat_test, b_prior_test, lambda1_initial = 20)    

One_Ridge_Reg <- function(tpdat, b_prior
                          , lambda1_initial = 20
                          , lambda1_lower_bound = 1
                          , lambda1_upper_bound = 500
                          , method = 'optim'){
  
  
  
  x1 = tpdat[,-c(1, y_position, ncol(tpdat))]  # exclude ID and sales
  x1 = as.matrix(cbind(1, x1[ , "log_trend"], -x1[ , "neg"], x1[ , "pos"], x1[ , promo_varnames]))  # need manual setup...
  
  cc = tpdat[,1]
  
  date = tpdat[, ncol(tpdat)]
  
  y1 = tpdat[ , y_position]
  
  lambda1_init = lambda1_initial
  lambda1_lower = lambda1_lower_bound #lower bound
  lambda1_upper = lambda1_upper_bound  #upper bound
  if(method=='optim'){
    r1 = nnridge(X=x1
                   , y=y1
                   , b_prior=b_prior
                   , b_lambda_init=c(lambda1_init, b_prior)
                   , b_lambda_lower=c(lambda1_lower, rep(0, length(b_prior)))
                   , b_lambda_upper=c(lambda1_upper, rep(Inf, length(b_prior))))
                   
  }else if(method=="optimr"){
    
    r1 = nnridge2(X=x1
                   , y=y1
                   , b_prior=b_prior
                   , b_lambda_init=c(lambda1_init, b_prior)
                   , b_lambda_lower=c(lambda1_lower, rep(0, length(b_prior)))
                   , b_lambda_upper=c(lambda1_upper, rep(Inf, length(b_prior))))    
    
   }
  
  cat("Cohort ID = ", cc[1], "\n")
  cat("initial lambda: ", lambda1_init, "\n")
  cat("lambda lower bound: ", lambda1_lower, "\n")
  cat("lambda upper bound: ", lambda1_upper, "\n")
  cat("lambda selected: ", r1$lambda, "\n")  
  
  cat("estimated coefficients: ", r1$b, "\n")
  
  ypred = x1 %*% r1$b
  
  rsqr = 1- sum((y1 - ypred)^2)/sum((y1 - mean(y1))^2) # R-square
  
  cat("RSquare: ", rsqr, "\n")
  cat("Model for Cohort ID ", cc[1], " finished! \n")
  
  prediction = data.frame(cohort_id = cc[1], y=y1, ypred = ypred, date=date, stringsAsFactors = FALSE)
  
  return(list(cohort_id = cc[1], date_id=date, lambda = r1$lambda, beta = r1$b, rsquare = rsqr, pred = prediction
              , x=x1))
  
}


### test 
cohort_id_test <- 9211231

tpdat_test <- rawdata_input[rawdata_input[ , cohort_column_name] == cohort_id_test,]

b_prior_test <- cohort_promo_priors[ cohort_promo_priors[,cohort_column_name]==cohort_id_test, promo_varnames] %>%
      as.numeric()
b_prior_test <- c(rep(0,n_nonpromo), b_prior_test)
# names(b_prior_test) <- NULL

s1 <- One_Ridge_Reg(tpdat_test, b_prior_test, lambda1_initial = 20)    
s1b <- One_Ridge_Reg(tpdat_test, b_prior_test, method = 'optimr')



#######    write another version of function without optimizing the lambda1

RidgeRSS2 <- function(     b      # b consists of intercept, non-promo variables, promo variables. To be optimized on. No penalty lambda
                         , X
                         , y
                         
                         , b_prior   # b_prior includes intercept, non-promo variables, promo variables.
                         , lambda   # lambda for penalty
                         , intercept_shrinkage = FALSE
                         , weight=NULL 
){  
  
  if(is.null(weight)) {weight=rep(1,length(y))} # if no weight input, then evenly assign weight
  
  if(intercept_shrinkage){
    
    return( sum( weight*(y - X %*% b)^2 ) + lambda * sum((b-b_prior)^2) )
    
  } else {
    
    return( sum( weight*(y - X %*% b)^2 ) + lambda * sum((b[-1]-b_prior[-1])^2) )  # removed penalty for intercept
    
  }
}

nnridge3 <- function(    X
                       , y
                       , b_prior          # b_prior includes intercept, non-promo variables, promo variables.
                       , b_init    # b_lambda_init consists of initials for intercept, non-promo variables, promo variables. No penalty lambda 
                       , lambda_specified    # specified lambda
                       , b_lower=-Inf
                       , b_upper=Inf
                       , intercept_shrinkage = FALSE
                       , weight=NULL
){
  
  p = length(b_prior)
  bfgsOptimConst = optim(par=b_init,  # put par in the first place according to the document
                         RidgeRSS2, X=X, y=y, b_prior=b_prior, lambda = lambda_specified, intercept_shrinkage= intercept_shrinkage, weight = weight,
                         lower=b_lower, upper=b_upper,
                         method = 'L-BFGS-B')
  par_est = bfgsOptimConst$par
  return(list(lambda= lambda_specified, b=par_est))
  
}

One_Ridge_Reg2 <- function(tpdat, b_prior, lambda_specified = 20
                           # , lambda1_lower_bound = 1
                           # , lambda1_upper_bound = 500
                           # , method = 'optim'
                           ){
  
  
  
  x1 = tpdat[,-c(1, y_position)]  # exclude ID and sales
  x1 = as.matrix(cbind(1, x1[ , "log_trend"], x1[ , "pos"], -x1[ , "neg"], x1[ , promo_varnames]))  # need manual setup...
  
  cc = tpdat[,1]
  
  y1 = tpdat[ , y_position]
  
  # lambda1_init = lambda1_initial
  # lambda1_lower = lambda1_lower_bound #lower bound
  # lambda1_upper = lambda1_upper_bound  #upper bound

    r1 = nnridge3(X=x1
                 , y=y1
                 , b_prior=b_prior
                 , b_init= b_prior
                 , lambda_specified = lambda_specified
                 , b_lower=c(rep(0, length(b_prior)))
                 , b_upper=c(rep(Inf, length(b_prior))))
 
  
  cat("Cohort ID = ", cc[1], "\n")
  # cat("initial lambda: ", lambda1_init, "\n")
  # cat("lambda lower bound: ", lambda1_lower, "\n")
  # cat("lambda upper bound: ", lambda1_upper, "\n")
  cat("lambda specified: ", r1$lambda, "\n")  
  
  cat("estimated coefficients: ", r1$b, "\n")
  
  ypred = x1 %*% r1$b
  
  rsqr = 1- sum((y1 - ypred)^2)/sum((y1 - mean(y1))^2) # R-square
  
  cat("RSquare: ", rsqr, "\n")
  cat("Model for Cohort ID ", cc[1], " finished! \n")
  
  prediction = data.frame(cohort_id = cc[1], y=y1, ypred = ypred, stringsAsFactors = FALSE)
  
  return(list(cohort_id = cc[1], lambda = r1$lambda, beta = r1$b, rsquare = rsqr, pred = prediction, x=x1))
  

}

### test 
cohort_id_test <- 3

tpdat_test <- rawdata_input[rawdata_input[ , cohort_column_name] == cohort_id_test,]

b_prior_test <- cohort_promo_priors[ cohort_promo_priors[,cohort_column_name]==cohort_id_test, promo_varnames]  
b_prior_test <- c(rep(0,n_nonpromo), b_prior_test)
names(b_prior_test) <- NULL

s1 <- One_Ridge_Reg2(tpdat_test, b_prior_test, lambda_specified = 1)  #-- it is minor difference as above


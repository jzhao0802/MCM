
rm(list = ls())

#Load packages
library(doParallel)
library(dplyr)

# load in raw data
root_path <- "D:/Project_Files3/EURO/MCM/BD"

destination_folder <- "Ridge Reg"  #update
rawdata_file_name <- "data4brms.csv"  #update
n_promo <- 5         # it is only for promotional variables  --  update
n_nonpromo <- 4      # it includes both the intercept and non-promo variables  -- update 
nonpromo_varnames <- c("log_trend", "pos", "neg")
y_position <- 2   # update according to rawdata_input below

setwd(paste(root_path,destination_folder, sep="/"))

rawdata <- read.csv(paste(root_path, destination_folder, rawdata_file_name, sep = '/'), stringsAsFactors = FALSE)
cohort_column_name <- "final_segment"
sales_var_name <- "y1"

n_cohort <- n_distinct(rawdata[,cohort_column_name])

promo_varnames <- grep("_adj_stk_rt$", colnames(rawdata), value = TRUE)

# re-order columns
rawdata_input <- rawdata[ , c(cohort_column_name, sales_var_name, nonpromo_varnames, promo_varnames)] 


# cohort level priors
national_priors <- read.csv(paste(root_path, destination_folder, 'priors_inBrm.csv', sep = '/'), stringsAsFactors = FALSE)

nat_priors<- grep('_adj_stk_rt$', national_priors[,'varNm']) %>% national_priors[ . , 'mu']

cohort_promo_priors <- matrix(nat_priors, nrow = n_cohort, ncol = n_promo, byrow = TRUE)
cohort_promo_priors <- cbind(c(1:n_cohort), cohort_promo_priors)
colnames(cohort_promo_priors) <- c(cohort_column_name, grep('_adj_stk_rt$', national_priors[,'varNm'], value = TRUE))


# define some functions... 

# the RidgeRSS is updated -- make shrinkage on intercept optinal, the default is NOT to shrink on intercept.
RidgeRSS <- function(  X
                     , y
                     , b_prior   # b_prior includes intercept, non-promo variables, promo variables.
                     , b_lambda  # b_lambda consists of penalty lambda, intercept, non-promo variables, promo variables. 
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


nnridge <- function(  X
                    , y
                    , b_prior          # b_prior includes intercept, non-promo variables, promo variables.
                    , b_lambda_init    # b_lambda_init consists of initials for penalty lambda, intercept, non-promo variables, promo variables. 
                    , b_lambda_lower=-Inf
                    , b_lambda_upper=Inf
                    , intercept_shrinkage = FALSE
                    , weight=NULL
                    ){
  
  p = length(b_prior)
  bfgsOptimConst = optim(RidgeRSS, X=X, y=y, b_prior=b_prior, intercept_shrinkage= intercept_shrinkage, weight = weight,
                         par=b_lambda_init, lower=b_lambda_lower, upper=b_lambda_upper,
                         method = 'L-BFGS-B')
  par_est = bfgsOptimConst$par
  return(list(lambda=par_est[1], b=par_est[-1]))
  
}


# test one cohort
One_Ridge_Reg <- function(tpdat, b_prior, lower_bound_scale = 0.001){
  
  
  x1 = tpdat[,-c(1, y_position)]  # exclude ID and sales
  x1 = as.matrix(cbind(1, x1[ , "log_trend"], x1[ , "pos"], -x1[ , "neg"], x1[ , promo_varnames]))  # need manual setup...
  
  cc = tpdat[,1]
  
  y1 = tpdat[ , y_position]
  
  lambda1_init = 100
  lambda1_lower = lambda1_init * lower_bound_scale  #lower bound
  lambda1_upper = lambda1_init * 5                  #upper bound
  
  r1 = nnridge(  X=x1
               , y=y1
               , b_prior=b_prior
               , b_lambda_init=c(lambda1_init, b_prior)
               , b_lambda_lower=c(lambda1_lower, rep(0, length(b_prior)))
               , b_lambda_upper=c(lambda1_upper, rep(Inf, length(b_prior)))
               )
  
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
  
  prediction = data.frame(cohort_id = cc[1], y=y1, ypred = ypred, stringsAsFactors = FALSE)
  
  return(list(cohort_id = cc[1], lambda = r1$lambda, beta = r1$b, rsquare = rsqr, pred = prediction))
  
}

### test 
cohort_id_test <- 1

tpdat_test <- rawdata_input[rawdata_input[ , cohort_column_name] == cohort_id_test,]

b_prior_test <- cohort_promo_priors[ cohort_promo_priors[,cohort_column_name]==cohort_id_test, promo_varnames]  
b_prior_test <- c(rep(0,n_nonpromo), b_prior_test)
names(b_prior_test) <- NULL

s1 <- One_Ridge_Reg(tpdat_test, b_prior_test)
s2 <- One_Ridge_Reg(tpdat_test, b_prior_test, lower_bound_scale = 0.01)
### end of test

#Loop thru all chort IDs

outcome <- lapply(cohort_promo_priors$landscape, function(i){
  
  tpdat_test <- rawdata_input[rawdata_input[ , cohort_column_name] == i,]
  
  b_prior_test <- cohort_promo_priors[ cohort_promo_priors[,cohort_column_name]== i, promo_varnames] 
  
  b_prior_test <- c(rep(0,n_nonpromo), b_prior_test) %>% as.numeric()
  
  # names(b_prior_test) <- NULL
  
  out <- One_Ridge_Reg(tpdat_test, b_prior_test, lambda1_initial=0.01, lambda1_lower_bound = 0.01, lambda1_upper_bound = 0.02)
  
  return(out)
  
})


cohort_model_fit <- lapply(1:length(cohort_promo_priors$landscape), function(i){
  
  return(c(outcome[[i]][["cohort_id"]], outcome[[i]][["rsquare"]], outcome[[i]][["lambda"]]
           , outcome[[i]][["beta"]]))
  
  }) %>% do.call(rbind.data.frame, . )
colnames(cohort_model_fit) <- c(cohort_column_name, "RSquare", "Lambda", "Intercept_beta"
                                , paste0(c(nonpromo_varnames, promo_varnames), '_beta')
                                )

# added by Jie
cohort_model_fit_addx <- lapply(outcome, function(X)data.frame(X$cohort_id, X$date_id, X$x)) %>% do.call(rbind.data.frame,.) %>%
      setNames(c(cohort_column_name, "date", "Intercept", nonpromo_varnames, promo_varnames)) %>%
      left_join(cohort_model_fit, by=c("landscape"="landscape"))


cohort_model_pred <- lapply(1:n_cohort, function(i){
  
  return(cbind(outcome[[i]][["pred"]], date=outcome[[i]]$date_id))
  
})  %>% do.call(rbind.data.frame, .)

fit_pred <- left_join(cohort_model_fit_addx, cohort_model_pred, by=c('landscape'='cohort_id', 'date'='date'))
sum(is.na(fit_pred$ypred))

var4Cont <- c('Intercept', promo_varnames, nonpromo_varnames)
cont <- lapply(var4Cont, function(v)fit_pred[, v]*fit_pred[, paste0(v, '_beta')]) %>%
      do.call(cbind.data.frame, .) %>%
      setNames(paste0(var4Cont, '_cont')) %>%
      bind_cols(data.frame(fit_pred[, c(cohort_column_name, 'date', 'ypred')])) %>%
      mutate_if(is.factor, as.character)

# check 
sum(apply(cont %>% select(one_of(grep('_cont$', names(.), value=T))), 1, sum)!=cont$ypred)
summary(apply(cont %>% select(one_of(grep('_cont$', names(.), value=T))), 1, sum) - cont$ypred)

# multiply by mean(y)
raw <- read.csv(file=paste0(root_path, 'mod_data_from_zi.csv')
                , stringsAsFactors = FALSE) 
names(raw) <- tolower(names(raw))
y_mean <- mean(rawdata2$std_units_trev_xep_adj)

fit_pred_multiply_toOutput <- cont %>% 
      
      select(one_of(grep('_cont$|ypred', names(.), value=T))) %>% 
      mutate_all(funs(.*y_mean)) %>%
      bind_cols(cont %>% select(-one_of(grep('_cont$|ypred', names(.), value=T)))) %>%
      left_join(raw %>% 
                      select(one_of(c(cohort_column_name, 'date', 'std_units_trev_xep_adj')))
                , by=c('landscape'='landscape', 'date'='date')) %>%
      setNames(gsub("_cont", '', names(.))) %>%
      select(one_of(c( 'date', cohort_column_name, 'std_units_trev_xep_adj', 'ypred', 
                       'Intercept', promo_varnames, nonpromo_varnames)))

# check
summary(apply(fit_pred_multiply_toOutput %>% select(one_of(var4Cont)), 1, sum, na.rm=T)-fit_pred_multiply_toOutput$ypred)
write.csv(fit_pred_multiply_toOutput, file=paste0(resultDir, 'contribution.csv'))

# added by Jie end




# check
sum(is.na(cohort_model_fit_add_orgYPred$ypred))
write.csv(cohort_model_fit_add_orgYPred, file=paste0(resultDir, 'cohort_model_fit_add_orgYpred.csv'))
write.csv(cohort_model_fit, file=paste0(resultDir, 'cohort_model_fit.csv'))



#overall rsquare
with(cohort_model_pred, 1 - sum((y-ypred)^2)/sum((y-mean(y))^2)   )

cohort_model_pred_mth <-cohort_model_pred %>% group_by(cohort_id) %>% mutate(month_id = 1:12) %>% ungroup()
 
monthly_pred <- cohort_model_pred_mth %>% group_by(month_id) %>% summarise_at(c("y","ypred"), sum)

with(monthly_pred, 1 - sum((y-ypred)^2)/sum((y-mean(y))^2))

write.csv(cohort_model_fit, "cohort_model_fit.csv", row.names=FALSE)

write.csv(cohort_model_pred, "cohort_model_pred.csv", row.names=FALSE)

write.csv(monthly_pred, "monthly_pred.csv", row.names=FALSE)

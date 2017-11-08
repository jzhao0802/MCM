
lambda_loop <- function(lmb){
      cat('lambda ::::', lmb, '\n')
      result_list <- lapply(1:nrow(cohort_promo_priors), function(i){
            cohort_id_test <- cohort_promo_priors$landscape[i]
            
            tpdat_test <- rawdata_input[rawdata_input[ , cohort_column_name] == cohort_id_test,]
            
            b_prior_test <- cohort_promo_priors[ cohort_promo_priors[,cohort_column_name]==cohort_id_test, promo_varnames] %>%
                  as.numeric()
            b_prior_test <- c(rep(0,n_nonpromo), b_prior_test)
            
            b_upper <- c(rep(Inf, n_nonpromo), b_upper)
            s1 <- One_Ridge_Reg(tpdat_test, b_prior_test
                                , lambda1_initial = lmb
                                , b_upper = b_upper
                                , lambda1_lower_bound = lambda_lower
                                , lambda1_upper_bound = lambda_upper
            )
            
            return(s1)
      })
      
      
      # clean up the result
      # prediction
     prediction <- lapply(result_list, function(X){
            return(X$pred)
      }) %>%
            do.call(rbind.data.frame, .) %>%
            # bind_cols(data.frame(month=rawdataStd$month[rawdataStd$landscape %in% cohort_promo_priors$landscape[1:nrow(cohort_promo_priors)]]))
           bind_cols(data.frame(month=rawdataStd$month))
           
      # rsquare by month
      rsquare_by_month <- prediction %>%
            #       group_by_(.dots=setNames(list(cohort_column_name), cohort_column_name)) %>%
            group_by(month) %>%
            summarise(rsquare=ifelse(sum((y-mean(y))^2)==0, NA, 1-sum((y-ypred)^2)/sum((y-mean(y))^2))) %>%
            summarise(mean(rsquare, na.rm=T) ) %>%
            as.numeric()
#       rsquare_by_month <- prediction %>%
#             group_by(month) %>%
#             summarise(y=mean(y), ypred=mean(ypred)) %>%
#             summarise(rsquare=ifelse(sum((y-mean(y))^2)==0, NA, 1-sum((y-ypred)^2)/sum((y-mean(y))^2))) %>%
            
      # rsquare by cohort
      
      rsquare_by_cohort <- prediction %>%
            #       group_by_(.dots=setNames(list(cohort_column_name), cohort_column_name)) %>%
            group_by(cohort_id) %>%
            summarise(rsquare=ifelse(sum((y-mean(y))^2)==0, NA, 1-sum((y-ypred)^2)/sum((y-mean(y))^2))) %>%
            summarise(mean(rsquare, na.rm=T)) %>%
            as.numeric()
      
      # rsquare total
#       rsquare_total <- prediction %>%
#             mutate(rsquare=1-sum((y-ypred)^2)/sum((y-mean(y))^2))
      rsquare_total <- 1-sum((prediction$y-prediction$ypred)^2)/sum((prediction$y-mean(prediction$y))^2)
      
      # coefficients & optimum lambda
      coefficients <- lapply(result_list, function(X){
            temp <- c(X$cohort_id, X$beta[-1], X$lambda, X$rsquare)
            return(temp)
            
      }) %>%
            do.call(rbind.data.frame, .) %>%
            setNames(c(
                  cohort_column_name
                  , paste0(nonpromo_varnames, '_coef')
                  , paste0(promo_varnames, '_coef')
                  , 'selected lambda'
                  , 'rsquare_byCht')) %>%
            left_join(cohort_promo_priors, by=cohort_column_name) %>%
            bind_cols(data.frame(lambda=rep(lmb, length(result_list))))
      
#       variation <- lapply(promo_varnames, function(v){
#             a <- (coefficients[, paste0(v, '_coef')]-coefficients[, paste0(v)])^2
#             return(a)
#       }) %>% do.call(unlist,.)
      
      res <- list(rsquare_by_month=rsquare_by_month
                  , rsquare_by_cohort=rsquare_by_cohort
                  , rsquare_total=rsquare_total
                  , coefficients_prior=coefficients
                  )
      
}

lambda_list <- c(seq(0.001, 0.01, 0.001), seq(0.02, 0.1, 0.01), seq(0.2, 1, 0.1), seq(2, 10, 1), seq(20, 100, 10)
                 , seq(200, 1000, 100), seq(2000, 10000, 1000)
                 )
# lambda_list <- c(0.01)
# results_loop_lambda <- lapply(lambda_list, function(i)lambda_loop(i))
b_upper <- c(0.102,	0.00936,	0.001079,	0.00442,	0.00104)

lambda_lower <- 0
lambda_upper <- 11000

library(snowfall)
sfInit(parallel=TRUE, cpus=4, type='SOCK')

sfExport('cohort_promo_priors', 'rawdata_input', 'rawdataStd', 'cohort_column_name', 'n_nonpromo'
         , 'y_position', 'promo_varnames', 'nonpromo_varnames', 'lambda_lower', 'lambda_upper'
         , 'b_upper'
)

sfExport('RidgeRSS', 'nnridge', 'nnridge2', 'One_Ridge_Reg', 'y_position', 'promo_varnames'
)

sfClusterEval(library("doParallel"))
sfClusterEval(library("dplyr"))
sfClusterEval(library("xlsx"))

# lambda_list <- c(1,2)
results_loop_lambda <- sfClusterApplyLB(lambda_list, lambda_loop)
sfStop()

rsquare_monthly_byLmb <- lapply(results_loop_lambda, function(X)X$rsquare_by_month) %>%
      do.call(rbind.data.frame, .) %>%
      setNames('rsquare_monthly_byLmb')

rsquare_cohort_byLmb <- lapply(results_loop_lambda, function(X)X$rsquare_by_cohort) %>%
      do.call(rbind.data.frame, .) %>%
      setNames('rsquare_cohort_byLmb')

rsquare_total_byLmb <- lapply(results_loop_lambda, function(X)X$rsquare_total) %>%
      do.call(rbind.data.frame, .) %>%
      setNames('rsquare_total_byLmb')

coef_prior_variation <- lapply(results_loop_lambda, function(X){
      X$coefficients_prior
}) %>% do.call(rbind.data.frame, .) %>%
{
      dtLastStep <- .
      variation <- lapply(promo_varnames, function(v){
            square_diff <- (dtLastStep[, paste0(v, '_coef')]-dtLastStep[, v])^2
            return(square_diff)
      }) %>%
            do.call(cbind.data.frame, .) %>%
            setNames(paste0(promo_varnames, '_diff_square')) %>%
            bind_cols(dtLastStep)
      
      
      return(variation)
}

# coefficient variation by each labmda
coef_variation_byLmb <- coef_prior_variation %>%
      select(one_of(c('lambda', 'selected lambda', grep('_diff_square$', names(.), value=T)))) %>%
      setNames(gsub("(\\w+)(\\s+)(\\w+)", "\\1_\\3_mean", names(.))) %>%
      mutate(selected_lambda_mean=selected_lambda_mean/n_cohort) %>%
      group_by(lambda) %>%
      summarise_all(sum)


ncohort.neg_rsquare <- coef_prior_variation %>%
      mutate(bNeg=(rsquare_byCht<0)) %>%
      group_by(lambda) %>%
      summarise(n.neg.rsquare=sum(bNeg, na.rm=T))


# column bind all the columns
dfTarget <- cbind.data.frame(
      lambda=select(coef_variation_byLmb, lambda)
      , lambda_selected_mean=select(coef_variation_byLmb, selected_lambda_mean)
      , rsquare_monthly_byLmb
      , rsquare_cohort_byLmb
      , rsquare_total_byLmb
      , ncohort.neg_rsquare$n.neg.rsquare
      , select(coef_variation_byLmb, -lambda, -selected_lambda_mean)
) %>% setNames(c('Initial lambda'
                 , 'Mean of Selected lambda'
                 , 'Rsquare_Overall (cohort and month level, i.e. cohort level)'
                 , 'Rsquare_National (rolled up to monthly, i.e. national level)'
                 , 'Rsquare total'
                 , "Number of cohort with Rsquare <0"
                 , paste0('Sum of squared difference between posterior coefficient and prior coefficient for ', promo_varnames)
                 )
               )

timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", destination_folder, '/', timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

write.csv(dfTarget, paste0(resultDir, 'dfTarget.csv')
          , row.names=F)




# get contribution
org_promo_varnames <- gsub('(\\w+)(_stk_rt$)', '\\1', promo_varnames)
for_mean_promo <- read.csv(file=paste0(root_path, 'for_model_updated_20171101.csv'), stringsAsFactors = F) %>%
      select(one_of(paste0(org_promo_varnames, '_adj'))) %>%
      apply(., 2, mean, na.rm=T) %>%
      setNames(c(promo_varnames))

for_ctl_mean <- read.csv(file=paste0(root_path, 'mod_data_beta_1.csv'), stringsAsFactors = F) %>%
      
      select(one_of(c('log_trend2', 'Neg', 'Pos'))) %>%
      apply(., 2, mean, na.rm=T) %>%
      setNames(nonpromo_varnames)


# promo_mean <- c(for_mean_promo, for_ctl_mean) %>%
#       
#       setNames(c(promo_varnames, nonpromo_varnames))

raw <- read.csv(file=paste0(root_path, 'mod_data_from_zi.csv')
                , stringsAsFactors = FALSE) 
names(raw) <- tolower(names(raw))
cohort_model_fit_add_orgYPred <- cohort_model_fit %>% as.data.frame %>%
      left_join(cohort_model_pred[, c('ypred', 'cohort_id', 'date')], by=c('landscape'='cohort_id')) %>%
      left_join(raw[, c(cohort_column_name, 'date', 'std_units_trev_xep_adj')],by=c("landscape"="landscape", 'date'='date')) %>%
      select(one_of(c( 'date', cohort_column_name, 'std_units_trev_xep_adj', 'ypred', 'Intercept', promo_varnames, nonpromo_varnames)))

check <- cohort_model_fit_add_orgYPred <- select(one_of(c('ypred', 'Intercept', promo_varnames, nonpromo_varnames))) %>%
      apply(.[, -1], sum, na.rm=T)

promo_mean <- rep(y_mean, 8) %>%
      #       
            setNames(c(promo_varnames, nonpromo_varnames))

contribution <- lapply(c(promo_varnames, nonpromo_varnames), function(v){
      return(cohort_model_fit_add_orgYPred[, v]*rawdataStd[, v]*promo_mean[[v]])
}) %>% do.call(cbind.data.frame, .) %>%
      setNames(paste0(c(promo_varnames, nonpromo_varnames))) %>%
      bind_cols(select(cohort_model_fit_add_orgYPred, one_of(c('date', cohort_column_name))))




contribution_output <- cohort_model_fit_add_orgYPred %>% 
      select(c(one_of('std_units_trev_xep_adj', 'ypred', 'Intercept'))) %>%
      mutate(Intercept=Intercept*1.313339) %>%
      bind_cols(contribution) %>%
      select(one_of(c( 'date', cohort_column_name, 'std_units_trev_xep_adj', 'ypred', 'Intercept', promo_varnames, nonpromo_varnames)))

# check 
contribution_output_check <- contribution_output %>%
      select(one_of(c('ypred', 'Intercept', promo_varnames, nonpromo_varnames))) %>%
      {
            dtLastStep <- .
            check <- apply(dtLastStep[, -1], 1, sum, na.rm=T)-dtLastStep[, 1]
            return(check)
      }


write.csv(contribution_output, paste0(resultDir, 'contribution.csv')
          , row.names=F)


# updated on Nov03 to get the correct contribution
# outcome is the result from a single labmda (0.01)
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



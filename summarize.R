
lambda_loop <- function(lmb){
      cat('lambda ::::', lmb, '\n')
      result_list <- lapply(1:nrow(cohort_promo_priors), function(i){
            cohort_id_test <- cohort_promo_priors$landscape[i]
            
            tpdat_test <- rawdata_input[rawdata_input[ , cohort_column_name] == cohort_id_test,]
            
            b_prior_test <- cohort_promo_priors[ cohort_promo_priors[,cohort_column_name]==cohort_id_test, promo_varnames] %>%
                  as.numeric()
            b_prior_test <- c(rep(0,n_nonpromo), b_prior_test)
            
            s1 <- One_Ridge_Reg(tpdat_test, b_prior_test, lambda1_initial = lmb
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
                  , coefficients_prior=coefficients)
      
}

lambda_list <- c(seq(0.01, 0.1, 0.01), seq(0.2, 1, 0.1), seq(2, 10, 1), seq(20, 100, 10))
# lambda_list <- c(0.01)
# results_loop_lambda <- lapply(lambda_list, function(i)lambda_loop(i))

lambda_lower <- 0.01
lambda_upper <- 150

library(snowfall)
sfInit(parallel=TRUE, cpus=4, type='SOCK')

sfExport('cohort_promo_priors', 'rawdata_input', 'rawdataStd', 'cohort_column_name', 'n_nonpromo'
         , 'y_position', 'promo_varnames', 'nonpromo_varnames', 'lambda_lower', 'lambda_upper'
)

sfExport('RidgeRSS', 'nnridge', 'nnridge2', 'One_Ridge_Reg', 'y_position', 'promo_varnames'
)

sfClusterEval(library("doParallel"))
sfClusterEval(library("dplyr"))
sfClusterEval(library("xlsx"))

# lambda_list <- c(1,2)
results_loop_lambda <- sfClusterApplyLB(lambda_list, lambda_loop)
sfStop()

rsqure_total <- 
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
      select(one_of(c('lambda', grep('_diff_square$', names(.), value=T)))) %>%
      group_by(lambda) %>%
      summarise_all(sum)

ncohort.neg_rsquare <- coef_prior_variation %>%
      mutate(bNeg=(rsquare_byCht<0)) %>%
      group_by(lambda) %>%
      summarise(n.neg.rsquare=sum(bNeg, na.rm=T))


# column bind all the columns
dfTarget <- cbind.data.frame(
      lambda=lambda_list
      , rsquare_monthly_byLmb
      , rsquare_cohort_byLmb
      , rsquare_total_byLmb
      , ncohort.neg_rsquare$n.neg.rsquare
      , select(coef_variation_byLmb, -lambda)
) %>% setNames(c('Initial lambda'
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




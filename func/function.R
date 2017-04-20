
runAdj <- function(v, df, nrx_var_size_adj, promo_var_size_adj){
      vct <- df[, v]
      vct_adj <- vct/df[, nrx_var_size_adj]
      return(vct_adj)
}

run_stk <- function(i, dtLastStep, firstmon, adj_var, T1, Retain){
      row_idx <- (1+T1*(i-1)):(T1+T1*(i-1))  # row index for current IDs
      Start <- colMeans( dtLastStep[row_idx[1:firstmon], adj_var] )
      temp <- Stock(dtLastStep[row_idx, adj_var], Start, Retain)## Stock and standardize Variables (Note set retention rates)
      return(temp) 
}

run_root <- function(i, dtLastStep, rt_test){
      temp <- dtLastStep[, i] ^ rt_test[i]
      return(temp)
}


runStd <- function(var2std, data){
      mean_vct <- data %>% dplyr::select(one_of(var2std)) %>% apply(., 2, mean)
      mod_data11 <- data %>% dplyr::select(one_of(var2std)) %>% sweep(., 2, mean_vct, '/') %>%
            bind_cols(data %>% dplyr::select(-one_of(var2std)))      
}

model_data_prepare <- function(bStd, nrx_var, rt_test
                               , salesVar4revenue=salesVar4revenue){
      df <- read.xlsx(file=paste0(data_path_2, 'for_model_data_0329.xlsx')
                      , sheetIndex=1
                      , header = T
      ) %>% #[1] 3182   13 
      {
            dtLastStep <- .
            names(dtLastStep) <- tolower(names(dtLastStep))
            dtLastStep[is.na(dtLastStep)] <- 0
            dtLastStep
      } %>%
            mutate(year=gsub("^(\\d{4})(.+$)", "\\1", date, perl=T)) %>%
            mutate(month=gsub('\\d{4}-(\\d{2})-\\d+', "\\1", date, perl=T))
      IDs <- length (unique(df[, IDs_var]))                   # Number of Nanobricks
      T1 <- length(unique(df[, T1_var]))
      records2rm <- which(df$year!=year2Rm)
      control_df <- data.frame(event1=ifelse(df$month %in% c('02'), 1, 0)
                               , event2=ifelse(df$month %in% c('08'), 1, 0))
      
      cat('1')
      
      df_final <- lapply(salesVars2adj, function(v)runAdj(v, df, nrx_var_size_adj, promo_var_size_adj[1])) %>%
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
            } %>%
            bind_cols(control_df)  %>%
            {
                  # size adjust promo variables
                  
                  dtLastStep <- .
                  for( i in 1:length(c(promo_var, nrx_var))) {
                        v <- paste0(c(promo_var, nrx_var)[i], "_adj")
                        dtLastStep[, v] <- dtLastStep[, c(promo_var, nrx_var)[i] ] / dtLastStep[, c(promo_var_size_adj, nrx_var_size_adj)[i]]
                  }
                  dtLastStep
            }
      cat('2')
      
      
      
      X_withStk <- df_final %>% dplyr::select(one_of(adj_var)) %>%
      {
            dtLastStep <- .
            temp <- lapply(1:IDs, function(i)run_stk(i, dtLastStep, firstmon, adj_var, T1, Retain)) %>%
                  do.call(rbind, .) %>%
                  as.data.frame()
            temp
      } %>%
            setNames(stk_var)
      cat('3')
      
      X_withRt <- X_withStk %>% {
            dtLastStep <- .
            temp <- lapply(1:length(stk_var), function(i)run_root(i, dtLastStep, rt_test)) %>%
                  do.call(cbind, .) %>%
                  as.data.frame() %>%
                  setNames(rt_var)
            temp
      }
      
      mod_data <- data.frame(df_final[, otherVars_inModel]
                             , X_withRt
                             , control_df
      ) %>%
            .[-records2rm,]
      
      X4Bayes <- X_withStk[-records2rm, ]
      
      if(bStd){
            var2std <- c(rt_var, ctrl_var, paste0(nrx_var, '_adj'))
            mod_data <- runStd(var2std=var2std, data=mod_data)
            
            X4Bayes <- X4Bayes %>% StandardStock(.)
      }
      # [1] 1032   11
      cat('4')
      sales_mean <- df_final[-records2rm, ] %>% dplyr::select(one_of(salesVar4revenue)) %>%
            sapply(., mean)
      
      temp_result <- list(mod_data4BaseLine=mod_data
                          , X4Bayes=X4Bayes
                          , sales_mean=sales_mean
      )
}




run_baseLine <- function(model_data, var_inModel, nrx_adj, formula){
      #       stk_var_inModel <- promo_var
#       nrx_adj <- paste0(nrx_var, '_adj')
#       f_eval(~f_interp(~lm(uqf(formula), data=model_data)))
      lmm <- lmer(formula, data = model_data)
      summary(lmm)$coefficients
      Anova(lmm)
      coef(lmm)
      
      #Coefficients
      coef <- data.frame (coef(lmm)[[1]])
      coef$final_segment<-rownames(coef)
      model_data$final_segment <- as.character(model_data$final_segment)
      model_df <- left_join (model_data, coef, by= c("final_segment"="final_segment"))
      model_df$sum_fitted <- predict(lmm)
      
      # find R-squared & MAPE
      model_df$mape <- abs ((model_df[, nrx_adj]-model_df$sum_fitted)/model_df[, nrx_adj])
      mape <- mean (model_df$mape[model_df$mape != Inf])
      Rsquare <- 1 - sum( (model_df$sum_fitted - model_df[, nrx_adj])^2 ) / sum( (model_df[, nrx_adj]-mean(model_df[, nrx_adj]))^2 )
      
      getMSE <- function(vct){
            mse <-vct^2
            return(mse)
      }
      varVal <- lazyeval::interp(~(x-sum_fitted)^2, x=as.name(nrx_adj))
      mse_bySeg <- model_df %>% dplyr::select(one_of(c("final_segment", "sum_fitted", nrx_adj))) %>%
            mutate_(.dots=setNames(list(varVal), 'mse')) %>%
            group_by(final_segment) %>%
            # summarise_each(funs(getMSE(.)), sum_fitted)
            # summarise_each(funs(getMSE))
            summarise(mse=sum(mse), cnt=n()) %>%
            mutate(mse=mse/cnt) %>%
            dplyr::select(-cnt) %>%
            .[order(.$mse, decreasing = T), ]
      
      
      result_temp <- list(model_df=model_df, mape=mape, Rsquare=Rsquare, mse_bySeg=mse_bySeg)
      return(result_temp)
}



run_bayes <- function(X4Bayes, model_data4BaseLine, prod, IDs_var, ctrl_var, var_inModel
                      , iters, p, d1, d2, nrx_var, mu1, prec1, M1, bStd){
      ctrl <- model_data4BaseLine[, ctrl_var]
      if(bStd){
            ctrl <- runStd(ctrl_var, ctrl)
      }
      X <- as.matrix(cbind(X4Bayes, ctrl))
      atts1 <- length(var_inModel)
      atts2 <- length(ctrl_var)
      dimbeta <- atts1 + atts2
      T_mod <- length(unique(model_data4BaseLine$date))
      y1 <- model_data4BaseLine[, nrx_var]
      if(bStd){
            y1 <- y1/mean(y1)
      }
      IDs <- length(unique(model_data4BaseLine[, IDs_var]))
      myloop <- function(X, Rname ,Rmu,Rprec,RM, IDs, resultDir) {
            name = Rname              # Name of Output File
            mu=Rmu
            prec=Rprec
            M=RM
            
            mean(X[,1:dimbeta]) # Check Standardization again
            data <- list(y=y1,X=X,d1=d1,d2=d2,atts1=atts1,atts2=atts2
                         , mu=mu,prec=prec,p=p,M=M,dimbeta=dimbeta
                         ,T=T_mod,IDs=IDs)# Setup "data" For Winbugs
            parameters <- c("root","betam","a","alpha","beta","Xb"
                            ,"stdevint","stdevdata","stdevbeta")# Set Parameters to Monitor
            betatemp <-     0*c(1:dimbeta) #->0 # set initial values for MCMC (should have no influence)
            precm    <- 1 + 0*c(1:dimbeta) #->1
            precdata <- 1 + 0*c(1:IDs) #->1
            inits1=list(betam=betatemp,precm=precm,precdata=precdata)  #-> starting point for every chain
            inits2=list(betam=betatemp,precm=precm,precdata=precdata)
            inits3=list(betam=betatemp,precm=precm,precdata=precdata)
            inits=list(inits1,inits2,inits3)
            
            # Run PromoMix Model
            out<-PromoMix(data=data,inits=inits,parameters=parameters
                          ,n.chains=3,n.iter=iters,debug=T,atts1=atts1
                          ,atts2=atts2,T=T_mod,name=name
                          , resultDir=resultDir)
            out
      }
      
      
      #Model 02
      
      a1R<-myloop(X=X, Rname=prod ,Rmu=mu1,Rprec=prec1
                  ,RM=M1, IDs=IDs, resultDir=resultDir)
      Mbeta <- cbind(colnames(X), a1R$Mbeta)
      Roots <- a1R$Roots
      temp_result <- list(Mbeta=Mbeta, Roots=Roots)
      return(temp_result)
}


run_roi <- function(inPath, outPath, promo_var, sales_mean, prod, dt_name, vars4rt, price_vct
                    , unitCosts_vct, ctrl_var, IDs, rt_test, model_data_list
                    , otherVars_inModel){
      means <- read.csv(paste0(inPath, prod, dt_name), stringsAsFactors = F)
      betam <- means[grep("^betam.+$", means$X, perl=T), 'Mean']
      beta <- means[grep("^beta\\W", means$X, perl = T), 'Mean']
      
      IDs <- length(unique(model_data_list$mod_data4BaseLine$final_segment))
      getBeta <- function(i, j){
            unlist(lapply(1:IDs, function(j)beta[i+length(betam)*(j-1)]))
      }
      coefs <- lapply(1:length(betam), function(i)getBeta(i, j)) %>%
            do.call(cbind, .) %>%
            as.data.frame()
      
      
      
      names(coefs) <- paste0('beta_',c(vars4rt, ctrl_var))
      coefs$seg <- 1:IDs
      

      data <- model_data_list$X4Bayes %>% dplyr::select(one_of(vars4rt)) %>%
      {
            dtLastStep <- .
            temp <- lapply(1:length(vars4rt), function(i)run_root(i, dtLastStep, rt_test)) %>%
                  do.call(cbind, .) %>%
                  as.data.frame() %>%
                  setNames(vars4rt)
            temp
      } %>%
            bind_cols(data.frame(model_data_list$mod_data4BaseLine)[, c(otherVars_inModel, ctrl_var)])
      
      
      mod_data_beta <- left_join(data, coefs, by=c('final_segment'='seg'))
      mod_data_beta_cont <- 
            lapply(c(vars4rt, ctrl_var), function(v)mod_data_beta[, v]*mod_data_beta[, paste0('beta_', v)]) %>%
            do.call(cbind, .) %>% 
            as.data.frame() %>%
            setNames(paste0('cont_', c(vars4rt, ctrl_var)))

      contributions <- colSums(mod_data_beta_cont)
      
      price_cost_df <- data.frame(promo_var=promo_var
                                  , price=price_vct
                                  , uniCost=unitCosts_vct
      )
      
      price_df <- t(price_cost_df[price_cost_df$promo_var %in% promo_var, 'price']) %>% 
            as.data.frame() %>%
            setNames(paste0('price_', promo_var, '_adj_stk'))
      mod_data_beta_ums <- lapply(c(vars4rt), function(v)
            mod_data_beta[, v]*mod_data_beta[, paste0('beta_', v)]*sales_mean) %>%
            do.call(cbind, .) %>%
            as.data.frame() %>%
            setNames(paste0('ums_', vars4rt, '_norm'))
      
      unitCosts_df <- t(price_cost_df[price_cost_df$promo_var %in% promo_var, 'uniCost']) %>%
            as.data.frame() %>%
            setNames(paste0('uniCost_', promo_var, '_adj_stk'))
      mod_data_beta_costs <- lapply(c(vars4rt), function(v)
            model_data_list$X4Bayes[, v]*unitCosts_df[, paste0('uniCost_', v)]) %>%
            do.call(cbind, .) %>%
            as.data.frame() %>%
            setNames(paste0('costs_', vars4rt))

      model_data_beta_agg <- cbind(mod_data_beta, mod_data_beta_ums, mod_data_beta_costs) %>%
            dplyr::select(one_of(c(grep('^(ums_|costs_)', names(.), value = T, perl = T)
                                   , 'final_segment'))) %>%
            dplyr::group_by(final_segment) %>%
            # dplyr::summarise_each(funs(mean), one_of(grep('^(ums_|costs_)', names(.), value = T, perl = T)))
            dplyr::summarise_each(funs(sum)) %>%
            {
                  dtLastStep <- .
                  roi_df <- lapply(vars4rt, function(v)
                        dtLastStep[, paste0('ums_', v, '_norm')]/
                              dtLastStep[, paste0('costs_', v)]) %>%
                        do.call(cbind, .) %>%
                        as.data.frame() %>%
                        setNames(paste0('roi_', vars4rt)) %>%
                        bind_cols(dtLastStep)
                  roi_df
            }
      
      write.csv(model_data_beta_agg, paste0(outPath, 'for_roi_qc.csv'), row.names = F)
      return(model_data_beta_agg)
}


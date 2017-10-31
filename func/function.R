
runAdj <- function(v, df, size){
      vct <- df[, v]
      vct_adj <- vct/df[, size]
      return(vct_adj)
}

run_stk <- function(i, dtLastStep, firstmon, adj_var, T1, Retain){
      row_idx <- (1+T1*(i-1)):(T1+T1*(i-1))  # row index for current IDs
      Start <- colMeans( dtLastStep[row_idx[1:firstmon], adj_var] )
      temp <- Stock(dtLastStep[row_idx, adj_var], Start, Retain)## Stock and standardize Variables (Note set retention rates)
      return(temp) 
}


getStart <- function(x, T, j)mean(x[((1+T*(j-1)):(3+T*(j-1)))])
run_stk1 <- function(j, X1, T, atts1, Retain){
      # Note: Change as needed --> Current assumes first 3 months representative of earlier period
      Start <- apply(X1, 2, getStart, T, j)
      
      # Stock and standardize Variables (Note set retention rates)
      t <- Stock(X1[((1+T*(j-1)):(T+T*(j-1))), 1:atts1], Start, Retain)	
      return(t)
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

model_data_prepare1 <- function(inPath, inFile, T1_var){
      varVal1 <- lazyeval::interp(~as.numeric(gsub("^(\\d{4})(.+$)", "\\1", date, perl=T))
                                  , date=as.name(T1_var))
      varVal2 <- lazyeval::interp(~as.numeric(gsub('\\d{4}-(\\d{2})-\\d+', "\\1", date, perl=T))
                                  , date=as.name(T1_var))
      varVal3 <- lazyeval::interp(~as.Date(date, "%m/%d/%Y")
                                  , date=as.name(T1_var))
      
      df <- read.xlsx(file=paste0(inPath, inFile)
                      , sheetIndex=1
                      , header = T
      ) %>% #[1] 3182   13 
      {
            dtLastStep <- .
            names(dtLastStep) <- tolower(names(dtLastStep))
            dtLastStep[is.na(dtLastStep)] <- 0
            dtLastStep
      } %>%
            mutate_(.dots=setNames(list(varVal1), 'year')) %>%
            mutate_(.dots=setNames(list(varVal2), 'month')) %>%
            mutate_(.dots=setNames(list(varVal3), 'date')) 
            

      return(df)
}

model_data_prepare2 <- function(df, bStd, nrx_var, rt_test, control_df
                                , salesVar4revenue=salesVar4revenue
                                , bDropSmallSeg){
      
      adj_var <- paste0(promo_var, "_adj")
      stk_var <- paste0(adj_var, "_stk")
      rt_var <- paste0(stk_var, '_rt')
      
      
      IDs <- length (unique(df[, IDs_var]))                   # Number of Nanobricks
      T1 <- length(unique(df[, T1_var]))
      records2rm <- which(df$date<as.Date(min_date) | df$date>as.Date(max_date))
      
      if(bDropSmallSeg){
            records2rm2 <- which(df$final_segment %in% segs2drop)
            records2rm <- c(records2rm, records2rm2)
      }
      ctrl_var <- names(control_df)
      cat('1')
      
      df_final <- lapply(salesVars2adj, function(v)runAdj(v, df, nrx_var_size_adj)) %>%
            # data.frame(do.call(cbind.data.frame, .))
            do.call(cbind, .) %>%
            as.data.frame() %>%
            tbl_df() %>%
            {
                  dtLastStep <- .
                  temp2 <- df %>% dplyr::select(-one_of(salesVars2adj))
                  temp3 <- cbind(temp2, dtLastStep)
                  names(temp3) <- c(setdiff(names(df), salesVars2adj), paste0(salesVars2adj, '_adj'))
                  temp3
            } %>%
#             bind_cols(control_df)  %>%
            {
                  # size adjust promo variables
                  
                  dtLastStep <- .
                  for( i in 1:length(c(promo_var))) {
                        v <- paste0(c(promo_var)[i], "_adj")
                        dtLastStep[, v] <- dtLastStep[, c(promo_var)[i] ] / dtLastStep[, c(promo_var_size_adj)[i]]
                  }
                  dtLastStep
            }
      cat('2')
      
      
      X_withOnlyAdj <- df_final %>% dplyr::select(one_of(adj_var))
      T_vct_all <- df_final[, T1_var]
      
      X_withStk <- X_withOnlyAdj %>%
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
#       X_withOnlyAdj <- X_withOnlyAdj[-records2rm, ]
      if(bStd){
            var2std <- c(rt_var, ctrl_var, paste0(nrx_var, '_adj'))
            mod_data <- runStd(var2std=var2std, data=mod_data)
            
            X4Bayes <- X4Bayes %>% StandardStock(.)
      }
      # [1] 1032   11
      cat('4')
      sales_mean <- df_final[-records2rm, ] %>% dplyr::select(one_of(paste0(salesVar4revenue, '_adj'))) %>%
            sapply(., mean)
      
      temp_result <- list(mod_data4BaseLine=mod_data
                          , X4Bayes=X4Bayes
                          , X_withOnlyAdj=X_withOnlyAdj
                          , sales_mean=sales_mean
                          , records2rm=records2rm
                          , T_vct_all=T_vct_all
      )
}



run_baseLine <- function(model_data, nrx_var
                         # , promo_var_inBl, ctrl_var_inBl
                         , promo_var_inBl_fixed, ctrl_var_inBl_fixed
                         , promo_var_inBl_rnd, ctrl_var_inBl_rnd){
      #       stk_var_inModel <- promo_var
      nrx_adj <- paste0(nrx_var, '_adj')
#       f_eval(~f_interp(~lm(uqf(formula), data=model_data)))
      # var_inModel = paste0(promo_var_inBl, '_adj_stk_rt')
      
      promo_var_inBl_fixed <- paste0(promo_var_inBl_fixed, '_adj_stk_rt')
      if(promo_var_inBl_rnd == ""){
            promo_var_inBl_rnd=NULL
      }else{
            promo_var_inBl_rnd <- paste0(promo_var_inBl_rnd, '_adj_stk_rt')
            
      }
#       promo_var_inBl_rnd <- setdiff(paste0(promo_var_inBl, '_adj_stk_rt'), promo_var_inBl_fixed)
#       ctrl_var_inBl_rnd <- setdiff(ctrl_var_inBl, ctrl_var_inBl_fixed)
      
      var_fixed <- c(promo_var_inBl_fixed, ctrl_var_inBl_fixed)
      var_rnd <- c(promo_var_inBl_rnd, ctrl_var_inBl_rnd)
      
      
      formula <- paste0(paste0(nrx_adj, ' ~ ')
                        , paste0(var_fixed, collapse = '+')
                        , '+'
                        , '(1 +'
                        , paste0(var_rnd, collapse = "+")
                        , '|'
                        , IDs_var
                        , ')'
      )
      
      lmm <- lmer(formula=as.formula(formula), data = model_data)
      summary(lmm)$coefficients
      Anova(lmm)
      coef(lmm)
      
      #Coefficients
      coef <- data.frame (coef(lmm)[[1]])
      names(coef) <- paste0("coef_", names(coef))
      coef$final_segment<-rownames(coef)
      model_data$final_segment <- as.character(model_data$final_segment)
      model_df <- left_join (model_data, coef, by= c("final_segment"="final_segment"))
      model_df$sum_fitted <- predict(lmm)
      
      # find R-squared & MAPE
      Rsquare <- 1 - sum( (model_df$sum_fitted - model_df[, nrx_adj])^2 ) / sum( (model_df[, nrx_adj]-mean(model_df[, nrx_adj]))^2 )
      
      getMSE <- function(vct){
            mse <-vct^2
            return(mse)
      }
      varVal1 <- lazyeval::interp(~(x-sum_fitted)^2, x=as.name(nrx_adj))
      varVal2 <- lazyeval::interp(~abs((x-sum_fitted)/x), x=as.name(nrx_adj))
      
      mse_mape_bySeg <- model_df %>% dplyr::select(one_of(c("final_segment", "sum_fitted", nrx_adj))) %>%
            mutate_(.dots=setNames(list(varVal1), 'mse')) %>%
            mutate_(.dots=setNames(list(varVal2), 'mape')) %>%
            mutate(mape=ifelse(mape==Inf, NA, mape)) %>%
            group_by(final_segment) %>%
            # summarise_each(funs(getMSE(.)), sum_fitted)
            # summarise_each(funs(getMSE))
            dplyr::summarise(mse=sum(mse), mape=mean(mape, na.rm=T), cnt=n()) %>%
            mutate(mse=mse/cnt) %>%
            dplyr::select(-cnt) %>%
            dplyr::arrange(desc(mse))
      
      mape <- mean (mse_mape_bySeg$mape, na.rm=T)
      
      
      result_temp <- list(model_df=model_df, coefs_fixed=summary(lmm)$coefficients, mape=mape, Rsquare=Rsquare, mse_mape_bySeg=mse_mape_bySeg)
      return(result_temp)
}


myloop <- function(X, Rname ,Rmu,Rprec,RM, IDs, resultDir, T_mod
                   , y1, d1, d2, p, atts1, atts2,dimbeta, iters, traceFile
                   , b4RtLoop) {
      name = Rname              # Name of Output File
      mu=Rmu
      prec=Rprec
      M=RM
      
      apply(X[,1:dimbeta], 2, mean) # Check Standardization again
      data4Bugs <- list(y=y1,X=X,d1=d1,d2=d2,atts1=atts1,atts2=atts2
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
      cat(file = traceFile, append = T, '7\n')
      save(data4Bugs, inits, parameters, iters, atts1, atts2, T_mod, file = paste0(resultDir, 'debug.RData'))
      out<-PromoMix(data=data4Bugs,inits=inits,parameters=parameters
                    ,n.chains=3,n.iter=iters,debug=F,atts1=atts1
                    ,atts2=atts2,T=T_mod,name=name
                    , resultDir=resultDir, b4RtLoop=b4RtLoop)
      cat(file = traceFile, append = T, '8\n')
      
      return(out)
}

run_bayes <- function(X4Bayes, model_data4BaseLine, prod, IDs_var, ctrl_var, promo_var
                      , iters, p, d1, d2, nrx_var, mu1, prec1, M1, bStd, resultDir
                      , traceFile, b4RtLoop){
      traceFile <- paste0(resultDir, traceFile, '.csv')
      nrx_adj <- paste0(nrx_var, '_adj')
      var_inModel <- paste0(promo_var, '_adj_stk')
      ctrl <- model_data4BaseLine[, ctrl_var]
#       if(bStd){
#             ctrl <- runStd(ctrl_var, ctrl)
#       }
      X <- as.matrix(cbind(X4Bayes, ctrl))
      atts1 <- length(var_inModel)
      atts2 <- length(ctrl_var)
      dimbeta <- atts1 + atts2
      T_mod <- length(unique(model_data4BaseLine$date))
      y1 <- model_data4BaseLine[, nrx_adj]
      if(bStd){
            y1 <- y1/mean(y1)
      }
      IDs <- length(unique(model_data4BaseLine[, IDs_var]))
      
      
      #Model 02
      
      a1R<-myloop(X=X, Rname=prod ,Rmu=mu1,Rprec=prec1
                  ,RM=M1, IDs=IDs, resultDir=resultDir
                  , y1=y1, d1=d1, d2=d2, atts1=atts1, p=p
                  , atts2=atts2,dimbeta=dimbeta, iters=iters
                  , T_mod=T_mod, traceFile = traceFile
                  , b4RtLoop=b4RtLoop
                  )
      Mbeta <- cbind(colnames(X), a1R$Mbeta)
      Roots <- a1R$Roots
      temp_result <- list(Mbeta=Mbeta, Roots=Roots)
      return(temp_result)
}


run_roi <- function(inPath, outPath, promo_var, prod, dt_name, price_vct
                    , unitCosts_vct, ctrl_var, IDs, rt_test, model_data_list
                    , otherVars_inModel){
      sales_mean <- model_data_list$sales_mean
      records2rm <- model_data_list$records2rm
      
      vars4rt <- paste0(promo_var, '_adj_stk')
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
            model_data_list$X_withOnlyAdj[-records2rm, gsub("(^.+_adj).+$", '\\1', v, perl=T)]*
            unitCosts_df[, paste0('uniCost_', v)]) %>%
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
                        bind_cols(dtLastStep) %>%
                        {
                              .[is.na(.)] <- 0
                              .
                        }
                        
                  roi_df
            }
      
      write.csv(model_data_beta_agg, paste0(outPath, 'for_roi_qc.csv'), row.names = F)
      return(model_data_beta_agg)
}

run_each_retention <- function(i, MyRetention
                               , promo_var
                               , model_data_list
                               , nrx_var
                               , T1_var
                               , IDs_var
                               , ctrl_var
                               , iters
                               , p, d2, mu1, prec1
                               , d1, M1
                               , bStd, resultDir
                               , traceFile
                               , b4RtLoop){
      
      cat(file = traceFile, append = T, '1\n')
      
      atts1 <- length(promo_var)
      Retain <- MyRetention[i, 1:atts1]
      X_withOnlyAdj <- model_data_list$X_withOnlyAdj
      T_vct_all <- model_data_list$T_vct_all
      cat(file = traceFile, append = T, '2\n')
      
      nrx_var_adj <- paste0(nrx_var, '_adj')
      y1 <- model_data_list$mod_data4BaseLine[, nrx_var_adj]
      atts2 <- length(ctrl_var)
      cat(file = traceFile, append = T, '3\n')
      
      T_all <- length(unique(T_vct_all))
      IDs <- length(unique(model_data_list$mod_data4BaseLine[, IDs_var]))
      cat(file = traceFile, append = T, '4\n')
      
      X_withStk_withCtrl <- lapply(1:IDs, function(i)run_stk1(j=i, X1=X_withOnlyAdj, T=T_all, atts1=atts1, Retain=Retain)) %>% 
            do.call(rbind, .) %>% 
            as.data.frame() %>% 
            setNames(paste0(names(X_withOnlyAdj), '_stk')) %>%
            .[-model_data_list$records2rm, ] %>%
            {
                  if(bStd) {
                        X_withStd <- StandardStock(.)
                        X_withStd
                  }else{
                        .
                  }
            } %>%
            bind_cols(model_data_list$mod_data4BaseLine[, ctrl_var])
      cat(file = traceFile, append = T, '5\n')
      
      dimbeta <- ncol(X_withStk_withCtrl)
      out <- myloop(X=as.matrix(X_withStk_withCtrl), Rname=prod ,Rmu=mu1,Rprec=prec1,RM=M1, IDs=IDs, resultDir=resultDir
                    , y1=y1, d1=d1, d2=d2, p=p, atts1=atts1, atts2=atts2,dimbeta=dimbeta, iters=iters
                    , T_mod = length(unique(model_data_list$mod_data4BaseLine[, T1_var])), traceFile=traceFile
                    , b4RtLoop=b4RtLoop)
      cat(file = traceFile, append = T, '6\n')
      
      return(out)
      
      
}


run_retention_loop <- function(inPath, path_fun, file, model_data_list, n.cpu
                               , promo_var, ctrl_var, nrx_var
                               , iters, p, d1, d2, mu1, prec1, M1
                               , T1_var, IDs_var, bStd, bTest, resultDir, bPar
                               , traceFile, outFile, b4RtLoop){
      MyRetention <- read.table(paste0(inPath, file, '.csv'), header=T, sep=',')
      if(bTest)MyRetention <- MyRetention[1:2, ]
      traceFile <- paste0(resultDir, traceFile, '.csv')
      if(bPar){
            sfInit(parallel=TRUE, cpus=n.cpu, type='SOCK')
            sfSource(paste0(path_fun, "PromoMix Functions v3.txt"))
            sfSource(paste0(path_fun, "function.R"))
            
            sfExport('MyRetention', 'promo_var', 'model_data_list', 'nrx_var', 'T1_var', 'IDs_var'
                     , 'ctrl_var', 'T1_var', 'iters', 'p', 'd1', 'd2', 'mu1', 'prec1', 'M1', 'bStd'
                     , 'resultDir', 'traceFile', 'b4RtLoop')
            sfClusterEval(library("lattice"))
            sfClusterEval(library("coda"))
            sfClusterEval(library("plyr"))
            sfClusterEval(library("dplyr"))
            sfClusterEval(library("arm"))
            sfClusterEval(library("R2WinBUGS"))
            sfClusterEval(library("xlsx"))
            sfClusterEval(library("lme4"))
            sfClusterEval(library("car"))
            
            temp <- sfClusterApplyLB(1:nrow(MyRetention), run_each_retention
                                     , MyRetention=MyRetention
                                     , promo_var=promo_var
                                     , model_data_list=model_data_list
                                     , nrx_var=nrx_var
                                     , T1_var=T1_var
                                     , IDs_var=IDs_var
                                     , ctrl_var=ctrl_var
                                     , iters=iters
                                     , p=p, d2=d2, mu1=mu1, prec1=prec1
                                     , d1=d1, M1=M1
                                     , bStd=bStd, resultDir=resultDir
                                     , traceFile=traceFile
                                     , b4RtLoop=b4RtLoop
                                     )
            sfStop()
      }else{
            temp <- lapply(1:nrow(MyRetention), function(j)run_each_retention(i=j, MyRetention=MyRetention
                                                                   , promo_var=promo_var
                                                                   , model_data_list=model_data_list
                                                                   , nrx_var=nrx_var
                                                                   , T1_var=T1_var
                                                                   , IDs_var=IDs_var
                                                                   , ctrl_var=ctrl_var
                                                                   , iters=iters
                                                                   , p=p, d2=d2, mu1=mu1, prec1=prec1
                                                                   , d1=d1, M1=M1
                                                                   , bStd=bStd, resultDir=resultDir
                                                                   , traceFile=traceFile
                                                                   , b4RtLoop=b4RtLoop)
                           )
      }
      
      Deviance <- unlist(lapply(temp, function(x)x$Deviance))
      pD <- unlist(lapply(temp, function(x)x$pD))
      DIC <- unlist(lapply(temp, function(x)x$DIC))
      a <- unlist(lapply(temp, function(x)x$a))
      Roots <- lapply(temp, function(x)x$Roots) %>%
            do.call(rbind, .) %>%
            as.data.frame() %>%
            setNames(paste0(promo_var, '_root'))
      
      Mbeta <- lapply(temp, function(x)x$Mbeta) %>%
            do.call(rbind, .) %>%
            as.data.frame() %>%
            setNames(paste0(c(promo_var, ctrl_var), '_Mbeta'))
      
      temp_df <- MyRetention %>% setNames(paste0(promo_var, '_rtRate')) %>%
            bind_cols(data.frame(Deviance, pD, DIC, a)) %>% 
            bind_cols(temp1 <- Roots %>% bind_cols(Mbeta))
      
      write.csv(temp_df, paste0(resultDir, outFile, '.csv'), row.names = F)
      return(temp_df)
      
}
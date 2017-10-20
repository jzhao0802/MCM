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



library(brms)
model_data_list=readRDS(file = paste0('C:/work/working materials/MCM/R part/Code/Results/2017-10-17 07.22.03/', 'model_data_list.RDS'))
nrx_var <- c('prescriptions')

IDs_vct <- model_data_list$mod_data4BaseLine$final_segment
X4Bayes=model_data_list$X4Bayes
model_data4BaseLine=model_data_list$mod_data4BaseLine
ctrl_var_inBys <- c('log_trend', 'pos', 'neg', 'pos_77')
# final_segment <- IDs_var
ctrl_var=ctrl_var_inBys
rnd_vars<- ctrl_var
# rnd_vars = ""
ctrl <- model_data4BaseLine[, ctrl_var]

#       if(bStd){
#             ctrl <- runStd(ctrl_var, ctrl)
#       }
X <- as.matrix(data.frame(X4Bayes, ctrl, final_segment=IDs_vct))
T_mod <- length(unique(model_data4BaseLine$date))
nrx_adj <- paste0(nrx_var, '_adj')

y1 <- model_data4BaseLine[, nrx_adj]
if(bStd){
      y1 <- y1/mean(y1)
}
IDs <- length(unique(model_data4BaseLine[, IDs_var]))

data4brms <- cbind(y1, X)

fix_vars <- paste0(promo_var, '_adj_stk')
formula4brms <- as.formula(paste0('y1~'
                  , paste0(fix_vars, collapse="+")
                  , '+(1+'
                  # , '+(1'
                  , paste0(rnd_vars, collapse = "+")
                  , "|"
                  , IDs_var
                  , ")"
                  )
                  )
brmsformula(formula4brms)
brm_fit <- brm(formula = 
                     # y1 ~ call_adj_stk+ (1+call_adj_stk | final_segment)
               brmsformula(formula4brms)
               , data = data4brms
               # , family = lognormal()
               , prior =  c(set_prior("normal(0.01, 0.5)", coef="call_adj_stk")
                            , set_prior("normal(0.04, 0.5)", coef="meeting_epu_adj_stk")
                            , set_prior("normal(0.012, 0.5)", coef="meeting_national_adj_stk")
                            , set_prior("normal(0.023, 0.5)", coef="meeting_international_adj_stk")
                            , set_prior("normal(0.001, 0.5)", coef="meeting_other_adj_stk")
               )
               # , prior = prior_list
               , iter = 30
               , chains = 3
)

brm(brm_fit)
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


Sys.getenv('PATH')
system('g++ -v')

system('where make')

brmsformula()
# prod <- "prolia"
# min_date <- "2015/01/01"
# max_date <- "2015/12/01"
# IDs_var <- 'final_segment'
# T1_var <- 'date'                       # Number of total Time Periods
# random = ""
# 
# salesVars2adj <- c('prescriptions', 'units_sales', 'eur_sales')
# salesVar4revenue <- 'eur_sales'
# promo_var <- c("call", "meeting_epu", "meeting_national", "meeting_international", "meeting_other")
# price_vct <- c(238, 238, 238, 238, 238) #nrx price
# unitCosts_vct <- c(121.4, 1,1,1,1)
# promo_var_size_adj <- rep(c("cohort_count_fromseg"), length(promo_var))
# 
# # set up nrx variable and size adjustment
# nrx_var <- c('prescriptions')
# nrx_var_size_adj <-c('cohort_count_fromsales')
# 
# bStd <- T
# 
# firstmon <- 3
# Retain <- c(0.8, 0.8, 0.8, 0.8, 0.8) # Change
# 
# otherVars_inModel <- c('prescriptions_adj', 'final_segment', 'date')
# 
# n.cpu <- 4



getMu <- function(distri, var, idx1, idx2){
      if(is.character(idx1)){
            return(paste0(distri, "(", mu_prec_priors[match(var, mu_prec_priors$varNm), idx1], ","
                          , mu_prec_priors[match(var, mu_prec_priors$varNm), idx2]
                          , ')'))
            
      }else{
            return(paste0(distri, "(", idx1, ","
                          , mu_prec_priors[match(var, mu_prec_priors$varNm), idx2]
                          , ')'))
            
      }
      
}

library(brms)
nrx_var <- c('prescriptions')
bStd <- T
promo_var <- c("call", "meeting_epu", "meeting_national", "meeting_international", "meeting_other")

model_data_list=readRDS(file = paste0('C:/work/working materials/MCM/R part/Code/Results/2017-10-17 07.22.03/', 'model_data_list.RDS'))

IDs_vct <- model_data_list$mod_data4BaseLine$final_segment
# X4Bayes=model_data_list$X4Bayes
# use data for baseline to do the stan model(the data adding root)
prefix <- "_adj_stk_rt"
X4Bayes <- model_data_list$mod_data4BaseLine[, paste0(promo_var, prefix)]
model_data4BaseLine=model_data_list$mod_data4BaseLine
ctrl_var_inBys <- c('log_trend', 'pos', 'neg', 'pos_77')
# final_segment <- IDs_var
IDs_var <- 'final_segment'

ctrl_var=ctrl_var_inBys
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

fix_vars <- c(paste0(promo_var, prefix)
              ,  ctrl_var
)
rnd_vars <- c(paste0(promo_var, prefix), ctrl_var)

# do some adjugment -- drop the low level segments (14)
meanByCohort <- data4brms %>%
      as.data.frame() %>%
      dplyr::select(one_of(c(grep("_rt$", names(model_data4BaseLine), value=T), 'final_segment', 'y1'))) %>%
      group_by(final_segment) %>%
      dplyr::summarise_all(funs(mean)) %>%
      arrange(y1)


#       dplyr::select(-final_segment) %>%
#       apply(., 1, mean)

# delete those segments with low level sales
segs_low <- meanByCohort %>%
      filter(y1<0.1) %>%
      dplyr::select(final_segment) %>%
      as.data.frame()

data4brms_dropLowSegs <- data4brms[!data4brms[, 'final_segment'] %in% segs_low$final_segment,]
write.csv(data4brms_dropLowSegs
          , file =  paste0("C:\\work\\working materials\\MCM\\R part\\Code\\Results\\2017-10-26 19.34.23/data4brms_dropLowSegs.csv")
          , row.names=F
          )      

library(dplyr)
mu_prec_priors <- data.frame(
      varNm=c(paste0(promo_var, prefix), ctrl_var)
      , mu=c(0.040105695,	0.005538787,	0.012136681,	0.023471172,	0.001431081, 0,0,0,0)
      , prec=c(1061.49395,	125222.6303,	26080.28162,	6973.368051,	1875787.238, 0.9604, 0.9604, 0.9604, 0.9604)
      
) %>%
      mutate(vars=1/prec) %>%
      mutate(stdev=vars^0.5) %>%
      mutate(varNm=as.character(varNm))

# %>% 
#       t(.) %>%
#       setNames(., c(promo_var, ctrl_var))
mu_prec_priors$gamma4Cauchy <- mu_prec_priors$mu/100



# getMu('normal', 'call_adj_stk', 'mu', 'stdev')
# names(mu_prec_priors)
formula4brms <- as.formula(paste0('y1~'
                                  , paste0(fix_vars, collapse="+")
                                  , '+(1+'
                                  , paste0(rnd_vars, collapse = "+")
                                  , "|"
                                  , IDs_var
                                  , ")"
)

)
brmsformula(formula4brms)

t0 <- Sys.time()
brm_fit <- brm(formula = 
                     brmsformula(formula4brms)
               , data = data4brms
               # , family = lognormal()
               , prior =  c(set_prior('normal(0, 1)', class = 'Intercept')
                            , set_prior("cauchy(0, 2.5)", coef = 'Intercept', class='sd', group = IDs_var)
                            
                            , set_prior(getMu('normal', paste0("call", prefix), 'mu', 'stdev'), coef=paste0("call", prefix), class = 'b')
                            , set_prior(getMu('normal', paste0("meeting_epu", prefix), 'mu', 'stdev'), coef=paste0("meeting_epu", prefix), class = 'b')
                            , set_prior(getMu('normal', paste0("meeting_national", prefix), 'mu', 'stdev'), coef=paste0("meeting_national", prefix), class = 'b')
                            , set_prior(getMu('normal', paste0("meeting_international", prefix), 'mu', 'stdev'), coef=paste0("meeting_international", prefix), class = 'b')
                            , set_prior(getMu('normal', paste0("meeting_other", prefix), 'mu', 'stdev'), coef=paste0("meeting_other", prefix), class = 'b')
                            
                            , set_prior(getMu('cauchy', paste0("call", prefix), 'stdev'
                                              , "gamma4Cauchy"), coef=paste0("call", prefix), class='sd', group = IDs_var)
                            , set_prior(getMu('cauchy', paste0("meeting_epu", prefix),  'stdev'
                                              , "gamma4Cauchy"), coef=paste0("meeting_epu", prefix), class='sd', group = IDs_var)
                            , set_prior(getMu('cauchy', paste0("meeting_national", prefix),  'stdev'
                                              , "gamma4Cauchy"), coef=paste0("meeting_national", prefix), class='sd', group = IDs_var)
                            , set_prior(getMu('cauchy', paste0("meeting_international", prefix),  'stdev'
                                              , "gamma4Cauchy"), coef=paste0("meeting_international", prefix), class='sd', group = IDs_var)
                            , set_prior(getMu('cauchy', paste0("meeting_other", prefix),  'stdev'
                                              , "gamma4Cauchy"), coef=paste0("meeting_other", prefix), class='sd', group = IDs_var)
                            
               )
               # , prior = prior_list
               , iter = 5000
               , chains = 3
               , control = list(adapt_delta = 0.96)
)
summary_fit <- summary(brm_fit)


timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

fix <- fixef(brm_fit, old = T)
write.csv(fix,paste0(resultDir, "fixed_from_brm.csv"))
rnd <- ranef(brm_fit, old = T)
write.csv(rnd, paste0(resultDir, "rnd_from_brm.csv"))
saveRDS(summary_fit, file=paste0(resultDir, 'summary_brmFit.RDS'))

timeUsed <- (Sys.time()-t0)
timeUsed

# pdf(filename=paste0(resultDir, 'plot4convergeCheck.pdf'))
# plot(brm_fit)
# dev.off()



Sys.getenv('PATH')
system('g++ -v')

system('where make')

brmsformula()
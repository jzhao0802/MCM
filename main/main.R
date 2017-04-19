

rm(list=ls(all=TRUE))

library(plyr)
library(dplyr)
library(reshape2)
library(lattice)
library(coda)
library("arm")
library("R2WinBUGS")
library(sas7bdat)
library(xlsx)
require(lme4)


data_path <- "../Data/for_test/"
data_path_1 <- data_path
data_path_2 <- "../Data/FR/"
path_fun <- "./func/"
source(paste0(path_fun, "PromoMix Functions v3.txt"))


timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
# setwd(resultDir)
setwd("C:\\work\\working materials\\MCM\\R part\\Code\\")


prod <- "prolia"
IDs <- length (unique(df_final$final_segment))                   # Number of Nanobricks
T1 <- 37                       # Number of total Time Periods
random = ""

salesVars2adj <- c('prescriptions', 'units_sales', 'eur_sales')

promo_var <- c("call", "meeting_epu", "meeting_national", "meeting_international", "meeting_other")
# promo_var_size_adj <- c("hcp_innano", "hcp_innano", "hcp_innano")
promo_var_size_adj <- rep(c("cohort_count_fromseg"), length(promo_var))

# set up nrx variable and size adjustment
nrx_var <- c('prescriptions')
nrx_var_size_adj <-c('cohort_count_fromsales')

firstmon <- 3
Retain <- c(0.8, 0.3, 0.3, 0.3, 0.3)  # Change
adj_var <- paste0(promo_var, "_adj")
stk_var <- paste0(adj_var, "_stk")
rt_var <- paste0(stk_var, '_rt')

ctrl_var_inModel <- c('event1', 'event2')
otherVars_inModel <- c('prescriptions', 'prescriptions_adj', 'final_segment', 'date')

rt_test <- rep(0.5, length(stk_var))


model_data_prepare <- function(){
      
}
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
      

records2rm <- which(df$year!='2015')
control_df <- data.frame(event1=ifelse(df$month %in% c('02'), 1, 0)
                         , event2=ifelse(df$month %in% c('08'), 1, 0))

df_final <- lapply(salesVars2adj, function(v){
      vct <- df[, v]
      vct_adj <- vct/df[, nrx_var_size_adj]*df[, promo_var_size_adj[1]]
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

X_withStk <- df_final %>% dplyr::select(one_of(adj_var)) %>%
{
      dtLastStep <- .
      temp <- lapply(1:IDs, function(i){
            row_idx <- (1+T1*(i-1)):(T1+T1*(i-1))  # row index for current IDs
            Start <- colMeans( dtLastStep[row_idx[1:firstmon], adj_var] )
            temp <- Stock(dtLastStep[row_idx, adj_var], Start, Retain)## Stock and standardize Variables (Note set retention rates)
            return(temp)
      }) %>%
            do.call(rbind, .) %>%
            as.data.frame()
      temp
} %>%
      setNames(stk_var)

X_withRt <- X_withStk %>% {
      dtLastStep <- .
      temp <- lapply(1:length(stk_var), function(i)dtLastStep[, i] ^ rt_test[i]) %>%
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

# [1] 1032   11





     
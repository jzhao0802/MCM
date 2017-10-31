# clean up the results from winbugss

beta <- read.csv(file = "C:\\work\\working materials\\MCM\\R part\\Code\\Results\\2017-10-27 16.49.15\\prolia_Means_1.csv")

names(beta) <- tolower(names(beta))

bBeta <- grepl("^beta\\[", as.character(beta$x), ignore.case = T)

beta_using <- beta[bBeta, ]

n.seg <- 86
n.coef <- 8

temp_list <- lapply(1:n.seg, function(s){
      coef <- beta_using$mean
      id_coefThisSeg <- (n.coef*(s-1)+1):(n.coef*(s-1)+n.coef)
      coefThisSeg <- coef[id_coefThisSeg]
      return(coefThisSeg)
})

ctrl_var_inBys <- c('log_trend', 'pos', 'neg')

coef_df <- do.call(rbind.data.frame, temp_list)

names(coef_df) <- c(paste0(promo_var, prefix), ctrl_var_inBys)

# fixed
bBetam <- grepl("^betam\\[", as.character(beta$x), ignore.case = T)
betam_using <- beta[bBetam, ]
coef_agg <- betam_using$mean

# rbind
coef_df_fix_rnd <- rbind(coef_agg, coef_df)
coef_df_fix_rnd$final_segment <- c("Aggregated", 1:n.seg) 
coef_df_fix_rnd <- select(coef_df_fix_rnd, one_of(c('final_segment', names(coef_df))))

timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")


write.csv(coef_df_fix_rnd, file=paste0(resultDir, 'coef_fixAndRnd_winbugs.csv'), row.names = F)

# load libraries
library(foreign)

# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

dpq_list <- list()

# remove all those with no PHQ data at all
# pool DPQ data
for (j in 1:15) {
  filename <- paste0("imputed_",j,".rds")
  # load imputed datasets
  df_imputed <- readRDS(filename)
  df <- df_imputed[df_imputed$phq == "Yes",]
  df$phq <- NULL
  dpq_list[[j]] <- df[,21:29]
}

dpq_pooled <- Reduce("+", dpq_list) / length(dpq_list)
dpq_rounded <- round(dpq_pooled,0)

for (j in 1:15) {
  filename <- paste0("imputed_",j,".rds")
  # load imputed datasets
  df_imputed <- readRDS(filename)
  df <- df_imputed[df_imputed$phq == "Yes",]
  df$phq <- NULL
  df[,21:29] <- dpq_rounded
  df$DPQ_total <- df$DPQ010 + df$DPQ020 + df$DPQ030 + 
    df$DPQ040 + df$DPQ050 + df$DPQ060 + 
    df$DPQ070 + df$DPQ080 + df$DPQ090
  df$depressed <- 0
  df$depressed[df$DPQ_total >= 10] <- 1
  table(df$depressed)
  df_depressed <- df[df$depressed == 1,]
  filename <- paste0("clean_",j,".rds")
  saveRDS(df_depressed, filename)
  stata_filename <- paste0("clean_",j,".dta")
  write.dta(df_depressed, stata_filename)
}


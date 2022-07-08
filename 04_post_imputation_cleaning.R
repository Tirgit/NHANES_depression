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
  filename <- paste0("clean_",j,".rds")
  saveRDS(df, filename)
}


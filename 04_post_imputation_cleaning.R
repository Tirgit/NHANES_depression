# load libraries
library(foreign)
library(dplyr)

# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

dpq_list <- list()
dpq_vars <- c("DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050",
              "DPQ060", "DPQ070", "DPQ080", "DPQ090")

# remove all those with no PHQ data at all
# pool DPQ data
for (j in 1:15) {
  filename <- paste0("imputed_",j,".rds")
  # load imputed datasets
  df_imputed <- readRDS(filename)
  df <- df_imputed[df_imputed$phq == "Yes",]
  df$phq <- NULL
  df$DPQ010 <- as.numeric(df$DPQ010) - 1
  df$DPQ020 <- as.numeric(df$DPQ020) - 1
  df$DPQ030 <- as.numeric(df$DPQ030) - 1
  df$DPQ040 <- as.numeric(df$DPQ040) - 1
  df$DPQ050 <- as.numeric(df$DPQ050) - 1
  df$DPQ060 <- as.numeric(df$DPQ060) - 1
  df$DPQ070 <- as.numeric(df$DPQ070) - 1
  df$DPQ080 <- as.numeric(df$DPQ080) - 1
  df$DPQ090 <- as.numeric(df$DPQ090) - 1
  dpq_list[[j]] <- df[,dpq_vars]
}

# find models of each corresponding value across the 15 imputed datasets
# for the 9 DPQ questions

# define mode function
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# create vectors of the data frames
my_list_unlisted <- lapply(dpq_list, unlist)

# find models across the 9 vectors
modes <- sapply(seq_along(my_list_unlisted[[1]]), function(i) {
  get_mode(sapply(my_list_unlisted, `[`, i))
})

# regain the structure of the original data frame. keep numeric for now
df_restored <- matrix(modes, nrow = nrow(dpq_list[[1]]), byrow = FALSE)
dpq_rounded <- as.data.frame(df_restored)
colnames(dpq_rounded) <- dpq_vars


for (j in 1:15) {
  filename <- paste0("imputed_",j,".rds")
  # load imputed datasets
  df_imputed <- readRDS(filename)
  df_excluded <- df_imputed[df_imputed$phq == "No",]
  df_excluded$phq <- NULL
  df_excluded$DPQ_total <- NA
  df_excluded$depressed <- NA
  df <- df_imputed[df_imputed$phq == "Yes",]
  df$phq <- NULL
  df[,dpq_vars] <- dpq_rounded
  df$DPQ_total <- df$DPQ010 + df$DPQ020 + df$DPQ030 + 
    df$DPQ040 + df$DPQ050 + df$DPQ060 + 
    df$DPQ070 + df$DPQ080 + df$DPQ090
  df$depressed <- 0
  df$depressed[df$DPQ_total >= 10] <- 1
  table(df$depressed)
  df[,dpq_vars] <- mutate_all(df[,dpq_vars], as.ordered)
  df_merged <- rbind(df, df_excluded)
  table(df_merged$depressed, useNA= "always")
  filename <- paste0("clean_",j,".rds")
  saveRDS(df_merged, filename)
  stata_filename <- paste0("clean_",j,".dta")
  write.dta(df_merged, stata_filename)
}


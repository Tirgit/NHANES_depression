# Depression Severity: 0-4 none, 5-9 mild, 10-14 moderate, 15-19 moderately severe, 20-27 severe.

# https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995

# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

# define DPQ vars
dpq_vars <- c("DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050",
              "DPQ060", "DPQ070", "DPQ080", "DPQ090")

# load imputed data
df <- readRDS("clean_1.rds")
df <- df[!is.na(df$DPQ_total),]
table(df$depressed, useNA = "always")

df_non <- df[df$DPQ_total <= 4,]
df_mild <- df[df$DPQ_total >= 5 & df$DPQ_total <= 9,]
df_mod <- df[df$DPQ_total >= 10 & df$DPQ_total <= 14,]
df_modsev <- df[df$DPQ_total >= 15 & df$DPQ_total <= 19,]
df_sev <-  df[df$DPQ_total >= 20,]

df_depressed <- df
# df_depressed <- df[df$DPQ_total >= 10,]




# test for tau equivalence
phq_model <- 'f =~ DPQ010 + DPQ020 + DPQ030 + DPQ040 + DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090'
phq_fit <- cfa(phq_model, data = df_depressed)
summary(phq_fit, standardized = TRUE, fit.measures = TRUE)


inspect(phq_fit, what="std")$lambda


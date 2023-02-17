# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

# libraries
library(dplyr)
library(tidyLPA)
library(VarSelLCM)
library(ggplot2)
library(dendextend)

# Depression questions

# DPQ010 - Have little interest in doing things
# DPQ020 - Feeling down, depressed, or hopeless
# DPQ030 - Trouble sleeping or sleeping too much
# DPQ040 - Feeling tired or having little energy
# DPQ050 - Poor appetite or overeating
# DPQ060 - Feeling bad about yourself
# DPQ070 - Trouble concentrating on things
# DPQ080 - Moving or speaking slowly or too fast
# DPQ090 - Thought you would be better off dead

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

# df_depressed <- df
df_depressed <- df[df$DPQ_total >= 10,]



res <- VarSelCluster(df_depressed[,dpq_vars], gvals = 6,
                             vbleSelec = FALSE, crit.varsel = "BIC")
print(res)



summary(res)
table(fitted(res))
head(fitted(res, type="probability"),30)
plot(res)
plot(x=res, y="DPQ010")
plot(x=res, y="DPQ020")
plot(x=res, y="DPQ030")
plot(x=res, y="DPQ040")
plot(x=res, y="DPQ050")
plot(x=res, y="DPQ060")
plot(x=res, y="DPQ070")
plot(x=res, y="DPQ080")
plot(x=res, y="DPQ090")



DPQ1_p <- t(coeffs@paramCategorical@alpha$DPQ010)
DPQ2_p <- t(coeffs@paramCategorical@alpha$DPQ020)
DPQ3_p <- t(coeffs@paramCategorical@alpha$DPQ030)
DPQ4_p <- t(coeffs@paramCategorical@alpha$DPQ040)
DPQ5_p <- t(coeffs@paramCategorical@alpha$DPQ050)
DPQ6_p <- t(coeffs@paramCategorical@alpha$DPQ060)
DPQ7_p <- t(coeffs@paramCategorical@alpha$DPQ070)
DPQ8_p <- t(coeffs@paramCategorical@alpha$DPQ080)
DPQ9_p <- t(coeffs@paramCategorical@alpha$DPQ090)

DPQ_p <- as.data.frame(rbind(DPQ1_p,DPQ2_p,DPQ3_p,DPQ4_p,DPQ5_p,DPQ6_p,DPQ7_p,DPQ8_p,DPQ9_p))
colnames(DPQ_p) <- c("Cluster 1", "Cluster 2","Cluster 3",
                     "Cluster 4","Cluster 5","Cluster 6",)
DPQ_p$Question <- c("Q1:0", "Q1:1", "Q1:2", "Q1:3",
                    "Q2:0", "Q2:1", "Q2:2", "Q2:3",
                    "Q3:0", "Q3:1", "Q3:2", "Q3:3",
                    "Q4:0", "Q4:1", "Q4:2", "Q4:3",
                    "Q5:0", "Q5:1", "Q5:2", "Q5:3",
                    "Q6:0", "Q6:1", "Q6:2", "Q6:3",
                    "Q7:0", "Q7:1", "Q7:2", "Q7:3",
                    "Q8:0", "Q8:1", "Q8:2", "Q8:3",
                    "Q9:0", "Q9:1", "Q9:2", "Q9:3")

# include overall frequency, and add results for chi squared test






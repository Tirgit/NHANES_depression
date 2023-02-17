# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

# libraries
library(dplyr)
library(tidyLPA)
library(VarSelLCM)

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




coef(res)



data(heart)
ztrue <- heart[,"Class"]
x <- heart[,-13]
# Add a missing value artificially (just to show that it works!)
x[1,1] <- NA

# Cluster analysis without variable selection
res_without <- VarSelCluster(x, gvals = 1:3, vbleSelec = FALSE, crit.varsel = "BIC")

# Cluster analysis with variable selection (with parallelisation)
res_with <- VarSelCluster(x, gvals = 1:3, nbcores = 4, crit.varsel = "BIC")
BIC(res_without)
BIC(res_with)
fitted(res_with)

summary(res_without)
head(fitted(res_without, type="probability"))

res_with <- VarSelCluster(x, gvals = 1, nbcores = 4, crit.varsel = "BIC")
print(res_with)

res_with <- VarSelCluster(x, gvals = 2, nbcores = 4, crit.varsel = "BIC")
print(res_with)


res_with <- VarSelCluster(x, gvals = 3, nbcores = 4, crit.varsel = "BIC")
print(res_with)



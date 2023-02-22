# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

# libraries
library(dplyr)
library(tidyLPA)
library(VarSelLCM)
library(ggplot2)
library(dendextend)
library(pheatmap)
library(lavaan)

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


# res <- VarSelCluster(df_depressed[,dpq_vars], gvals = 1:10,
#                              vbleSelec = FALSE, crit.varsel = "BIC")
# print(res)

# g = 6 is the cluster with the best performance
res <- VarSelCluster(df_depressed[,dpq_vars], gvals =6,
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

coeffs <- res@param
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
                     "Cluster 4","Cluster 5","Cluster 6")
rownames(DPQ_p) <- 1:nrow(DPQ_p)
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


sum(DPQ_p[1:4,6])



# extract cluster membership
cluster_membership <- fitted(res)
df_depressed <- cbind(df_depressed, cluster_membership)

# chi square test to test frequencies vs. expected based on global
# create a contingency table
tab <- table(df_depressed$DPQ090, df_depressed$cluster_membership)
tab

# calculate the expected frequency matrix
exp_freq <- chisq.test(tab)$expected
exp_freq

# create an empty matrix to store the p-values
p_values <- matrix(nrow = nrow(tab), ncol = ncol(tab))

# perform the Pearson's chi-squared test for each cell
for (i in 1:nrow(tab)) {
  for (j in 1:ncol(tab)) {
    obs_freq <- tab[i, j]
    exp_freq_val <- exp_freq[i, j]
    if (exp_freq_val >= 5) {
      p_values[i, j] <- chisq.test(matrix(c(obs_freq, exp_freq_val), nrow = 2))$p.value
    } else {
      p_values[i, j] <- chisq.test(matrix(c(obs_freq, exp_freq_val), nrow = 2), simulate.p.value = TRUE)$p.value
    }
  }
}

# print the matrix of p-values, Bonferroni corrected (6*4*9=216)
p_values*216






x <- table(df_depressed$DPQ_total, df_depressed$cluster_membership)
props <- prop.table(x, margin = 1) #margin definition 1: by row, 2: by column 
colnames(props) <- c("Cluster 1", "Cluster 2", "Cluster 3", 
                     "Cluster 4", "Cluster 5", "Cluster 6")

p <- pheatmap(props, cluster_rows = F, cluster_cols = F,
              legend = F, annotation_names_row = T,
              annotation_names_col = T, angle_col=45,
              display_numbers = T)

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_PHQ.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()


df_depressed$dep_category <- ""
df_depressed$dep_category[df_depressed$DPQ_total >= 10 & df_depressed$DPQ_total <= 14] <- "Moderate depression"
df_depressed$dep_category[df_depressed$DPQ_total >= 15 & df_depressed$DPQ_total <= 19] <- "Moderately severe depression"
df_depressed$dep_category[df_depressed$DPQ_total >= 20] <- "Severe depression"


x <- table(df_depressed$dep_category, df_depressed$cluster_membership)
props <- prop.table(x, margin = 1) #margin definition 1: by row, 2: by column 
colnames(props) <- c("Cluster 1", "Cluster 2", "Cluster 3", 
                     "Cluster 4", "Cluster 5", "Cluster 6")

p <- pheatmap(props, cluster_rows = F, cluster_cols = F,
              legend = F, annotation_names_row = T,
              annotation_names_col = T, angle_col=45,
              display_numbers = T)

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_deplevel_1.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()

props <- prop.table(x, margin = 2) #margin definition 1: by row, 2: by column 
colnames(props) <- c("Cluster 1", "Cluster 2", "Cluster 3", 
                     "Cluster 4", "Cluster 5", "Cluster 6")

p <- pheatmap(props, cluster_rows = F, cluster_cols = F,
              legend = F, annotation_names_row = T,
              annotation_names_col = T, angle_col=45,
              display_numbers = T, number_format = "%.2f")

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_deplevel_2.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()

# DICHOTOMIZE AS SCORE 3
df_depressed$DPQ010_d <- ifelse(df_depressed$DPQ010 == 3, 1, 0)
df_depressed$DPQ020_d <- ifelse(df_depressed$DPQ020 == 3, 1, 0)
df_depressed$DPQ030_d <- ifelse(df_depressed$DPQ030 == 3, 1, 0)
df_depressed$DPQ040_d <- ifelse(df_depressed$DPQ040 == 3, 1, 0)
df_depressed$DPQ050_d <- ifelse(df_depressed$DPQ050 == 3, 1, 0)
df_depressed$DPQ060_d <- ifelse(df_depressed$DPQ060 == 3, 1, 0)
df_depressed$DPQ070_d <- ifelse(df_depressed$DPQ070 == 3, 1, 0)
df_depressed$DPQ080_d <- ifelse(df_depressed$DPQ080 == 3, 1, 0)
df_depressed$DPQ090_d <- ifelse(df_depressed$DPQ090 == 3, 1, 0)

# # DICHOTOMIZE AS SCORE 2 or 3
# df_depressed$DPQ010_d <- ifelse(df_depressed$DPQ010 == 3 | df_depressed$DPQ010 == 2, 1, 0)
# df_depressed$DPQ020_d <- ifelse(df_depressed$DPQ020 == 3 | df_depressed$DPQ020 == 2, 1, 0)
# df_depressed$DPQ030_d <- ifelse(df_depressed$DPQ030 == 3 | df_depressed$DPQ030 == 2, 1, 0)
# df_depressed$DPQ040_d <- ifelse(df_depressed$DPQ040 == 3 | df_depressed$DPQ040 == 2, 1, 0)
# df_depressed$DPQ050_d <- ifelse(df_depressed$DPQ050 == 3 | df_depressed$DPQ050 == 2, 1, 0)
# df_depressed$DPQ060_d <- ifelse(df_depressed$DPQ060 == 3 | df_depressed$DPQ060 == 2, 1, 0)
# df_depressed$DPQ070_d <- ifelse(df_depressed$DPQ070 == 3 | df_depressed$DPQ070 == 2, 1, 0)
# df_depressed$DPQ080_d <- ifelse(df_depressed$DPQ080 == 3 | df_depressed$DPQ080 == 2, 1, 0)
# df_depressed$DPQ090_d <- ifelse(df_depressed$DPQ090 == 3 | df_depressed$DPQ090 == 2, 1, 0)
# 
# # DICHOTOMIZE AS SCORE 1 or 2 or 3
# df_depressed$DPQ010_d <- ifelse(df_depressed$DPQ010 == 0, 0, 1)
# df_depressed$DPQ020_d <- ifelse(df_depressed$DPQ020 == 0, 0, 1)
# df_depressed$DPQ030_d <- ifelse(df_depressed$DPQ030 == 0, 0, 1)
# df_depressed$DPQ040_d <- ifelse(df_depressed$DPQ040 == 0, 0, 1)
# df_depressed$DPQ050_d <- ifelse(df_depressed$DPQ050 == 0, 0, 1)
# df_depressed$DPQ060_d <- ifelse(df_depressed$DPQ060 == 0, 0, 1)
# df_depressed$DPQ070_d <- ifelse(df_depressed$DPQ070 == 0, 0, 1)
# df_depressed$DPQ080_d <- ifelse(df_depressed$DPQ080 == 0, 0, 1)
# df_depressed$DPQ090_d <- ifelse(df_depressed$DPQ090 == 0, 0, 1)

# props <- cbind(
# prop.table(table(df_depressed$DPQ_total, df_depressed$DPQ010_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_total, df_depressed$DPQ020_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_total, df_depressed$DPQ030_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_total, df_depressed$DPQ040_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_total, df_depressed$DPQ050_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_total, df_depressed$DPQ060_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_total, df_depressed$DPQ070_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_total, df_depressed$DPQ080_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_total, df_depressed$DPQ090_d), 1)[,2]
# )
# colnames(props) <- c("DPQ10","DPQ20","DPQ30",
#                      "DPQ40","DPQ50","DPQ60",
#                      "DPQ70","DPQ80","DPQ90")
# 
# 
# p <- pheatmap(props, cluster_rows = F, cluster_cols = F,
#          legend = F, annotation_names_row = T,
#          annotation_names_col = T, angle_col=45,
#          display_numbers = T)
# 
# tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/PHQ_DPQ_dich.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
# p
# dev.off()



props <- cbind(
  prop.table(table(df_depressed$cluster_membership, df_depressed$DPQ010_d), 1)[,2],
  prop.table(table(df_depressed$cluster_membership, df_depressed$DPQ020_d), 1)[,2],
  prop.table(table(df_depressed$cluster_membership, df_depressed$DPQ030_d), 1)[,2],
  prop.table(table(df_depressed$cluster_membership, df_depressed$DPQ040_d), 1)[,2],
  prop.table(table(df_depressed$cluster_membership, df_depressed$DPQ050_d), 1)[,2],
  prop.table(table(df_depressed$cluster_membership, df_depressed$DPQ060_d), 1)[,2],
  prop.table(table(df_depressed$cluster_membership, df_depressed$DPQ070_d), 1)[,2],
  prop.table(table(df_depressed$cluster_membership, df_depressed$DPQ080_d), 1)[,2],
  prop.table(table(df_depressed$cluster_membership, df_depressed$DPQ090_d), 1)[,2]
)
colnames(props) <- c("DPQ10","DPQ20","DPQ30",
                     "DPQ40","DPQ50","DPQ60",
                     "DPQ70","DPQ80","DPQ90")


p <- pheatmap(props, cluster_rows = F, cluster_cols = F,
              legend = F, annotation_names_row = T,
              annotation_names_col = T, angle_col=45,
              display_numbers = T)

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_DPQ.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()

# loop to remerge with the rest of the individuals

for (j in 1:15) {
  filename <- paste0("clean_",j,".rds")
  df <- readRDS(filename)
  df_excluded <- df[df$depressed == 0 | is.na(df$depressed),]
  df_excluded$cluster_membership <- NA
  df_excluded$dep_category <- NA
  var_exclude <- c("DPQ010_d","DPQ020_d","DPQ030_d","DPQ040_d",
                   "DPQ050_d","DPQ060_d","DPQ070_d","DPQ080_d","DPQ090_d")
  df_depressed <- df_depressed[ , !names(df_depressed) %in% var_exclude]
  
  df_all <- rbind(df_depressed, df_excluded)
  
  # recoding income
  df_all$ann_household_income <- as.character(df_all$ann_household_income)
  df_all$ann_household_income[df_all$ann_household_income == "10,000-14,999" | 
                                df_all$ann_household_income == "5,000-9,999" |
                                df_all$ann_household_income == "15,000-19,999" |
                                df_all$ann_household_income == "0-4,999"] <- "Under 20,000"
  df_all$ann_household_income[df_all$ann_household_income == "25,000-34,999" | 
                                df_all$ann_household_income == "20,000-24,999" |
                                df_all$ann_household_income == "45,000-54,999" |
                                df_all$ann_household_income == "65,000-74,999" |
                                df_all$ann_household_income == "35,000-44,999" |
                                df_all$ann_household_income == "55,000-64,999" |
                                df_all$ann_household_income == "75,000-99,999" |
                                df_all$ann_household_income == "20,000 and over"] <- "20,000 - 100,000"
  df_all$ann_household_income <- as.factor(df_all$ann_household_income)
  
  filename <- paste0("cluster_merged_",j,".rds")
  saveRDS(df_all, filename)
  # stata_filename <- paste0("cluster_merged_",j,".dta")
  # write.dta(df_all, stata_filename)
}













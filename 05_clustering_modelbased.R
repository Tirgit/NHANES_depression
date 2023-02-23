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
library(forcats)

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

# df_depressed <- df
df_depressed <- df[df$DPQ_total >= 10,]


# res <- VarSelCluster(df_depressed[,dpq_vars], gvals = 1:10,
#                              vbleSelec = FALSE, crit.varsel = "BIC")
# print(res)

# g = 6 is the cluster with the best performance
set.seed(9365)
res <- VarSelCluster(df_depressed[,dpq_vars], gvals =6,
                             vbleSelec = FALSE, crit.varsel = "BIC")
print(res)

summary(res)
table(fitted(res))
head(fitted(res, type="probability"),30)
# plot(res)
# plot(x=res, y="DPQ010")
# plot(x=res, y="DPQ020")
# plot(x=res, y="DPQ030")
# plot(x=res, y="DPQ040")
# plot(x=res, y="DPQ050")
# plot(x=res, y="DPQ060")
# plot(x=res, y="DPQ070")
# plot(x=res, y="DPQ080")
# plot(x=res, y="DPQ090")

# extract cluster membership
cluster_membership <- fitted(res)
df_depressed <- cbind(df_depressed, cluster_membership)



# CHI SQUARED TESTS TO TEST OBSERVED VS EXPECTED (iterate over the variables DPQ010-DPQ090)
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



# VISUALIZATION AND PROPORTIONS 
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


# EXPLORATION OF PRESCRIPTION MEDICATIONS
df_meds <- readRDS("meds_df.rds")
df_meds$RXDDCN1A[df_meds$RXDDRUG == 77777 | df_meds$RXDDRUG == 99999] <- "REFUSED/MISSING"

# left join will only keep those SEQN IDs that are in df_depressed
# but will create multiple rows for each med they are taking
df_dep_med <- left_join(df_depressed, df_meds, by = "SEQN")
# check that it is the same IDs included
# length(unique(df_depressed$SEQN))
# length(unique(df_dep_med$SEQN))
# sum(unique(df_depressed$SEQN) == unique(df_dep_med$SEQN))

# how many people do not use meds? n=820
table(df_dep_med$RXDUSE)

# tabulate number of meds
counted_data <- df_dep_med[df_dep_med$RXDUSE == 1,] %>%
  group_by(SEQN) %>%
  summarize(n_obs = n())
colnames(counted_data) <- c("SEQN", "n_meds")
table(counted_data$n_meds)

# histogram of number of meds used by cluster
df_dep_nmed <- left_join(df_depressed, counted_data, by = "SEQN")
df_dep_nmed$n_meds[is.na(df_dep_nmed$n_meds)] <- 0
table(df_dep_nmed$n_meds)

breaks <- c(-1, 0.9, 1.9, 2.9, 3.9, 4.9, 5.9, 50)
labels <- c("0", "1", "2", "3", "4", "5", "6 or more")

# Bin x into categories and convert to factor
df_dep_nmed$n_med_cat <- cut(df_dep_nmed$n_meds, breaks = breaks, labels = labels)
df_dep_nmed$n_med_cat <- as.factor(df_dep_nmed$n_med_cat)
df_dep_nmed$cluster_membership <- as.factor(df_dep_nmed$cluster_membership)

# plot
p <- ggplot(df_dep_nmed, aes(x = cluster_membership, fill = n_med_cat)) +
  geom_bar(alpha = 0.9, position = "fill") +
  labs(x = "Cluster", y = "Count", fill = "Number of\nunique\nprescription\nmedications") +
  theme_classic()

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_n_meds.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()


# visualization of categories per cluster, level 1, only those with medication
df_dep_med_onlyuse <- df_dep_med[!is.na(df_dep_med$RXDDCN1A),]

df_dep_norep <- distinct(df_dep_med_onlyuse, SEQN, RXDDCN1A, .keep_all = TRUE)

cluster_counts <- df_dep_norep %>% 
  group_by(cluster_membership) %>% 
  summarize(n_unique = n_distinct(SEQN))

count_data <- df_dep_norep %>% 
  group_by(cluster_membership, RXDDCN1A) %>%
  summarize(n = n())

# chi square for each medication (iterate over 1:18)
cont_table <- xtabs(n ~ cluster_membership + RXDDCN1A, data = count_data)
colnames(cont_table)
for (n_med in 1:18) {
print(colnames(cont_table)[n_med])
chi_sq_test <- chisq.test(cont_table[,n_med])
print(chi_sq_test)
}


new_row <- data.frame(cluster_membership = 2, RXDDCN1A = "ALTERNATIVE MEDICINES", n = 0.000001)
count_data <- rbind(count_data, new_row)
new_row <- data.frame(cluster_membership = 3, RXDDCN1A = "ALTERNATIVE MEDICINES", n = 0.000001)
count_data <- rbind(count_data, new_row)
new_row <- data.frame(cluster_membership = 1, RXDDCN1A = "BIOLOGICALS", n = 0.000001)
count_data <- rbind(count_data, new_row)
new_row <- data.frame(cluster_membership = 3, RXDDCN1A = "BIOLOGICALS", n = 0.000001)
count_data <- rbind(count_data, new_row)
new_row <- data.frame(cluster_membership = 4, RXDDCN1A = "BIOLOGICALS", n = 0.000001)
count_data <- rbind(count_data, new_row)
new_row <- data.frame(cluster_membership = 5, RXDDCN1A = "BIOLOGICALS", n = 0.000001)
count_data <- rbind(count_data, new_row)
new_row <- data.frame(cluster_membership = 6, RXDDCN1A = "BIOLOGICALS", n = 0.000001)
count_data <- rbind(count_data, new_row)
new_row <- data.frame(cluster_membership = 6, RXDDCN1A = "IMMUNOLOGIC AGENTS", n = 0.000001)
count_data <- rbind(count_data, new_row)

count_data$Proportion <- NA
count_data$lCI <- NA
count_data$uCI <- NA


for (cluster_n in 1:6) {
  # calculate proportion
  count_data$Proportion[count_data$cluster_membership == cluster_n] <- 
    count_data$n[count_data$cluster_membership == cluster_n]/cluster_counts$n_unique[cluster_n]
  
  # calculate 95% CI lower bound
  count_data$lCI[count_data$cluster_membership == cluster_n] <- count_data$Proportion[count_data$cluster_membership == cluster_n] - 1.96*(sqrt((count_data$Proportion[count_data$cluster_membership == cluster_n]*(1-count_data$Proportion[count_data$cluster_membership == cluster_n]))/cluster_counts$n_unique[cluster_n]))
  
  # calculate 95% CI upper bound
  count_data$uCI[count_data$cluster_membership == cluster_n] <- count_data$Proportion[count_data$cluster_membership == cluster_n] + 1.96*(sqrt((count_data$Proportion[count_data$cluster_membership == cluster_n]*(1-count_data$Proportion[count_data$cluster_membership == cluster_n]))/cluster_counts$n_unique[cluster_n]))
}

count_data$lCI[count_data$lCI < 0] <- 0
count_data[101:108,3:6] <- 0

colnames(count_data) <- c("Cluster", "Medication", "n", "Proportion", "lCI", "uCI")
 
count_data$Cluster <- as.character(count_data$Cluster)

count_data$Cluster[count_data$Cluster == "1"] <- "Uniform severe depression cluster"
count_data$Cluster[count_data$Cluster == "2"] <- "Severe somatic symptom profile cluster"
count_data$Cluster[count_data$Cluster == "3"] <- "Moderate somatic depression cluster"
count_data$Cluster[count_data$Cluster == "4"] <- "Uniform moderately severe depression cluster"
count_data$Cluster[count_data$Cluster == "5"] <- "Uniform moderate depression cluster"
count_data$Cluster[count_data$Cluster == "6"] <- "Severe mental depression cluster"

count_data$Medication <- factor(count_data$Medication, 
                                levels = sort(unique(count_data$Medication)))

clusnames <- c("Uniform severe depression cluster", 
               "Severe somatic symptom profile cluster",
               "Moderate somatic depression cluster",
               "Uniform moderately severe depression cluster",
               "Uniform moderate depression cluster",
               "Severe mental depression cluster")

for (i in clusnames) {
p <- ggplot(count_data[count_data$Cluster == i,], aes(x = Proportion, y = Medication, fill = Medication)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(xmin=lCI, xmax=uCI), width=.2) +
  theme_minimal() +
  labs(x = "", y = "", fill = NULL) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 10))) +
  scale_x_continuous(limits = c(0, 0.8), expand = c(0, 0))

filename <- paste0("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/",i,"_meds.tiff")
tiff(filename, units="in", width=10, height=10, res=300, compression = 'lzw')
print(p)
dev.off()
}

p <- ggplot(count_data, aes(x = Proportion, y = Medication, fill = Medication)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(xmin=lCI, xmax=uCI), width=.2) +
  theme_minimal() +
  facet_wrap(~ Cluster, ncol = 1, scales = "free") +
  labs(x = "", y = "", fill = NULL) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 10))) +
  scale_x_continuous(limits = c(0, 0.8), expand = c(0, 0))

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/allcluster_meds.tiff", units="in", width=10, height=10, res=300, compression = 'lzw')
print(p)
dev.off()


# FOCUS ON 4 medication groups:

p <- ggplot(count_data[count_data$Medication == "CARDIOVASCULAR AGENTS",], aes(x = Proportion, y = Cluster, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(xmin=lCI, xmax=uCI), width=.2) +
  theme_minimal() +
  labs(x = "", y = "", fill = NULL) +
  theme(axis.ticks.y = element_blank(),
        axis.text = element_text(size = 20),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(0, 0.8), expand = c(0, 0))

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/CVD_meds_cluster.tiff", units="in", width=10, height=10, res=300, compression = 'lzw')
print(p)
dev.off()

p <- ggplot(count_data[count_data$Medication == "CENTRAL NERVOUS SYSTEM AGENTS",], aes(x = Proportion, y = Cluster, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(xmin=lCI, xmax=uCI), width=.2) +
  theme_minimal() +
  labs(x = "", y = "", fill = NULL) +
  theme(axis.ticks.y = element_blank(),
        axis.text = element_text(size = 20),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(0, 0.8), expand = c(0, 0))

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/CNS_meds_cluster.tiff", units="in", width=10, height=10, res=300, compression = 'lzw')
print(p)
dev.off()

p <- ggplot(count_data[count_data$Medication == "METABOLIC AGENTS",], aes(x = Proportion, y = Cluster, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(xmin=lCI, xmax=uCI), width=.2) +
  theme_minimal() +
  labs(x = "", y = "", fill = NULL) +
  theme(axis.ticks.y = element_blank(),
        axis.text = element_text(size = 20),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(0, 0.8), expand = c(0, 0))

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/METABOLIC_meds_cluster.tiff", units="in", width=10, height=10, res=300, compression = 'lzw')
print(p)
dev.off()

p <- ggplot(count_data[count_data$Medication == "PSYCHOTHERAPEUTIC AGENTS",], aes(x = Proportion, y = Cluster, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(xmin=lCI, xmax=uCI), width=.2) +
  theme_minimal() +
  labs(x = "", y = "", fill = NULL) +
  theme(axis.ticks.y = element_blank(),
        axis.text = element_text(size = 20),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(0, 0.8), expand = c(0, 0))

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/PSYCH_meds_cluster.tiff", units="in", width=10, height=10, res=300, compression = 'lzw')
print(p)
dev.off()



# LOOP TO REMERGE WITH THE REST OF INDIVIDUALS

for (j in 1:15) {
  filename <- paste0("clean_",j,".rds")
  df <- readRDS(filename)
  df_excluded <- df[df$depressed == 0 | is.na(df$depressed),]
  df_excluded$cluster_membership <- NA
  df_excluded$dep_category <- NA
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













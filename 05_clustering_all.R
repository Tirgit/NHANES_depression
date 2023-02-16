# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

# libraries
library(cluster) 
library(dendextend)
library(ggplot2)
library(ggdendro)
library(fpc)
library(dplyr)
library(reshape2)
library(gplots)
library(FactoMineR)
library(factoextra)
library(ClustOfVar)
library(pheatmap)

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

# load imputed data
df <- readRDS("clean_1.rds")
df_depressed <- df[!is.na(df$DPQ_total),]
table(df_depressed$depressed, useNA = "always")

df_non <- df_depressed[df_depressed$DPQ_total <= 4,]
df_mild <- df_depressed[df_depressed$DPQ_total >= 5 & df_depressed$DPQ_total <= 9,]
df_mod <- df_depressed[df_depressed$DPQ_total >= 10 & df_depressed$DPQ_total <= 14,]
df_modsev <- df_depressed[df_depressed$DPQ_total >= 15 & df_depressed$DPQ_total <= 19,]
df_sev <-  df_depressed[df_depressed$DPQ_total >= 20,]

#### MCA ANALYSIS, MODERATE

# https://datascienceplus.com/using-mca-and-variable-clustering-in-r-for-insights-in-customer-attrition/
# http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials
# https://machinelearnit.com/2018/01/22/acm-clustering/
# http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials#algorithm-of-the-hcpc-method
# http://www.sthda.com/english/articles/22-principal-component-methods-videos/74-hcpc-using-factominer-video/


df_depressed$DPQ010 <- factor(df_depressed$DPQ010, ordered=T)
df_depressed$DPQ020 <- factor(df_depressed$DPQ020, ordered=T)
df_depressed$DPQ030 <- factor(df_depressed$DPQ030, ordered=T)
df_depressed$DPQ040 <- factor(df_depressed$DPQ040, ordered=T)
df_depressed$DPQ050 <- factor(df_depressed$DPQ050, ordered=T)
df_depressed$DPQ060 <- factor(df_depressed$DPQ060, ordered=T)
df_depressed$DPQ070 <- factor(df_depressed$DPQ070, ordered=T)
df_depressed$DPQ080 <- factor(df_depressed$DPQ080, ordered=T)
df_depressed$DPQ090 <- factor(df_depressed$DPQ090, ordered=T)

res.mca <- MCA(df_depressed[,21:29], graph=TRUE)
fviz_mca_var(res.mca, repel=TRUE)
eigenvalues <- get_eigenvalue(res.mca)
head(round(eigenvalues, 2), 10)
fviz_screeplot(res.mca)

# run variable clustering excluding the target variable (churn) 
variable_tree <- hclustvar(X.quali = df_depressed[,21:29])
print(variable_tree)
#plot the dendrogram of variable groups
plot(variable_tree)

# variables 
var <- get_mca_var(res.mca)
var$contrib
fviz_contrib(res.mca, choice = "var", axes = 1)

# individuals
ind <- get_mca_ind(res.mca)
ind

fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())

fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "DPQ090", # color by groups 
             palette = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             ellipse.level = 0.95, alpha.ind = 0.25,
             ggtheme = theme_minimal()) 

fviz_ellipses(res.mca, c("DPQ010", "DPQ090"),
              geom = "point")


# clustering
res.hcpc <- HCPC(res.mca, min = 3, max = 10, nb.clust = -1, graph = TRUE)

# Individuals factor map
p <- fviz_cluster(res.hcpc, geom = "point", main = "Factor map")

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/DPQ_clusters.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()

# test variable differences
res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$category

# cluster vector
cluster_membership <- res.hcpc$data.clust$clust
df_depressed <- cbind(df_depressed, cluster_membership)


x <- table(df_depressed$DPQ_total, df_depressed$cluster_membership)
props <- prop.table(x, margin = 1) #margin definition 1: by row, 2: by column 
colnames(props) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")

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
colnames(props) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")

p <- pheatmap(props, cluster_rows = F, cluster_cols = F,
              legend = F, annotation_names_row = T,
              annotation_names_col = T, angle_col=45,
              display_numbers = T)

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_deplevel_1.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()

props <- prop.table(x, margin = 2) #margin definition 1: by row, 2: by column 
colnames(props) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")

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



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
df <- readRDS("clean_5.rds")

df_mod <- df[df$DPQ_total >= 10 & df$DPQ_total <= 14,]
df_modsev <- df[df$DPQ_total >= 15 & df$DPQ_total <= 19,]
df_sev <-  df[df$DPQ_total >= 20,]

#### MCA ANALYSIS, MODERATE

# https://datascienceplus.com/using-mca-and-variable-clustering-in-r-for-insights-in-customer-attrition/
# http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials
# https://machinelearnit.com/2018/01/22/acm-clustering/
# http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials#algorithm-of-the-hcpc-method
# http://www.sthda.com/english/articles/22-principal-component-methods-videos/74-hcpc-using-factominer-video/

df_depressed <- df_mod

df_depressed$DPQ010 <- as.factor(df_depressed$DPQ010)
df_depressed$DPQ020 <- as.factor(df_depressed$DPQ020)
df_depressed$DPQ030 <- as.factor(df_depressed$DPQ030)
df_depressed$DPQ040 <- as.factor(df_depressed$DPQ040)
df_depressed$DPQ050 <- as.factor(df_depressed$DPQ050)
df_depressed$DPQ060 <- as.factor(df_depressed$DPQ060)
df_depressed$DPQ070 <- as.factor(df_depressed$DPQ070)
df_depressed$DPQ080 <- as.factor(df_depressed$DPQ080)
df_depressed$DPQ090 <- as.factor(df_depressed$DPQ090)

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

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/DPQ_clusters_mod.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()

# test variable differences
res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$category

# cluster vector
DPQ_score <- df_depressed$DPQ_total
cluster_membership <- res.hcpc$data.clust$clust
df_depressed <- cbind(df_depressed, DPQ_score, cluster_membership)


x <- table(df_depressed$DPQ_score, df_depressed$cluster_membership)
props <- prop.table(x, margin = 1) #margin definition 1: by row, 2: by column 
colnames(props) <- c("Cluster 1", "Cluster 2", "Cluster 3")

p <- pheatmap(props, cluster_rows = F, cluster_cols = F,
              legend = F, annotation_names_row = T,
              annotation_names_col = T, angle_col=45,
              display_numbers = T)

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_PHQ_mod.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
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
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ010_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ020_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ030_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ040_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ050_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ060_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ070_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ080_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ090_d), 1)[,2]
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

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_DPQ_dich_mod.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()













#### MCA ANALYSIS, MODERATELY SEVERE

# https://datascienceplus.com/using-mca-and-variable-clustering-in-r-for-insights-in-customer-attrition/
# http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials
# https://machinelearnit.com/2018/01/22/acm-clustering/
# http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials#algorithm-of-the-hcpc-method
# http://www.sthda.com/english/articles/22-principal-component-methods-videos/74-hcpc-using-factominer-video/

df_depressed <- df_modsev

df_depressed$DPQ010 <- as.factor(df_depressed$DPQ010)
df_depressed$DPQ020 <- as.factor(df_depressed$DPQ020)
df_depressed$DPQ030 <- as.factor(df_depressed$DPQ030)
df_depressed$DPQ040 <- as.factor(df_depressed$DPQ040)
df_depressed$DPQ050 <- as.factor(df_depressed$DPQ050)
df_depressed$DPQ060 <- as.factor(df_depressed$DPQ060)
df_depressed$DPQ070 <- as.factor(df_depressed$DPQ070)
df_depressed$DPQ080 <- as.factor(df_depressed$DPQ080)
df_depressed$DPQ090 <- as.factor(df_depressed$DPQ090)

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

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/DPQ_clusters_modsev.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()

# test variable differences
res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$category

# cluster vector
DPQ_score <- df_depressed$DPQ_total
cluster_membership <- res.hcpc$data.clust$clust
df_depressed <- cbind(df_depressed, DPQ_score, cluster_membership)


x <- table(df_depressed$DPQ_score, df_depressed$cluster_membership)
props <- prop.table(x, margin = 1) #margin definition 1: by row, 2: by column 
colnames(props) <- c("Cluster 1", "Cluster 2", "Cluster 3")

p <- pheatmap(props, cluster_rows = F, cluster_cols = F,
              legend = F, annotation_names_row = T,
              annotation_names_col = T, angle_col=45,
              display_numbers = T)

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_PHQ_modsev.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
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
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ010_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ020_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ030_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ040_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ050_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ060_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ070_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ080_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ090_d), 1)[,2]
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

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_DPQ_dich_modsev.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()













#### MCA ANALYSIS, SEVERE

# https://datascienceplus.com/using-mca-and-variable-clustering-in-r-for-insights-in-customer-attrition/
# http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials
# https://machinelearnit.com/2018/01/22/acm-clustering/
# http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials#algorithm-of-the-hcpc-method
# http://www.sthda.com/english/articles/22-principal-component-methods-videos/74-hcpc-using-factominer-video/

df_depressed <- df_sev

df_depressed$DPQ010 <- as.factor(df_depressed$DPQ010)
df_depressed$DPQ020 <- as.factor(df_depressed$DPQ020)
df_depressed$DPQ030 <- as.factor(df_depressed$DPQ030)
df_depressed$DPQ040 <- as.factor(df_depressed$DPQ040)
df_depressed$DPQ050 <- as.factor(df_depressed$DPQ050)
df_depressed$DPQ060 <- as.factor(df_depressed$DPQ060)
df_depressed$DPQ070 <- as.factor(df_depressed$DPQ070)
df_depressed$DPQ080 <- as.factor(df_depressed$DPQ080)
df_depressed$DPQ090 <- as.factor(df_depressed$DPQ090)

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

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/DPQ_clusters_sev.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()

# test variable differences
res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$category

# cluster vector
DPQ_score <- df_depressed$DPQ_total
cluster_membership <- res.hcpc$data.clust$clust
df_depressed <- cbind(df_depressed, DPQ_score, cluster_membership)


x <- table(df_depressed$DPQ_score, df_depressed$cluster_membership)
props <- prop.table(x, margin = 1) #margin definition 1: by row, 2: by column 
colnames(props) <- c("Cluster 1", "Cluster 2", "Cluster 3")

p <- pheatmap(props, cluster_rows = F, cluster_cols = F,
              legend = F, annotation_names_row = T,
              annotation_names_col = T, angle_col=45,
              display_numbers = T)

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_PHQ_sev.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
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
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ010_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ020_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ030_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ040_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ050_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ060_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ070_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ080_d), 1)[,2],
# prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ090_d), 1)[,2]
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

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_DPQ_dich_sev.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()




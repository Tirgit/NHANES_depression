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

# keep depressed PHQ-9 >= 10
df$DPQ_total <- df$DPQ010 + df$DPQ020 + df$DPQ030 + 
                df$DPQ040 + df$DPQ050 + df$DPQ060 + 
                df$DPQ070 + df$DPQ080 + df$DPQ090

df$depressed <- 0
df$depressed[df$DPQ_total >= 10] <- 1
table(df$depressed)


# df$DPQ010 <- as.factor(df$DPQ010)
# df$DPQ020 <- as.factor(df$DPQ020)
# df$DPQ030 <- as.factor(df$DPQ030)
# df$DPQ040 <- as.factor(df$DPQ040)
# df$DPQ050 <- as.factor(df$DPQ050)
# df$DPQ060 <- as.factor(df$DPQ060)
# df$DPQ070 <- as.factor(df$DPQ070)
# df$DPQ080 <- as.factor(df$DPQ080)
# df$DPQ090 <- as.factor(df$DPQ090)


# narrow down to depressed
df_depressed <- df[df$depressed == 1,]
colnames(df_depressed)

# calculate Gower distance
# gower.dist <- daisy(df_depressed[,21:29], metric = c("gower"))
# divisive.clust <- diana(as.matrix(gower.dist), diss = TRUE, keep.diss = TRUE)
# plot(divisive.clust, main = "Divisive")


# perform hierarchical clustering
# aggl.clust.c <- hclust(gower.dist, method = "complete")
# plot(aggl.clust.c,
#     main = "Agglomerative, complete linkages")


# n_clusters <- 3


# dendro <- as.dendrogram(aggl.clust.c)
# dendro.col <- dendro %>%
#   set("branches_k_color", k = n_clusters, value = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) %>%
#   set("branches_lwd", 0.6) %>%
#   set("labels_colors", 
#       value = c("darkslategray")) %>% 
#   set("labels_cex", 0.5)
# ggd1 <- as.ggdend(dendro.col)
# p <- ggplot(ggd1, theme = theme_minimal()) +
#   labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 3")
# 
# tiff("cluster_dendro.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
# p
# dev.off()
# 
# 
# mycl <- cutree(aggl.clust.c, k=n_clusters)
# mycol <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# mycol <- mycol[as.vector(mycl)]
# 
# tiff("cluster_heatmap.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
# heatmap(as.matrix(df_depressed[,21:29]), Rowv=as.dendrogram(aggl.clust.c), Colv=NA,
#         #col=colorpanel(40, "black","yellow","green"),
#         scale="column", RowSideColors=mycol) 
# dev.off()
# 

#### MCA ANALYSIS

# https://datascienceplus.com/using-mca-and-variable-clustering-in-r-for-insights-in-customer-attrition/
# http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials
# https://machinelearnit.com/2018/01/22/acm-clustering/
# http://sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials#algorithm-of-the-hcpc-method
# http://www.sthda.com/english/articles/22-principal-component-methods-videos/74-hcpc-using-factominer-video/


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
p <- fviz_screeplot(res.mca)

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/PHQ_screeplot.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()

# run variable clustering excluding the target variable (churn) 
variable_tree <- hclustvar(X.quali = df_depressed[,21:29])
print(variable_tree)
#plot the dendrogram of variable groups
plot(variable_tree)

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/PHQ_variable_tree.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
plot(variable_tree)
dev.off()


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

# Individuals facor map
p <- fviz_cluster(res.hcpc, geom = "point", main = "Factor map")

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/DPQ_clusters.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()

# test variable differences
res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$category

# cluster vector
DPQ_score <- df_depressed$DPQ_total
cluster_membership <- res.hcpc$data.clust$clust
df_depressed <- cbind(df_depressed, DPQ_score, cluster_membership)
# Depression Severity: 0-4 none, 5-9 mild, 10-14 moderate, 15-19 moderately severe, 20-27 severe.
df_depressed$depression_severity <- "moderate"
df_depressed$depression_severity[df_depressed$DPQ_score >= 15] <- "moderate_severe"
df_depressed$depression_severity[df_depressed$DPQ_score >= 20] <- "severe"


x <- table(df_depressed$DPQ_score, df_depressed$cluster_membership)
props <- prop.table(x, margin = 1) #margin definition 1: by row, 2: by column 
colnames(props) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")

p <- pheatmap(props, cluster_rows = F, cluster_cols = F,
              legend = F, annotation_names_row = T,
              annotation_names_col = T, angle_col=45,
              display_numbers = T)

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_PHQ.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()


x <- table(df_depressed$depression_severity, df_depressed$cluster_membership)
props <- prop.table(x, margin = 1) #margin definition 1: by row, 2: by column 
colnames(props) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")

p <- pheatmap(props, cluster_rows = F, cluster_cols = F,
              legend = F, annotation_names_row = T,
              annotation_names_col = T, angle_col=45,
              display_numbers = T)

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_PHQcat1.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()

props <- prop.table(x, margin = 2) #margin definition 1: by row, 2: by column 
colnames(props) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")

p <- pheatmap(props, cluster_rows = F, cluster_cols = F,
              legend = F, annotation_names_row = T,
              annotation_names_col = T, angle_col=45,
              display_numbers = T)

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/cluster_PHQcat2.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()


df_depressed$DPQ010_d <- ifelse(df_depressed$DPQ010 == 0, 0, 1)
df_depressed$DPQ020_d <- ifelse(df_depressed$DPQ020 == 0, 0, 1)
df_depressed$DPQ030_d <- ifelse(df_depressed$DPQ030 == 0, 0, 1)
df_depressed$DPQ040_d <- ifelse(df_depressed$DPQ040 == 0, 0, 1)
df_depressed$DPQ050_d <- ifelse(df_depressed$DPQ050 == 0, 0, 1)
df_depressed$DPQ060_d <- ifelse(df_depressed$DPQ060 == 0, 0, 1)
df_depressed$DPQ070_d <- ifelse(df_depressed$DPQ070 == 0, 0, 1)
df_depressed$DPQ080_d <- ifelse(df_depressed$DPQ080 == 0, 0, 1)
df_depressed$DPQ090_d <- ifelse(df_depressed$DPQ090 == 0, 0, 1)

props <- cbind(
prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ010_d), 1)[,2],
prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ020_d), 1)[,2],
prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ030_d), 1)[,2],
prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ040_d), 1)[,2],
prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ050_d), 1)[,2],
prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ060_d), 1)[,2],
prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ070_d), 1)[,2],
prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ080_d), 1)[,2],
prop.table(table(df_depressed$DPQ_score, df_depressed$DPQ090_d), 1)[,2]
)
colnames(props) <- c("DPQ10","DPQ20","DPQ30",
                     "DPQ40","DPQ50","DPQ60",
                     "DPQ70","DPQ80","DPQ90")


p <- pheatmap(props, cluster_rows = F, cluster_cols = F,
         legend = F, annotation_names_row = T,
         annotation_names_col = T, angle_col=45,
         display_numbers = T)

tiff("H:/BACKUP/Projects/Joan_Aina_projects/NHANES_depression/PHQ_DPQ_dich.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()













# adding cluster info to the imputed clean files in loop
DPQ_score <- df_depressed$DPQ_total
cluster_membership <- res.hcpc$data.clust$clust
df_depressed <- cbind(df_depressed, DPQ_score, cluster_membership)


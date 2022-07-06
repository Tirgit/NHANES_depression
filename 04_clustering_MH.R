# libraries
library(cluster) 
library(dendextend)
library(ggplot2)
library(ggdendro)
library(fpc)
library(dplyr)
library(reshape2)
library(gplots)

# https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995

# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

# load imputed data
df_imputed <- readRDS("imputed_1.rds")

# remove all those with no PHQ data at all
df <- df_imputed[df_imputed$phq == "Yes",]
df$phq <- NULL

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
gower.dist <- daisy(df_depressed[,21:29], metric = c("gower"))
# divisive.clust <- diana(as.matrix(gower.dist), diss = TRUE, keep.diss = TRUE)
# plot(divisive.clust, main = "Divisive")


# perform hierarchical clustering
aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative, complete linkages")


n_clusters <- 4


dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = n_clusters, value = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col)
p <- ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 4")

tiff("cluster_dendro.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
p
dev.off()


mycl <- cutree(aggl.clust.c, k=n_clusters)
mycol <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
mycol <- mycol[as.vector(mycl)]

tiff("cluster_heatmap.tiff", units="in", width=5, height=4, res=300, compression = 'lzw')
heatmap(as.matrix(df_depressed[,21:29]), Rowv=as.dendrogram(aggl.clust.c), Colv=NA,
        #col=colorpanel(40, "black","yellow","green"),
        scale="column", RowSideColors=mycol) 
dev.off()









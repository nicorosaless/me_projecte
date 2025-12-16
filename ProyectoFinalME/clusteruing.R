# Clustering demographic dataset

rm(list=ls())
##-- Libraries
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(NbClust)    # Function NbClust
library(factoextra) # Several clustering graphics
library(hopkins)    # Hopkins index
library(FactoMineR) # Factor analysis
library(dendextend) # Compare dendograms
library(corrplot)   # Correlation graphics
library(cluster) 
##-- Load dataset
dd <- read.csv(".../demographic.csv",
               header = TRUE, sep = ",") #ajustar path

# 2. Remove cycle column and NAs

dd <- dd[, !(names(dd) == "SDDSRVYR")]
dd <- dd[, !(names(dd) == "RIDAGEMN")]
dd <- dd[, !(names(dd) == "RIDEXAGM")]
dd <- dd[, !(names(dd) == "DMQADFC")]
dd <- dd[, !(names(dd) == "DMDYRSUS")]
dd <- dd[, !(names(dd) == "DMDEDUC3")]
dd <- dd[, !(names(dd) == "RIDEXPRG")]
dd <- dd[, !(names(dd) == "AIALANGA")]
dd <- dd[, !(names(dd) == "DMDHSEDU")]
dd <- dd[!is.na(dd$INDFMPIR), ] 
dd <- dd[!is.na(dd$INDHHIN2), ]
dd <- dd[!is.na(dd$MIALANG), ]
dd <- dd[!is.na(dd$MIAPROXY), ]
dd <- dd[!is.na(dd$MIAINTRP), ]

# 3. Define categorical variables

categorical_vars <- c(
  "RIDSTATR","RIAGENDR","RIDRETH1","RIDRETH3","RIDEXMON","DMQMILIZ","DMDBORN4","DMDCITZN","DMDEDUC2","DMDMARTL"
  ,"SIALANG","SIAPROXY","SIAINTRP","FIALANG","FIAPROXY",
  "FIAINTRP","MIALANG","MIAPROXY","MIAINTRP","DMDHHSIZ",
  "DMDFMSIZ","DMDHHSZA","DMDHHSZB","DMDHHSZE","DMDHRGND",
  "DMDHRBR4","DMDHREDU","DMDHRMAR"
)


dd$DMDBORN4 <- as.numeric(dd$DMDBORN4)
dd$DMDBORN4[dd$DMDBORN4 %in% c(77,99) | is.na(dd$DMDBORN4)] <- 0
# DMDCITZN: valores existentes [1,2, NA, 7,9]
dd$DMDCITZN <- as.numeric(dd$DMDCITZN)
dd$DMDCITZN[dd$DMDCITZN %in% c(7,9) | is.na(dd$DMDCITZN)] <- 0

dd$DMDEDUC2 <- as.numeric(dd$DMDEDUC2)
dd$DMDEDUC2[dd$DMDEDUC2 %in% c(7,9) | is.na(dd$DMDEDUC2)] <- 0

# DMDHRBR4: valores existentes [1,2, NA,77]
dd$DMDHRBR4 <- as.numeric(dd$DMDHRBR4)
dd$DMDHRBR4[dd$DMDHRBR4 %in% c(77) | is.na(dd$DMDHRBR4)] <- 0

# DMDHREDU: valores existentes [1,2,3,4,5,NA,7,9]
dd$DMDHREDU <- as.numeric(dd$DMDHREDU)
dd$DMDHREDU[dd$DMDHREDU %in% c(7,9) | is.na(dd$DMDHREDU)] <- 0 

dd$DMQMILIZ <- as.numeric(dd$DMQMILIZ)
dd$DMQMILIZ[dd$DMQMILIZ %in% c(7,9) | is.na(dd$DMQMILIZ)] <- 0 

# DMDHRMAR: valores existentes [1,2,3,4,5,6,NA,77,99]
dd$DMDHRMAR <- as.numeric(dd$DMDHRMAR)
dd$DMDHRMAR[dd$DMDHRMAR %in% c(77,99) | is.na(dd$DMDHRMAR)] <- 0



cols_con_ceros <- c("DMDBORN4", "DMDCITZN", "DMDEDUC2", 
                    "DMDHRBR4", "DMDHREDU", "DMQMILIZ", "DMDHRMAR")

# Remove rows with zeros
dd <- dd[rowSums(dd[cols_con_ceros] == 0) == 0, ]

dd[categorical_vars] <- lapply(dd[categorical_vars], as.factor)
summary(dd)
set.seed(123)  
dd_500 <- dd[sample(nrow(dd), 500), ]

summary(dd_500)

gower_dist <- daisy(dd_500, metric = "gower")
gower_dist_sq <- gower_dist^2
hc_ward <- hclust(gower_dist_sq, method = "ward.D2")
hc_complete <- hclust(gower_dist_sq, method = "complete")

# iii. Dendrogram
par(mfrow = c(1, 2))
plot(hc_ward, main = "Ward.D2", cex = 0.7)
plot(hc_complete, main = "Complete", cex = 0.7)
par(mfrow = c(1, 1))

# Cophenetic correlation
cor(gower_dist, cophenetic(hc_ward))
cor(gower_dist, cophenetic(hc_complete))

# iv. Number of clusters
# Elbow method

suggested.level<-function(hc, min=3, max=10){
  
  if(min<2) stop("Min should be equal or higher than 2")
  intra    <- rev(cumsum(hc$height))
  quot     <- intra[min:(max)]/intra[(min - 1):(max - 1)]
  nb_clust <- which.min(quot) + min - 1
  return(nb_clust)
}

suggested.level(hc_ward)
suggested.level(hc_complete)


par(mfrow = c(1, 2))

# Ward
last_ward <- hc_ward$height[(length(hc_ward$height)-9):length(hc_ward$height)]
plot(length(last_ward):1, last_ward, type = "b", 
     main = "Elbow - Ward.D2", xlab = "Nº clusters", ylab = "Distancia")
grid()

# Complete
last_complete <- hc_complete$height[(length(hc_complete$height)-9):length(hc_complete$height)]
plot(length(last_complete):1, last_complete, type = "b",
     main = "Elbow - Complete", xlab = "Nº clusters", ylab = "Distancia")
grid()

par(mfrow = c(1, 1))


# v. Cluster description table
k <- 3  

clusters <- cutree(hc_ward, k = k)

# Summary table
cluster_size <- data.frame(
  Cluster = 1:k,
  Tamaño = as.numeric(table(clusters)),
  Porcentaje = round(as.numeric(table(clusters)) / nrow(dd_500) * 100, 2)
)
print(cluster_size)

# Cluster characteristics
for(i in 1:k) {
  print(summary(dd_500[clusters == i, ]))
}

# Descriptive plots (PNG)
if(!dir.exists("graficos_clusters")) dir.create("graficos_clusters")

# Identify variables
vars_cat_names <- intersect(names(dd_500), categorical_vars)
vars_num_names <- setdiff(names(dd_500), categorical_vars)

# 1. Numeric vars -> Boxplots
if(length(vars_num_names) > 0) {
  for(var_name in vars_num_names) {
    png(filename = paste0("graficos_clusters/boxplot_", var_name, ".png"), width = 800, height = 600)
    boxplot(dd_500[[var_name]] ~ clusters, 
            main = var_name,
            col = rainbow(k),
            xlab = "Cluster", ylab = "Valor")
    dev.off()
  }
}

# 2. Categorical vars -> Barplots
if(length(vars_cat_names) > 0) {
  for(var_name in vars_cat_names) {
    # Ensure factor
    vec_factor <- as.factor(dd_500[[var_name]])
    
    # Cross table
    tabla <- table(vec_factor, clusters)
    
    png(filename = paste0("graficos_clusters/barplot_", var_name, ".png"), width = 800, height = 600)
    # Proportional barplot
    barplot(prop.table(tabla, 2), 
            main = var_name,
            col = rainbow(nrow(tabla)),
            legend = rownames(tabla),
            args.legend = list(x = "topright", bty = "n", cex = 0.8),
            xlab = "Cluster", ylab = "Proporción")
    dev.off()
  }
}

dd_num <- dd_500[sapply(dd_500, is.numeric)]

summary(dd_num)

dd_num_scaled <- scale(dd_num)

km <- kmeans(dd_num_scaled, center = 5, nstart = 10)
km$size
heatmap(km$centers)
library(clustMixType)

kp <- kproto(dd_500, k =  5, type = "gower")
kp$centers

D <- as.matrix(gower_dist)

totss <- sum(D^2) / 2   # dividir entre 2 para no contar dos veces

# Within-cluster sum of squares
wcss <- 0
for (c in unique(kp$cluster)) {
  idx <- which(kp$cluster == c)
  Dc <- D[idx, idx]
  wcss <- wcss + sum(Dc^2) / 2
}

# Between
bcss <- totss - wcss

# Proporción explicada (equivalente a betweenss/totss)
explained <- bcss / totss
explained

EV <- IW <- c()

for (k in 1:10) {
  
  # k-prototypes
  kp <- kproto(dd_500, k = k, verbose = FALSE)
  
  # Within-cluster (Gower)
  wcss <- 0
  for (c in unique(kp$cluster)) {
    idx <- which(kp$cluster == c)
    Dc <- D[idx, idx]
    wcss <- wcss + sum(Dc^2) / 2
  }
  
  IW[k] <- wcss                      # inertia within equivalente
  EV[k] <- 1 - (wcss / totss)        # explained variability equivalente
}

par(mfrow=c(1,2))
plot(EV, type="b", pch=19,
  xlab="Number of clusters", ylab="Explained variability")
plot(IW, type="b", pch=19,
  xlab="Number of clusters", ylab="Inertia Within (Gower)")

# --- CORRECCIÓN: Recalcular k-prototypes específicamente para k=3 ---
set.seed(123) # Para reproducibilidad
kp <- kproto(dd_500, k = 3, verbose = FALSE)
# --------------------------------------------------------------------

# PCoA
# Gower to Euclidean
mds <- cmdscale(gower_dist, k = 2, eig = TRUE)
coords_mds <- as.data.frame(mds$points)
colnames(coords_mds) <- c("Dim1", "Dim2")

# Visualize clusters
par(mfrow = c(1, 2))

# Plot by clusters
# Define colors
mis_colores <- c("red", "green", "blue") 

plot(coords_mds$Dim1, coords_mds$Dim2, 
  col = mis_colores[kp$cluster], pch = 19, cex = 1.5,
  main = "K-Prototypes (k=3) - PCoA",
  xlab = paste0("Dim1 (", round(mds$eig[1]/sum(mds$eig)*100, 1), "%)"),
  ylab = paste0("Dim2 (", round(mds$eig[2]/sum(mds$eig)*100, 1), "%)"))
legend("topleft", legend = paste("Cluster", 1:3), col = mis_colores, pch = 19)

# Gráfico 2: Elbow plot
plot(EV, type = "b", pch = 19,
  xlab = "Number of clusters", ylab = "Explained variability",
  main = "Elbow plot")
abline(v = 3, col = "red", lty = 2, lwd = 2)
points(3, EV[3], col = "red", pch = 19, cex = 2)

par(mfrow = c(1, 1))

# K-Prototypes summary table
kp_cluster_size <- data.frame(
  Cluster = 1:3,
  Tamaño = as.numeric(table(kp$cluster)),
  Porcentaje = round(as.numeric(table(kp$cluster)) / nrow(dd_500) * 100, 2)
)
print(kp_cluster_size)

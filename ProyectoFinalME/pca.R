# PCA demographic dataset

##-- Libraries
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(dplyr)

##-- Cargar dataset
dd <- read.csv(".../demographic.csv",
               header = TRUE, sep = ",") #ajustar path

# 2. Remove cycle column and NAs

dd <- dd[, !(names(dd) == "SDDSRVYR")]
dd <- dd[!is.na(dd$INDFMPIR), ] 
dd <- dd[!is.na(dd$INDHHIN2), ]

# 3. Define categorical variables

categorical_vars <- c(
  "RIDSTATR","RIAGENDR","RIDRETH1","RIDRETH3","RIDEXMON","DMQMILIZ",
  "DMQADFC","DMDBORN4","DMDCITZN","DMDEDUC3","DMDEDUC2","DMDMARTL",
  "RIDEXPRG","SIALANG","SIAPROXY","SIAINTRP","FIALANG","FIAPROXY",
  "FIAINTRP","MIALANG","MIAPROXY","MIAINTRP","AIALANGA","DMDHHSIZ",
  "DMDFMSIZ","DMDHHSZA","DMDHHSZB","DMDHHSZE","DMDHRGND",
  "DMDHRBR4","DMDHREDU","DMDHRMAR","DMDHSEDU"
)


# 4. Remove categorical vars

dd_num <- dd[, !(names(dd) %in% categorical_vars)]
cols_a_eliminar <- c("RIDAGEMN", "RIDEXAGM", "DMDYRSUS")
dd_num <- dd_num[, !(names(dd_num) %in% cols_a_eliminar)]


# 5. Remove zero variance columns

zero_var_cols <- names(dd_num)[sapply(dd_num, function(x) var(x, na.rm = TRUE) == 0)]
dd_num <- dd_num[, !(names(dd_num) %in% zero_var_cols)]


# 6. Check data

summary(dd_num)
str(dd_num)

# 4. Scale and PCA

dd_scaled <- scale(dd_num)

pca_result_dd <- prcomp(dd_scaled)

# VAPs
pca_result_dd$sdev                           # standard deviations of principal components
sqrt(eigen(cov(dd_scaled))$values) # square roots of eigenvalues

# VEPs
pca_result_dd$rotation[,1]                     # loadings
eigen(cov(dd_scaled))$vec[,1] 

# 5. Plots

# Scree plot
fviz_screeplot(pca_result_dd, addlabels = TRUE, ylim = c(0, 50)) +
  ggtitle("Scree Plot: PCA")

summary(pca_result_dd)

# Variables
fviz_pca_var(pca_result_dd, repel = TRUE, col.var = "blue") +
  ggtitle("PCA – Variables")

fviz_pca_var(pca_result_dd, axes = c(3, 4), repel = TRUE, col.var = "blue") +
  ggtitle("PCA – Variables (Dim 3 & 4)")




# Individuos
fviz_pca_ind(pca_result_dd, repel = TRUE, col.ind = "red", geom = "point") +
  ggtitle("PCA – Individuos")



# Biplot
fviz_pca_biplot(pca_result_dd, repel = TRUE,
                col.var = "blue", col.ind = "red", alpha.ind = 0.4) +
  ggtitle("Biplot PCA")

# Componentes nuevos
head(pca_result_dd$x)

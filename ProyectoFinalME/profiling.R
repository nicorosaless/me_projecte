# Profiling demographic dataset

rm(list=ls())

# 1. Libraries
library(ggplot2)
library(dplyr)
library(cluster)
library(FactoMineR)
library(factoextra)
library(ggpubr)
library(psych)
library(DataExplorer)

# 2. Data preparation
# Path
dd <- read.csv(".../demographic.csv",
               header = TRUE, sep = ",") #ajustar path

# Remove columns
dd <- dd[, !(names(dd) == "SDDSRVYR")]
dd <- dd[, !(names(dd) == "RIDAGEMN")]
dd <- dd[, !(names(dd) == "RIDEXAGM")]
dd <- dd[, !(names(dd) == "DMQADFC")]
dd <- dd[, !(names(dd) == "DMDYRSUS")]
dd <- dd[, !(names(dd) == "DMDEDUC3")]
dd <- dd[, !(names(dd) == "RIDEXPRG")]
dd <- dd[, !(names(dd) == "AIALANGA")]
dd <- dd[, !(names(dd) == "DMDHSEDU")]

# Remove NAs
dd <- dd[!is.na(dd$INDFMPIR), ] 
dd <- dd[!is.na(dd$INDHHIN2), ]
dd <- dd[!is.na(dd$MIALANG), ]
dd <- dd[!is.na(dd$MIAPROXY), ]
dd <- dd[!is.na(dd$MIAINTRP), ]

# Categorical variables
categorical_vars <- c(
  "RIDSTATR","RIAGENDR","RIDRETH1","RIDRETH3","RIDEXMON","DMQMILIZ","DMDBORN4","DMDCITZN","DMDEDUC2","DMDMARTL"
  ,"SIALANG","SIAPROXY","SIAINTRP","FIALANG","FIAPROXY",
  "FIAINTRP","MIALANG","MIAPROXY","MIAINTRP","DMDHHSIZ",
  "DMDFMSIZ","DMDHHSZA","DMDHHSZB","DMDHHSZE","DMDHRGND",
  "DMDHRBR4","DMDHREDU","DMDHRMAR"
)

# Recoding
dd$DMDBORN4 <- as.numeric(dd$DMDBORN4)
dd$DMDBORN4[dd$DMDBORN4 %in% c(77,99) | is.na(dd$DMDBORN4)] <- 0

dd$DMDCITZN <- as.numeric(dd$DMDCITZN)
dd$DMDCITZN[dd$DMDCITZN %in% c(7,9) | is.na(dd$DMDCITZN)] <- 0

dd$DMDEDUC2 <- as.numeric(dd$DMDEDUC2)
dd$DMDEDUC2[dd$DMDEDUC2 %in% c(7,9) | is.na(dd$DMDEDUC2)] <- 0

dd$DMDHRBR4 <- as.numeric(dd$DMDHRBR4)
dd$DMDHRBR4[dd$DMDHRBR4 %in% c(77) | is.na(dd$DMDHRBR4)] <- 0

dd$DMDHREDU <- as.numeric(dd$DMDHREDU)
dd$DMDHREDU[dd$DMDHREDU %in% c(7,9) | is.na(dd$DMDHREDU)] <- 0 

dd$DMQMILIZ <- as.numeric(dd$DMQMILIZ)
dd$DMQMILIZ[dd$DMQMILIZ %in% c(7,9) | is.na(dd$DMQMILIZ)] <- 0 

dd$DMDHRMAR <- as.numeric(dd$DMDHRMAR)
dd$DMDHRMAR[dd$DMDHRMAR %in% c(77,99) | is.na(dd$DMDHRMAR)] <- 0

cols_con_ceros <- c("DMDBORN4", "DMDCITZN", "DMDEDUC2", 
                    "DMDHRBR4", "DMDHREDU", "DMQMILIZ", "DMDHRMAR")

dd <- dd[rowSums(dd[cols_con_ceros] == 0) == 0, ]

dd[categorical_vars] <- lapply(dd[categorical_vars], as.factor)

set.seed(123)  
dd_500 <- dd[sample(nrow(dd), 500), ]
summary(dd_500)

# 3. Clustering
gower_dist <- daisy(dd_500, metric = "gower")
gower_dist_sq <- gower_dist^2
hc_ward <- hclust(gower_dist_sq, method = "ward.D2")

k <- 3
dd_500$cluster <- factor(cutree(hc_ward, k = k))

# 4. Profiling analysis

# Create results dir
pathProfiling <- "profiling_results/"
if (!dir.exists(pathProfiling)) dir.create(pathProfiling)

# Variables to analyze
vars_to_profile <- colnames(dd_500)[colnames(dd_500) != "cluster"]
significant_vars <- c()

sink(file = paste0(pathProfiling, "Test_Significance.txt"))

for (var_name in vars_to_profile) {
  
  current_var <- dd_500[[var_name]]
  
  # --- Numeric variables ---
  if (is.numeric(current_var)) {
    
    # Normality test
    # Shapiro test
    testSH <- shapiro.test(current_var)
    
    if (testSH$p.value > 0.05) {
      # Normal -> ANOVA
      test <- aov(current_var ~ dd_500$cluster)
      p_val <- summary(test)[[1]][["Pr(>F)"]][1]
      test_name <- "ANOVA"
    } else {
      # Not Normal -> Kruskal-Wallis
      test <- kruskal.test(current_var ~ dd_500$cluster)
      p_val <- test$p.value
      test_name <- "Kruskal-Wallis"
    }
    
    cat("\n============ ", var_name, " (", test_name, ") ================\n")
    print(test)
    
    if (p_val <= 0.05) {
      significant_vars <- c(significant_vars, var_name)
      
      # Plots
      gr_Boxplot <- ggboxplot(dd_500, "cluster", var_name, fill = "cluster")
      gr_Hist    <- gghistogram(dd_500, x = var_name, add = "mean", rug = TRUE,
                                color = "cluster", fill = "cluster")
      gr <- ggarrange(gr_Boxplot, gr_Hist, heights = c(2, 0.7), ncol = 2, nrow = 1, align = "v")
      
      ggsave(filename = paste0(pathProfiling, "num_", var_name, ".png"),
             plot = gr, bg = "white", width = 8, height = 4)
    }
  }
  
  # --- Categorical variables ---
  else if (is.factor(current_var) || is.character(current_var)) {
    
    # Check levels
    if (length(unique(current_var)) < 2) {
      cat("\n============ ", var_name, " (Skipped: < 2 levels) ================\n")
      next
    }

    # Chi-squared test
    # Simulate p-value
    test <- chisq.test(current_var, dd_500$cluster, simulate.p.value = TRUE)
    
    cat("\n============ ", var_name, " (Chi-squared) ================\n")
    print(test)
    
    if (test$p.value <= 0.05) {
      significant_vars <- c(significant_vars, var_name)
      
      # Plot
      # Proportions
      df_plot <- dd_500 %>%
        group_by(cluster, !!sym(var_name)) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(cluster) %>%
        mutate(prop = count / sum(count))
      
      gr <- ggplot(df_plot, aes(x = cluster, y = prop, fill = !!sym(var_name))) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = paste("Distribution of", var_name, "by Cluster"), y = "Proportion") +
        theme_minimal()
      
      ggsave(filename = paste0(pathProfiling, "cat_", var_name, ".png"),
             plot = gr, bg = "white", width = 6, height = 4)
    }
  }
}
sink()

# 5. Templates using catdes
# Describe clusters
res_catdes <- catdes(dd_500, num.var = which(names(dd_500) == "cluster"))

sink(file = paste0(pathProfiling, "Cluster_Templates.txt"))
print(res_catdes)
sink()

# Plot catdes results
pdf(paste0(pathProfiling, "catdes_plots.pdf"))
plot(res_catdes, show = "quanti", barplot = TRUE, main = "Quantitative Variables by Cluster")
plot(res_catdes, show = "quali", barplot = TRUE, main = "Qualitative Variables by Cluster")
dev.off()

print("Profiling completed. Results saved in 'profiling_results/' folder.")













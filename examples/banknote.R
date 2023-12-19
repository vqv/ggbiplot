#' ---
#' title: banknote data
#' ---

data(banknote, package= "mclust")
library(ggbiplot)
library(ggplot2)

banknote.pca <- prcomp(banknote[, -1], scale = TRUE)
banknote.pca

ggscreeplot(banknote.pca) +
  theme_bw(base_size = 14) 
  

ggbiplot(banknote.pca,
         obs.scale = 1, var.scale = 1,
         groups = banknote$Status,
         ellipse = TRUE, 
         ellipse.level = 0.5, ellipse.alpha = 0.1, ellipse.linewidth = 0,
         varname.size = 4,
         varname.color = "black") +
  labs(fill = "Status", color = "Status") +
  theme_minimal(base_size = 14) +
  theme(legend.direction = 'horizontal', legend.position = 'top')

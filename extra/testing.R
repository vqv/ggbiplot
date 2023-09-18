#' ---
#' title: test cases for ggbiplot
#' ---

library(ggbiplot)
library(ggplot2)
library(dplyr)

data(wine, package="ggbiplot")
wine.pca <- prcomp(wine, scale. = TRUE)
ggbiplot(wine.pca, groups=wine.class,
         ellipse = TRUE, 
         ellipse.linewidth = 1.1,
         circle = TRUE,
         varname.color = "darkred",
         varname.size = 4) +
  theme_minimal() +
  theme(legend.direction = 'horizontal', legend.position = 'top')

# unfilled
ggbiplot(wine.pca, groups=wine.class,
         ellipse = TRUE, 
         ellipse.linewidth = 1.2, ellipse.fill = FALSE,
         circle = TRUE,
         varname.color = "darkred",
         varname.size = 4) +
  theme_minimal() +
  theme(legend.direction = 'horizontal', legend.position = 'top')



#' USArrests

data("USArrests")
arrests.pca <- 
  prcomp (~ Murder + Assault + UrbanPop + Rape,
          data=USArrests,
          scale. = TRUE)


ggbiplot(arrests.pca,
         labels = state.abb[match(row.names(USArrests), state.name)] ,
         circle = TRUE,
         varname.size = 4,
         varname.color = "red") +
  theme_minimal(base_size = 14) 

ggbiplot(arrests.pca,
         groups = state.region,
         labels = state.abb[match(row.names(USArrests), state.name)],
         labels.size = 4,
         ellipse = TRUE, ellipse.level = 0.5, ellipse.alpha = 0.1,
         circle = TRUE,
         varname.size = 4,
         varname.color = "black") +
  theme_minimal(base_size = 14) +
  theme(legend.direction = 'horizontal', legend.position = 'top')


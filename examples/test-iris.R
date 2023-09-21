#' ---
#' title: test iris data for ggbiplot
#' ---

library(ggbiplot)
library(ggplot2)
library(dplyr)
library(MASS)



data(iris)
iris.pca <- prcomp (~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                    data=iris,
                    scale. = TRUE)

iris.gg <-
  ggbiplot(iris.pca, obs.scale = 1, var.scale = 1,
           groups = iris$Species, point.size=2,
           varname.size = 5, 
           varname.color = "black",
           varname.adjust = 1.2,
           ellipse = TRUE, 
           circle = TRUE) +
  labs(fill = "Species", color = "Species") +
  theme_minimal(base_size = 14) +
  theme(legend.direction = 'horizontal', legend.position = 'top')

#' ## label the ellipses
group.labs <-
  iris.gg$data |>
  summarise(xvar = mean(xvar),
            yvar = mean(yvar), .by = groups)

iris.gg + geom_label(data = group.labs,
                     aes(x = xvar, y=yvar, label=groups),
                     size = 5) +
  theme(legend.position = "none")

# show point labels
ggbiplot(iris.pca, obs.scale = 1, var.scale = 1,
         groups = iris$Species, 
         point.size=2,
         labels = row.names(iris),
         varname.size = 5, 
         varname.color = "black",
         varname.adjust = 1.2,
         ellipse = TRUE, 
         circle = FALSE) +
  labs(fill = "Species", color = "Species") +
  theme_minimal(base_size = 14) +
  theme(legend.direction = 'horizontal', legend.position = 'top')


#' ## Try lda()

iris.lda <- lda(Species ~ ., data = iris)

ggbiplot(iris.lda,
         groups = iris$Species,
         ellipse = TRUE,
         varname.size = 5) +
  labs(fill = "Species", color = "Species") +
  theme_minimal(base_size = 14) +
  theme(legend.direction = 'horizontal', legend.position = 'top')



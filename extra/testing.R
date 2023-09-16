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


#' ## penguins data
#' 
data(penguins, package = "palmerpenguins")
peng <- penguins |>
  rename(
    bill_length = bill_length_mm, 
    bill_depth = bill_depth_mm, 
    flipper_length = flipper_length_mm, 
    body_mass = body_mass_g
  ) |>
  filter(!is.na(bill_depth),
         !is.na(sex))


peng.pca <- prcomp (~ bill_length + bill_depth + flipper_length + body_mass,
                    data=peng,
                    scale. = TRUE)

peng.gg <-
ggbiplot(peng.pca, obs.scale = 1, var.scale = 1,
         groups = peng$species, point.size=2,
         varname.size = 6, 
         varname.color = "black",  #scales::muted("black"),
         ellipse = TRUE, ellipse.linewidth = 1.2,
         circle = TRUE) +
  labs(fill = "Species", color = "Species") +
  theme_minimal(base_size = 14) +
  theme(legend.direction = 'horizontal', legend.position = 'top')

# label the ellipses
group.labs <-
  peng.gg$data |>
  summarise(xvar = mean(xvar),
            yvar = mean(yvar), .by = groups)

peng.gg + geom_label(data = group.labs,
                     aes(x = xvar, y=yvar, label=groups),
                     size = 5) +
  theme(legend.position = "none")




# try reflecting & scaline var vectors
ggbiplot(peng.pca, obs.scale = 1, var.scale = 1,
         var.factor = -1,
         varname.adjust = 1,
         groups = peng$species, point.size=2,
         varname.size = 5, varname.color = scales::muted("black"),
         ellipse = TRUE, ellipse.linewidth = 1.4,
         circle = TRUE) +
  labs(fill = "Species", color = "Species") +
  theme_minimal() +
  theme(legend.direction = 'horizontal', legend.position = 'top')


# last two dimensions: outliers; use observation labels
ggbiplot(peng.pca, obs.scale = 1, var.scale = 1, choices = 3:4,
         groups = peng$species, 
         labels = row.names(peng),
         point.size=2,
         var.factor = 2.1, varname.adjust = 1,
         varname.size = 5, varname.color = scales::muted("red"),
         ellipse = TRUE, ellipse.alpha = 0.1, 
         circle = TRUE) +
  labs(fill = "Species", color = "Species") +
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
  theme_minimal(base_size = 14) +
  theme(legend.direction = 'horizontal', legend.position = 'top')

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


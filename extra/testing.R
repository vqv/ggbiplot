data(wine, package="ggbiplot")
wine.pca <- prcomp(wine, scale. = TRUE)
ggbiplot(wine.pca, groups=wine.class,
         ellipse = TRUE, 
         ellipse.linewidth = 1.2,
         circle = TRUE,
         varname.color = "darkred",
         varname.size = 4) +
  theme_minimal() +
  theme(legend.direction = 'horizontal', legend.position = 'top')


library(dplyr)
library(ggplot2)
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
                    na.action=na.omit,
                    scale. = TRUE)

ggbiplot(peng.pca, obs.scale = 1, var.scale = 1,
         groups = peng$species, point.size=2,
         varname.size = 5, varname.color = scales::muted("black"),
         ellipse = TRUE, ellipse.linewidth = 1.4,
         circle = TRUE) +
#  scale_fill_discrete(name = 'Species') +
  theme_minimal() +
  theme(legend.direction = 'horizontal', legend.position = 'top')

# try reflecting & scaline var vectors
ggbiplot(peng.pca, obs.scale = 1, var.scale = 1,
         var.factor = -1.1,
         groups = peng$species, point.size=2,
         varname.size = 5, varname.color = scales::muted("black"),
         ellipse = TRUE, ellipse.linewidth = 1.4,
         circle = TRUE) +
  #  scale_fill_discrete(name = 'Species') +
  theme_minimal() +
  theme(legend.direction = 'horizontal', legend.position = 'top')


# last two dimensions: outliers
ggbiplot(peng.pca, obs.scale = 1, var.scale = 1, choices = 3:4,
         groups = peng$species, point.size=2,
         varname.size = 5, varname.color = scales::muted("red"),
         ellipse = TRUE, ellipse.linewidth = 1.4,
         circle = TRUE) +
  #  scale_fill_discrete(name = 'Species') +
  theme_minimal() +
  theme(legend.direction = 'horizontal', legend.position = 'top')




library(biplot2d3d)
# uses ade
devtools::install_github("Andros-Spica/biplot2d3d")

# Use iris data
data("iris")

# get an ordination object
# ("PCA" is the default input of this function)
pca <- princomp(iris[, 1:4])

# Default plot using Species as the group factor
biplot_2d(pca, groups = iris$Species)

# Use the typical visualization,
# placing scores and loadings around the same origin
biplot_2d(pca, groups = iris$Species, detach_arrows = FALSE)


# Compare different versions of the classical biplot
# "default" vs. "pc.biplot"
biplot_2d(pca,
          output_type = "preview",
          leave_device_open = TRUE,
          x_title = 'biplot_type = "default"',
          x_title_fig = c(0, 1, 0.9, 1),
          fit_into_main = TRUE,
          main_fig = c(0, 0.499, 0, 1))
biplot_2d(pca,
          output_type = "preview",
          open_new_device = FALSE,
          biplot_type = "pc.biplot",
          x_title = 'biplot_type = "pc.biplot"',
          x_title_fig = c(0, 1, 0.9, 1),
          fit_into_main = TRUE,
          main_fig = c(0.5099, 1, 0, 1))


# ---------------------------------------------------------
# Plot groups as different colors and point types (pch),
# make group star, ellipsis, and label invisible and
# add a group legend with a a custom title.
biplot_2d(pca,
          groups = iris$Species,
          group_color = NULL,
          point_pch = c(1, 3, 2),
          group_star_cex = 0,
          group_ellipse_cex = 0,
          group_label_cex = 0,
          show_group_legend = T,
          group_legend_title = "Species")

# ---------------------------------------------------------
# Polish covariance arrows
# Abbreviate variables names
dimnames(pca$loadings)[[1]] <- c("SL", "SW", "PL", "PW")
# Set a specific justification (adj) for each variable label
arrow_label_adj_override <- rbind(c(-0.1, 0),
                                  c(-0.1, 0.5),
                                  c(0.5, 1.3),
                                  c(0.5, 1.3))
row.names(arrow_label_adj_override) <-
  dimnames(pca$loadings)[[1]]
# Plot: arrows with different colors and
# without the background grid
biplot_2d(pca,
          groups = iris$Species,
          point_pch = c(1, 3, 2),
          group_star_cex = 0,
          group_ellipse_cex = 0,
          group_label_cex = 0,
          show_group_legend = T,
          group_legend_title = "Species",
          arrow_color = c("orange",
                          "blue",
                          "red",
                          "green"),
          arrow_label_adj_override = arrow_label_adj_override,
          show_grid = FALSE)

# ---------------------------------------------------------
# Get arbitrary Very Important Points
irisVIP <- list(setosa = (1:nrow(iris) == 16 |
                            1:nrow(iris) == 42),
                versicolor=(1:nrow(iris) == 61),
                virginica=(1:nrow(iris) == 107 |
                             1:nrow(iris) == 118 |
                             1:nrow(iris) == 132))

# Plot observations using their names and group by Species using only color.
# Mark the VIP and add the respective legend with custom characters.
biplot_2d(pca,
          groups = iris$Species,
          point_type = "label",
          point_label = row.names(iris),
          group_color = c("red", "blue", "green"),
          group_star_cex = 0,
          group_ellipse_cex = 0,
          group_label_cex = 0,
          show_group_legend = TRUE,
          group_legend_title = "",
          vips = irisVIP,
          vip_pch = c("X", "O", "+"),
          vip_cex = c(2, 2, 3),
          vip_legend_fig = c(0.01, 0.25, 0.7, 0.99),
          show_axes = FALSE)

# 3D biplots

# Default plot using Species as the groups
biplot_3d(pca, groups = iris$Species)

biplot_3d(pca,
          groups = iris$Species,
          group_representation = "ellipsoids",
          ellipsoid_label_alpha = 0,
          show_group_legend = TRUE,
          group_legend_title = "",
          arrow_center_pos = c(.5, 0, .5),
          arrow_body_length = 1,
          arrow_body_width = 2,
          view_theta = 0,
          view_zoom = 0.9)

# ---------------------------------------------------------
# Plot observations using their names and groups as
# stars but adding a legend instead of labels.
# Modify the aspect to normalize the variability
# of axes and do not show them. Zoom out a little.
biplot_3d(pca, groups = iris$Species,
          point_type = "label", 
          point_label = row.names(iris),
          star_label_alpha = 0,
          show_group_legend = TRUE, 
          group_legend_title = "",
          arrow_center_pos = c(.5, 0, .5),
          arrow_body_length = 2, 
          arrow_body_width = 2,
          show_axes = FALSE, view_zoom = 1)



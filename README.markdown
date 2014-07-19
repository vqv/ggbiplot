ggbiplot
========

This is experimental branch of ggbiplot is a complete overhaul of the existing 
ggbiplot package.  It adds several new geoms and fortify methods that allow 
biplots to be constructed in a more ggplot2-like manner.  It is currently 
under development so its documentation is sparse and the interface and 
behavior are likely to change.  

Installation
------------
```R
library(devtools)
install_github("ggbiplot", "vqv", ref = "experimental")
```

The package has several dependencies, including version >= 1.0.0 of ggplot2.  Check the DESCRIPTION file and make sure you have the correct versions of the dependencies installed.

Example Usage
-------------
```R
data(wine)
cultivar <- wine.class
m <- prcomp(wine, scale = TRUE)
df <- fortify(m, wine, scale = 1, equalize = FALSE)

# Basic principal components plot with ellipses
g <- ggplot(df, aes(x = PC1, y = PC2)) +
  geom_point(aes(group = cultivar, color = cultivar, shape = cultivar)) + 
  stat_ellipse(aes(group = cultivar, color = cultivar))

# Biplot with correlation circle
g <- g + geom_axis(data = attr(df, "basis"), aes(label = .name)) + 
  annotate("circle", x = 0, y = 0, radius = 1, alpha = 1/4)

# Some tweaks to the layout
g <- g + scale_color_discrete(name = '')
g <- g + opts(legend.direction = 'horizontal', legend.position = 'top')

print(g)
```

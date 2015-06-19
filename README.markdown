# ggbiplot

This experimental branch of ggbiplot is a complete overhaul of the existing 
ggbiplot package.  It adds several new geoms and fortify methods that allow 
biplots to be constructed in a more ggplot2-like manner.  It is currently 
under development so its documentation is sparse and the interface and 
behavior are likely to change.  

## Installation
```R
library(devtools)
install_github("vqv/ggbiplot", ref = "experimental")
```

The package has several dependencies, including version >= 1.0.0 of ggplot2.  Check the DESCRIPTION file and make sure you have the correct versions of the dependencies installed.

## Design
The new design factors the functionality of the original ggbiplot() function into separate parts.  The basic scheme for constructing a biplot is as follows:

1. Compute a linear dimension reduction such as principal components analysis (PCA) or linear discriminant analysis using functions such as `prcomp()` in base R or `lda()` in MASS.
2. Convert the dimension reduction object into a data frame that can be used for plotting using the `fortify()` method.  This method returns a data frame containing the projections of the data (scores) with the attribute `basis` set equal to a matrix the loadings (or basis vectors for the projection).
3. Use `ggplot2` to produce a basic plot, e.g. a principal components score plot, from the fortified dimension reduction object.
4. Promote the basic plot to biplot by adding an additional biplot axes layer using `geom_axis()` (provided by ggbiplot).
5. Additional embellishments such as circles and ellipses can be added using `geom_circle()` (provided by ggbiplot) and `stat_ellipse()` (provided by ggplot versions >=1.0.0).


## Examples
The folowing examples are based on correlation PCA of the `wine` dataset that is included with `ggbiplot`.
```R
data(wine)
cultivar <- wine.class
m <- prcomp(wine, scale = TRUE)
```

### Basic principal components score plot
```R
qplot(PC1, PC2, data = fortify(m), color = cultivar)
```

### Principal components score plot with ellipses for groups
```R
qplot(PC1, PC2, data = fortify(m), color = cultivar) + 
  stat_ellipse(aes(group = cultivar))
```

### Basic principal components biplot
```R
df <- fortify(m)
g <- ggplot(df, aes(x = PC1, PC2)) + 
  geom_point(aes(color = cultivar)) +
  geom_axis(data = attr(df, "basis"), aes(label = .name))
print(g)
```
In this example, the fortified object has to be stored because we need 
access to two different data frames: one containing the scores and one containing the loadings.  By default `fortify()` does not scale the 
loadings.  So the arrows in the above biplot correspond exactly to the 
rows of the loadings matrix.  Another way to think of them is that they are 
the projections of unit variables, e.g. an observation consisting of a 
one in coordinate (variable) and zeroes everywhere else.  This scaling is 
useful because it results in a plot where the scores and loadings vectors 
are on the **same scale**.

### Correlation principal components biplot
```R
df <- fortify(m, scale = 1, equalize = FALSE)
g <- ggplot(df, aes(x = PC1, PC2)) + 
  geom_point(aes(color = cultivar)) +
  geom_axis(data = attr(df, "basis"), aes(label = .name)) + 
  annotate("circle", x = 0, y = 0, radius = 1, alpha = 1/4)
print(g)
```
This example reproduces a "traditional" biplot where the scores are 
standardized and the loadings are scaled up by the inverse of the standardization. In the call to `fortify()`,  the argument `scale = 1` works similarly to the `scale` parameter in `biplot.princomp()` from base R.  
By default, `fortify()` will rescale the lengths of the loadings vectors so 
that they are comparable to the lengths of the score vectors whenever `scale != 1`. The argument `equalize = FALSE` ensures that this does not happen.

### Correlation biplot with ellipses and additional tweaks
```R
df <- fortify(m, scale = 1, equalize = FALSE)
g <- ggplot(df, aes(x = PC1, PC2)) + 
  geom_point(aes(color = cultivar)) +
  stat_ellipse(aes(group = cultivar, color = cultivar))
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
g <- g + coord_equal()

g1 <- g + geom_axis(data = attr(df, "basis"), aes(label = .name)) + 
  annotate("circle", x = 0, y = 0, radius = 1, alpha = 1/4)

print(g1)
```

### Thresholded correlation biplot with rescaled biplot axes
The following example continues the previous. We will 
manually scale the biplot axes by scaling the loadings and 
threshold variables with small loadings.
```R
g2 <- g + geom_axis(data = subset(attr(df, "basis"), PC1^2 + PC2^2 > 1/3), 
                    aes(PC1 * 1.1, PC2 * 1.1, label = .name)) +
  annotate("circle", x = 0, y = 0, radius = 1.1, alpha = 1/4)

print(g2)
```

### Fisher's iris data
```R
library(MASS)
data(iris)

# Standardize numeric variables and apply LDA
iris_z <- lapply(iris, function(x) if (is.numeric(x)) scale(x) else x)
m <- lda(Species ~ ., data = iris_z)
df <- fortify(m, iris_z)

g <- ggplot(df, aes(x = LD1, y = LD2)) +
  geom_point(aes(color = Species)) + 
  stat_ellipse(aes(group = Species, color = Species)) +
  geom_axis(data = attr(df, "basis"), aes(label = abbreviate(.name))) + 
  ylim(-4, 4) + coord_equal()

print(g)
```

### USArrests
```R
data(USArrests)
data(state)
m <- princomp(USArrests)
df <- fortify(m, scale = 1)

g <- ggplot(df, aes(x = PC1, y = PC2)) +
  geom_text(aes(label = state.abb[match(rownames(df),state.name)])) +
  geom_axis(data = attr(df, "basis"), aes(label = .name))
print(g)
```
Compare the above with
```R
example(biplot.princomp)
````

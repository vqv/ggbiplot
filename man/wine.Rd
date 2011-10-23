\name{wine}
\alias{wine}
\docType{data}
\title{
Chemical composition of three cultivars of wine
}
\description{
Chemical constituents of wines from three different cultivars grown in the same region in Italy.  The cultivars,
'barolo', 'barbera', and 'grignolino', are indicated in wine.class.
}
\usage{data(wine)}
\format{
  The format is:
 chr "wine"
}
\source{
http://archive.ics.uci.edu/ml/datasets/Wine
}
\examples{
data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
print(ggscreeplot(wine.pca))                                               
print(ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = wine.class, ellipse = TRUE, circle = TRUE))
}
\keyword{datasets}

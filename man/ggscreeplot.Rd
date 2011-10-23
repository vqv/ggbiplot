\name{ggscreeplot}
\alias{ggscreeplot}
\title{Screeplot for Principal Components}
\usage{
  ggscreeplot(pcobj, type = c("pev", "cev"))
}
\arguments{
  \item{pcobj}{an object returned by prcomp() or
  princomp()}

  \item{type}{the type of scree plot.  'pev' corresponds
  proportion of explained variance, i.e. the eigenvalues
  divided by the trace. 'cev' corresponds to the cumulative
  proportion of explained variance, i.e. the partial sum of
  the first k eigenvalues divided by the trace.}
}
\description{
  Screeplot for Principal Components
}
\examples{
data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
print(ggscreeplot(wine.pca))
}


library(biplotEZ)


biplot (iris[,1:4]) |> 
  PCA(group.aes=iris[,5]) |> 
  concentration.ellipse(kappa=2) |> 
  plot()


biplot(iris[,1:4],iris[,5]) |> 
  CVA() |> plot()

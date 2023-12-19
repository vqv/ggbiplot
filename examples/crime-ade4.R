#' ---
#' title: crime data - ade4
#' ---

library(ggplot2)
library(ggbiplot)
library(dplyr)
library(ade4)
library(adegraphics)

data(crime)

crime.dudi <- 
  crime |> 
  dplyr::select(where(is.numeric)) |>
  dudi.pca(scannf = FALSE, nf = 7)

ggbiplot(crime.dudi,
         obs.scale = 1, var.scale = 1,
         labels = crime$st ,
         circle = TRUE,
         varname.size = 4,
         varname.color = "red",
         axis.title = "Dimension ") +
  theme_minimal(base_size = 14) 


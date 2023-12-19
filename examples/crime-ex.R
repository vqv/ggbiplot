#' ---
#' title: crime data
#' ---

library(ggplot2)
library(ggbiplot)
library(dplyr)
library(corrplot)
library(FactoMineR)
library(factoextra)

data(crime)

crime |> 
  dplyr::select(where(is.numeric)) |> 
  cor() |> 
  corrplot(method = "ellipse", tl.srt = 0)

crime.pca <- 
  crime |> 
  dplyr::select(where(is.numeric)) |>
  prcomp(scale. = TRUE)

biplot(crime.pca)

crime.pca <- reflect(crime.pca)

#biplot(crime.pca)


# default scaling: standardized components
ggbiplot(crime.pca,
         labels = crime$st ,
         circle = TRUE,
         varname.size = 4,
         varname.color = "red") +
  theme_minimal(base_size = 14) 

ggbiplot(crime.pca,
         obs.scale = 1, var.scale = 1,
         labels = crime$st ,
         circle = TRUE,
         varname.size = 4,
         varname.color = "red") +
  theme_minimal(base_size = 14) 

# regions as groups, with ellipses
ggbiplot(crime.pca,
         groups = crime$region,
         labels = crime$st,
         labels.size = 4,
         var.factor = 1.4,
         ellipse = TRUE, ellipse.level = 0.5, ellipse.alpha = 0.1,
         circle = TRUE,
         varname.size = 4,
         varname.color = "black") +
  labs(fill = "Region", color = "Region") +
  theme_minimal(base_size = 14) +
  theme(legend.direction = 'horizontal', legend.position = 'top')



crime.PCA <- 
  crime |> 
  dplyr::select(where(is.numeric)) |>
  PCA()

crime.PCA$var$cor

# -----------------------------
# Supplementary variables
supp_data <- state.x77 |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "state") |>
  select(state, Income:`Life Exp`, `HS Grad`) |>
  rename(Life_Exp = `Life Exp`,
         HS_Grad = `HS Grad`)

crime_joined <-
  dplyr::left_join(crime[, 1:8], supp_data, by = "state")
names(crime_joined)

row.names(crime_joined) <- crime$st
crime.PCA_sup <- PCA(crime_joined[,c(2:8, 9:12)], 
                     quanti.sup = 8:11,
                     scale.unit=TRUE, 
                     ncp=3, 
                     graph = FALSE)

crime.PCA_sup$var$coord
crime.PCA_sup$quanti.sup$coord

# reflect Dim 2
crime.PCA_sup <- ggbiplot::reflect(crime.PCA_sup, columns = 2)

crime.PCA_sup$var$coord
crime.PCA_sup$quanti.sup$coord

## NB: This is now handled in ggbiplot
crime.PCA_sup$quanti.sup$coord[, 2] <- -1 * crime.PCA_sup$quanti.sup$coord[, 2]

plot(crime.PCA_sup, choix = "var")


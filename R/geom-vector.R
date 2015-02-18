GeomVector <- proto(ggplot2:::Geom, {
  objname <- "vector"

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(
    xbegin = 0, 
    ybegin = 0, 
    colour = "black", 
    size = 0.5, 
    linetype = 1, 
    alpha = NA
  )
  guide_geom <- function(.) "segment"

  draw <- function(., data, scales, coordinates, 
    arrow = grid::arrow(length = unit(1/3, "picas")), ...) {

    if (empty(data)) return(zeroGrob())

    # Drop rows where (x, y) is nearly equal to (xbegin, ybegin)
    r2 <- with(data, (x - xbegin)^2 + (y - ybegin)^2)
    data <- subset(data, Vectorize(all.equal)(r2, 0) != TRUE)

    segment <- transform(data, 
      xend = x, yend = y, x = xbegin, y = ybegin,
      xbegin = NULL, ybegin = NULL
    )

    GeomSegment$draw(segment, scales, coordinates, arrow = arrow)
  }
})

#' Arrows
#'
#' @param arrow specification for arrow heads, as created by arrow()
#' @export
geom_vector <- function (mapping = NULL, data = NULL, stat = "identity",
  position = "identity", arrow = grid::arrow(length = unit(1/3, "picas")), ...) {

  GeomVector$new(mapping = mapping, data = data, stat = stat,
    position = position, arrow = arrow, ...)
}
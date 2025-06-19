if (requireNamespace("RColorBrewer")) {
  library("RColorBrewer")
  n <- 250
  x <- seq(-10, 10, length = n)
  y <- x
  f <- function(x, y) {
    r <- sqrt(x^2 + y^2)
    10 * sin(r) / r
  }
  z <- outer(x, y, f)
  z[is.na(z)] <- 1
  x <- matrix(x, nrow = n, ncol = n)
  y <- matrix(y, nrow = n, ncol = n, byrow = TRUE)
  sombrero <- data.frame(x = as.vector(x), y = as.vector(y), z = as.vector(z))
  class_colors <- brewer.pal(7, "Set2")[cut(1:nrow(sombrero), 7, label = FALSE)]
  scatterplot3d(sombrero, color = class_colors, pch = ".")
}

## numeric SOM example with demo(numeric)
## korresp SOM example (for contingency tables) with demo(korresp)
## relational SOM example (for dissimilarity data) with demo(relational)
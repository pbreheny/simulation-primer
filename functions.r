library(data.table)
library(ggplot2)
library(magrittr)

generate <- function(n1, n2, sd1, sd2, contamination, delta, ...) {
  x <- rnorm(n1, 0, sd=sd1^2)
  y <- rnorm(n2, delta, sd=sd2^2)
  list(x=x, y=y)
}

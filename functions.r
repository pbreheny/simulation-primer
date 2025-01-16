library(data.table)
library(ggplot2)
library(magrittr)

generate <- function(n1, n2, s1, s2, contamination, delta) {
  x <- rnorm(n1, 0, sd=s1^2)
  y <- rnorm(n2, delta, sd=s2^2)
  list(x=x, y=y)
}

width <- function(out) {
  out$conf.int[2] - out$conf.int[1]
}

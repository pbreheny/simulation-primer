#' Evaluate power and type 1 error rate for the t-test when variances are unequal
#' total sample size is always 20

source('functions.r')

# Define options
opt <- list(
  nsim = 1000,
  s1 = 1,
  s2 = 1:3,
  n1 = seq(2, 18, 2),
  mu = 1)

# Set up results frame
res <- expand.grid(
  rep = 1:opt$nsim,
  delta = c(0, opt$mu),
  s1 = opt$s1,
  s2 = opt$s2,
  n1 = opt$n1,
  p = NA_real_,
  width = NA_real_)

# Loop
pb <- txtProgressBar(0, nrow(res), style=3)
for (i in 1:nrow(res)) {
  # Generate
  dat <- generate(
    n1 = res$n1[i],
    n2 = 20 - res$n1[i],
    s1 = res$s1[i],
    s2 = res$s2[i],
    contamination = res$contamination[i],
    delta = res$delta[i])
  
  # Analyze
  out <- t.test(dat$x, dat$y, var.equal = TRUE)
  
  # Summarize
  res$p[i] <- out$p.value
  res$width[i] <- width(out)
  
  setTxtProgressBar(pb, i)
}

# Save
res <- as.data.table(res)
write.csv(res, 'res-1.csv')

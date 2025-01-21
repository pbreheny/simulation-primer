#' Evaluate power and type 1 error rate for the t-test when
#' variances are unequal total sample size is always 20

source('functions.r')

# Define options
opt <- list(
  equal_var = c(TRUE, FALSE),                                         # <1>
  rep = 1:1000,                                                       # <1> 
  sd1 = 1,
  sd2 = 1:3,
  n1 = seq(2, 18, 2),
  delta = c(0, 1))

# Set up results frame                                                
res <- expand.grid(opt)
res$n2 <- 20 - res$n1
res$p = NA_real_
res$width = NA_real_

# Loop
pb <- txtProgressBar(0, nrow(res), style=3)
for (i in 1:nrow(res)) {
  # Generate                                                          # <2> 
  if (i == 1 || res$rep[i] != res$rep[i-1]) {                         # <2> 
    dat <- do.call(generate, res[i,])                                 # <2> 
  }                                                                   # <2>
                                                                      
  # Analyze                                                           
  out <- t.test(dat$x, dat$y, var.equal = res$equal_var[i])           # <3>
                                                                      
  # Summarize                                                         
  res$p[i] <- out$p.value                                             
  res$width[i] <-   out$conf.int[2] - out$conf.int[1]                 
                                                                      
  setTxtProgressBar(pb, i)                                            
}                                                                     
close(pb)

# Save
write.csv(res, 'ex-2.csv', row.names = FALSE)                         

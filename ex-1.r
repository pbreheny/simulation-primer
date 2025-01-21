#' Evaluate power and type 1 error rate for the t-test when           # <1> 
#' variances are unequal total sample size is always 20               # <1> 

source('functions.r')                                                 # <2>

# Define options                                                      # <3> 
opt <- list(                                                          # <3> 
  rep = 1:1000,                                                       # <3> 
  sd1 = 1,                                                            # <3> 
  sd2 = 1:3,                                                          # <3> 
  n1 = seq(2, 18, 2),                                                 # <3> 
  delta = c(0, 1))                                                    # <3> 

# Set up results frame                                                
res <- expand.grid(opt)                                               # <4> 
res$n2 <- 20 - res$n1                                                 # <5>
res$p = NA_real_                                                      # <6> 
res$width = NA_real_                                                  # <6> 

# Loop
pb <- txtProgressBar(0, nrow(res), style=3)                           # <7> 
for (i in 1:nrow(res)) {                                              # <8> 
  # Generate                                                          # <8> 
  dat <- do.call(generate, res[i,])                                   # <8> 
                                                                      # <8> 
  # Analyze                                                           # <8> 
  out <- t.test(dat$x, dat$y, var.equal = TRUE)                       # <8> 
                                                                      # <8> 
  # Summarize                                                         # <8> 
  res$p[i] <- out$p.value                                             # <8> 
  res$width[i] <-   out$conf.int[2] - out$conf.int[1]                 # <8> 
                                                                      # <8> 
  setTxtProgressBar(pb, i)                                            # <8> 
}                                                                     # <8>
close(pb)

# Save
write.csv(res, 'ex-1.csv', row.names = FALSE)                         # <9>

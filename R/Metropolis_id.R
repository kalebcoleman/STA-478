metropolis_1d <- function(target_pdf, proposal, start, n, seed = NULL) {

  chain <- numeric(n)   # storage for samples
  x <- start            # initialize chain at starting value
  fx <- target_pdf(x)   # evaluate target pdf at start

  # MCMC loop
  for (t in seq_len(n)) {

    # a) Propose step
    y <- proposal(x)    # propose new candidate
    fy <- target_pdf(y) # evaluate target pdf at candidate

    # acceptance probability alpha
    alpha <- if (fx == 0) 1 else min(1, fy / fx)

    # accept or reject
    if (runif(1) < alpha) {
      x <- y; fx <- fy
    }
    chain[t] <- x       # record sample
  }
  return(chain)
}

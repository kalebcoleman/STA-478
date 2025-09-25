# Function takes a target PDF, the domain, and a sample size
AcceptReject <- function(target_pdf, domain, n) {

  # Step 1: extract domain bounds
  a <- domain[1]
  b <- domain[2]

  # Step 2: approximate maximum of target pdf on [a,b]
  # checking the pdf at 1000 different points
  grid <- seq(a, b, length.out = 1000)
  M <- max(target_pdf(grid))

  # step 3: initialize output vector
  out <- numeric(n)
  i <- 1

  # step 4: loop until we have n accepted samples
  while (i <= n) {

    # propose X from Uniform(a,b)
    x <- runif(1, a, b)

    # propose Y from Uniform(0,M)
    y <- runif(1, 0, M)

    # accept if the point (x,y) falls under the curve of the pdf
    if (y <= target_pdf(x)) {
      out[i] <- x
      i <- i + 1
    }
    # else reject and try again
  }

  return(out)
}

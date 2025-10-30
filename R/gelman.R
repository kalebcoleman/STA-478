#' Gelman-Rubin Convergence Diagnostic
#'
#' Computes the Gelman-Rubin statistic and related variance estimates for a set of
#' MCMC chains, mirroring the diagnostic coded in Assignment 6.
#'
#' @param results Numeric matrix or array where rows correspond to iterations and
#'   columns to chains.
#'
#' @return A one-row data frame with columns `W`, `B`, `sigma2.hat`, `R.hat`, and
#'   `n.eff`.
#'
#' @examples
#' sims <- cbind(rnorm(1000), rnorm(1000, 0.1))
#' Gelman(sims)
#'
#' @export
Gelman <- function(results) {

  # chain dimensions
  n <- dim(results)[1]   # steps per chain
  m <- dim(results)[2]   # number of chains

  # Within-chain variance W
  chain_vars <- apply(results, 2, var)   # per-chain variances
  W <- mean(chain_vars)                  # within-chain variance

  # Between-chain variance B
  chain_means <- apply(results, 2, mean) # per-chain means
  grand_mean <- mean(results)            # global mean
  B <- (n / (m - 1)) * sum((chain_means - grand_mean)^2)  # between-chain variance

  # Weighted variance estimate sigma2.hat
  sigma2.hat <- ((n - 1) / n) * W + (1 / n) * B

  # Gelman ratio R.hat
  R.hat <- sqrt(sigma2.hat / W)

  # Effective sample size n.eff
  n.eff <- m * n * sigma2.hat / B

  # put all stats into a data frame
  out <- data.frame(W = W, B = B,
                    sigma2.hat = sigma2.hat,
                    R.hat = R.hat,
                    n.eff = n.eff)
  return(out)
}

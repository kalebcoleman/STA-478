#' Metropolis Random-Walk Sampler (1D)
#'
#' Runs a one-dimensional Metropolis algorithm using user-supplied target and proposal
#' functions, matching the sampler developed in STA 478 activities.
#'
#' @param target_pdf Function returning the (possibly unnormalised) target density.
#' @param proposal Function that draws a proposal given the current state.
#' @param start Numeric starting value for the chain.
#' @param n Integer number of MCMC iterations.
#' @param seed Optional integer for reproducibility.
#'
#' @details
#' The Metropolis algorithm targets a density \eqn{f(x)} by proposing
#' \eqn{y \sim q(y \mid x)} and accepting with probability
#' \deqn{\alpha(x,y) = \min\left(1, \frac{f(y)}{f(x)}\right)}
#' for a symmetric proposal. Accepted proposals update the chain; rejections
#' repeat the current state, producing a Markov chain with stationary density
#' proportional to \eqn{f(x)}.
#'
#' @return Numeric vector of length `n` containing the simulated chain.
#'
#' @examples
#' target <- function(x) dnorm(x)
#' proposal <- function(x) rnorm(1, mean = x, sd = 0.5)
#' chain <- metropolis_1d(target, proposal, start = 0, n = 1000, seed = 1)
#'
#' @export
metropolis_1d <- function(target_pdf, proposal, start, n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

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

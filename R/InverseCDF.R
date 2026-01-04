#' Inverse CDF Sampling
#'
#' Generates random draws by transforming uniform samples through a supplied inverse
#' CDF, as demonstrated in Assignment 6 notes.
#'
#' @param iCDF Function that maps probabilities in (0, 1) to quantiles.
#' @param n Integer number of samples to generate.
#' @param ... Additional arguments passed to `iCDF`.
#'
#' @details
#' If \eqn{U \sim \mathrm{Unif}(0,1)} and \eqn{X = F^{-1}(U)}, then \eqn{X} has CDF
#' \eqn{F}. This helper generates uniforms and applies the supplied inverse CDF
#' to obtain samples from the target distribution.
#'
#' @return Numeric vector of simulated values.
#'
#' @examples
#' inv_exp <- function(u, rate) -log(1 - u) / rate
#' draws <- Inverse.CDF(inv_exp, n = 1000, rate = 2)
#'
#' @export
Inverse.CDF <- function(iCDF, n, ...) {

  # step 1: draw uniforms U ~ Unif(0,1)
  u <- runif(n)

  # step 2: apply the inverse CDF
  # uses the given inverse CDF from the function
  # to test I use the formula from the notes: F^{-1}(u) = -log(1 - u)/rate

  # Step 3: return results
  return(iCDF(u, ...))
}

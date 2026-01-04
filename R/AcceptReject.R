#' Accept-Reject Sampling
#'
#' Draws samples from a target density on a bounded interval using the accept-reject
#' algorithm with a uniform proposal. The function matches the Assignment 6 workflow
#' where samples were generated under a user-specified PDF.
#'
#' @param target_pdf Function that evaluates the target density at numeric inputs.
#' @param domain Numeric vector of length two giving the lower and upper bounds.
#' @param n Integer number of samples to generate.
#'
#' @details
#' The accept-reject method uses a proposal density \eqn{g(x)} and constant
#' \eqn{M} such that \eqn{f(x) \le M g(x)} on the domain. With a uniform proposal
#' on \eqn{[a,b]}, the acceptance probability at proposal \eqn{x} is
#' \deqn{\alpha(x) = \frac{f(x)}{M g(x)}.}
#' This implementation estimates \eqn{M} from a grid over the domain and accepts
#' when a uniform draw falls below the target density.
#'
#' @return Numeric vector of length `n` containing accepted samples.
#'
#' @examples
#' target <- function(x) dbeta(x, 2, 5)
#' samples <- AcceptReject(target, c(0, 1), 1000)
#'
#' @export
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

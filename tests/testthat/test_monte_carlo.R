# Validate accept-reject sampler returns the right number of bounded samples
test_that("AcceptReject generates samples within the target domain", {
  # Define the target density as a Beta(2,5) PDF on [0,1]
  target <- function(x) dbeta(x, 2, 5)
  # Lock the RNG so the sampler is reproducible
  set.seed(42)
  # Draw 100 samples using the accept-reject helper
  draws <- AcceptReject(target, c(0, 1), n = 100)
  # Confirm the sampler produced the requested number of draws
  expect_length(draws, 100)
  # Ensure every sample lies inside the specified domain
  expect_true(all(draws >= 0 & draws <= 1))
})

# Ensure inverse-CDF sampler aligns with the closed-form exponential quantile
test_that("Inverse.CDF matches analytical inverse for the exponential distribution", {
  # Fix the seed so both paths use identical uniforms
  set.seed(99)
  # Sample using the package helper
  simulated <- Inverse.CDF(function(u, rate) -log(1 - u) / rate, n = 1000, rate = 2)
  # Rewind the RNG to replay the same uniforms manually
  set.seed(99)
  uniforms <- runif(1000)
  # Apply the known exponential inverse CDF to the stored uniforms
  expected <- -log(1 - uniforms) / 2
  # The two approaches should align to numerical tolerance
  expect_equal(simulated, expected)
})

# Quick sanity check for the Metropolis sampler on a standard normal target
test_that("metropolis_1d produces a reasonable chain for a normal target", {
  # Target is the standard normal density
  target <- function(x) dnorm(x)
  # Proposal is a Gaussian random walk around the current state
  proposal <- function(x) rnorm(1, mean = x, sd = 0.5)
  # Run a short chain with a fixed seed for stability
  set.seed(7)
  chain <- metropolis_1d(target, proposal, start = 0, n = 2000)
  # The chain length should equal the requested number of iterations
  expect_length(chain, 2000)
  # All sampled values must remain finite
  expect_true(all(is.finite(chain)))
  # The empirical mean should stay near zero, allowing for Monte Carlo noise
  expect_lt(abs(mean(chain)), 0.25)
})

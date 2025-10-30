# Smoke test for bootstrap_performance on a logistic regression
test_that("bootstrap_performance returns bounded error rates", {
  # Fix the RNG for reproducibility of bootstrap resamples
  set.seed(99)
  # Run a light bootstrap with 50 replicates on the transmission outcome
  res <- bootstrap_performance(
    am ~ wt + hp,                     # logistic model relating auto transmission to weight and horsepower
    data = mtcars,                    # training data for the bootstrap draws
    B = 50,                           # number of bootstrap iterations for the smoke test
    seed = 123                        # seed forwarded to the helper for clarity
  )

  # Error summaries should be numeric and within [0, 1]
  expect_true(is.numeric(res$mean_error))
  expect_true(is.numeric(res$sd_error))
  expect_gte(res$mean_error, 0)
  expect_lte(res$mean_error, 1)
  expect_true(all(res$distribution$error >= 0))
  expect_true(all(res$distribution$error <= 1))
  # Skipped count should be integer and small
  expect_true(res$skipped >= 0)
})

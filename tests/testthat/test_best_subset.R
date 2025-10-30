# Confirm best_subset_search enumerates the expected combinations
test_that("best_subset_search evaluates all subsets up to the size limit", {
  # Fit subsets for mpg using three predictors to keep the grid manageable
  res <- best_subset_search(
    mpg ~ wt + hp + qsec,            # response and three candidate predictors
    data = mtcars,                   # classic vehicle data set from R
    max_predictors = 2,              # only evaluate one- and two-variable models
    k = 2,                           # use two-fold cross-validation for speed
    repeats = 1,                     # perform a single repeat of the folds
    seed = 1                         # lock RNG so results are reproducible
  )
  # Extract the summary table of mean scores
  summary <- res$summary
  # Extract the per-fold scoring details
  details <- res$details

  # choose(3,1) + choose(3,2) = 6 subsets should be evaluated
  expect_equal(nrow(summary), 6)
  # Each subset size should indicate either one or two predictors
  expect_true(all(summary$size %in% c(1, 2)))
  # The reported mean scores must be finite numbers
  expect_true(all(is.finite(summary$mean_score)))
  # Standard deviations must also be finite (no missing folds)
  expect_true(all(is.finite(summary$sd_score)))
  # The detailed fold results should cover exactly the same subset names
  expect_setequal(unique(details$subset), summary$subset)
})

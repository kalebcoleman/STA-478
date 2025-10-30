# Confirm VIF helper refuses single-column input because the regression is undefined
test_that("compute_vif flags insufficient predictors", {
  # A single predictor should raise an error informing the user
  expect_error(compute_vif(data.frame(x = 1:5)), "at least two predictor columns")
})

# Ensure the VIF utilities detect and remove perfectly collinear predictors
test_that("compute_vif and stepwise_vif behave on collinear data", {
  # Build a toy dataset where y is an exact multiple of x to force collinearity
  df <- data.frame(
    x = 1:5,              # baseline predictor
    y = 2 * (1:5),        # perfectly collinear with x
    z = c(5, 4, 3, 2, 1)  # independent column that should remain
  )
  # VIFs should explode for the collinear variables
  vifs <- suppressWarnings(compute_vif(df))
  expect_true(is.infinite(vifs["x"]) || is.infinite(vifs["y"]))

  # Stepwise VIF should drop one of the redundant columns while keeping useful ones
  step <- suppressWarnings(stepwise_vif(df, threshold = 5))
  expect_true(length(step$kept) <= 2)
  expect_true(any(c("x", "y") %in% step$removed))
})

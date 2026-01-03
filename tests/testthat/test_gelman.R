# Validate Gelman-Rubin diagnostic returns reasonable convergence metrics
test_that("Gelman reports near-1 R-hat for similar chains", {
  set.seed(101)
  draws <- cbind(rnorm(1000, 0, 1), rnorm(1000, 0, 1))
  diag <- Gelman(draws)

  expect_true(is.data.frame(diag))
  expect_true(all(c("W", "B", "sigma2.hat", "R.hat", "n.eff") %in% names(diag)))
  expect_true(is.finite(diag$R.hat))
  expect_lt(abs(diag$R.hat - 1), 0.1)
  expect_true(diag$n.eff > 0)
})

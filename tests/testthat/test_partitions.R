# Confirm the train/test splitter mirrors the manual assignment used in class
test_that("train_test_split respects proportion", {
  # Fix the RNG so the draw is reproducible across test runs
  set.seed(123)
  # Request a 60% training sample from the classic mtcars data
  parts <- train_test_split(mtcars, prop = 0.6, seed = 99)
  # Training rows should count to the expected rounded-down size
  expect_equal(nrow(parts$train), floor(0.6 * nrow(mtcars)))
  # The stored row indices must align with the number of training rows
  expect_equal(length(parts$index$train), nrow(parts$train))
  # Combining the train and test indices should cover every original row exactly once
  expect_setequal(
    c(parts$index$train, parts$index$test),
    seq_len(nrow(mtcars))
  )
})

# Verify the classification report computes standard metrics correctly
test_that("cv_classification_report returns expected metrics", {
  # Define ground-truth factor labels from the Weekly LOOCV example
  truth <- factor(c("Up", "Down", "Up", "Down"))
  # Provide predictions that intentionally include mistakes
  preds <- factor(c("Up", "Up", "Down", "Down"), levels = levels(truth))
  # Generate the classification report using "Up" as the positive level
  report <- cv_classification_report(preds, truth, positive = "Up")

  # Check accuracy equals the expected 50%
  expect_equal(report$metrics$accuracy, 0.5)
  # Confirm the error rate complements accuracy
  expect_equal(report$metrics$error_rate, 0.5)
  # Sensitivity should be 0.5 given one true positive and one false negative
  expect_equal(report$metrics$sensitivity, 0.5)
  # Specificity should also be 0.5 with one true negative and one false positive
  expect_equal(report$metrics$specificity, 0.5)
  # Precision mirrors the above counts at 0.5
  expect_equal(report$metrics$precision, 0.5)
  # F1 combines precision and recall into the same 0.5 score
  expect_equal(report$metrics$f1, 0.5)
  # Confirms the confusion matrix accounts for each observation
  expect_identical(sum(report$confusion), length(truth))
})

# Ensure the helper refuses mismatched prediction and truth lengths
test_that("cv_classification_report validates input lengths", {
  # Passing unequal-length vectors should trigger an informative error
  expect_error(
    cv_classification_report(c("A", "B"), c("A", "B", "A")),
    "must have the same length"
  )
})

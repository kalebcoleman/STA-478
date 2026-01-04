#' Classification Report
#'
#' Produces a confusion matrix and common scalar metrics from cross-validation
#' predictions, encapsulating the LOOCV summary workflow from Assignment 6.
#'
#' @param predictions Vector of predicted class labels.
#' @param truth Vector of true class labels.
#' @param positive Optional string naming the positive class for binary metrics.
#'
#' @details
#' The confusion matrix counts predictions versus truth. Accuracy is
#' \deqn{\frac{TP + TN}{TP + TN + FP + FN}.}
#' For binary outcomes, sensitivity (recall) is
#' \deqn{\frac{TP}{TP + FN},}
#' specificity is
#' \deqn{\frac{TN}{TN + FP},}
#' precision is
#' \deqn{\frac{TP}{TP + FP},}
#' and the F1 score is
#' \deqn{2 \cdot \frac{\mathrm{precision} \cdot \mathrm{recall}}
#' {\mathrm{precision} + \mathrm{recall}}.}
#'
#' @return List with elements `confusion` (table) and `metrics` (named list).
#'
#' @examples
#' truth <- factor(c("Up", "Down", "Up", "Down"))
#' preds <- factor(c("Up", "Up", "Down", "Down"), levels = levels(truth))
#' cv_classification_report(preds, truth, positive = "Up")
#'
#' @export
cv_classification_report <- function(predictions, truth, positive = NULL) {
  if (length(predictions) != length(truth)) {
    stop("predictions and truth must have the same length.")
  }

  truth <- factor(truth)                                # coerce the true labels to a factor
  if (is.null(positive)) positive <- levels(truth)[1L]  # default the positive class to the first level
  if (!positive %in% levels(truth)) {
    stop("positive must be one of the outcome levels.")
  }

  predictions <- factor(predictions, levels = levels(truth))   # align prediction levels with the truth factor
  confusion <- table(predictions, truth, dnn = c("predicted", "truth")) # labelled confusion matrix
  total <- sum(confusion)                                      # total number of evaluated cases

  accuracy <- if (total > 0) sum(diag(confusion)) / total else NA_real_ # share of correct predictions
  metrics <- list(
    accuracy = accuracy,
    error_rate = if (!is.na(accuracy)) 1 - accuracy else NA_real_       # misclassification proportion
  )

  if (nlevels(truth) == 2L) {
    neg <- setdiff(levels(truth), positive)                       # identify the negative class label
    tp <- confusion[positive, positive]                           # true positives
    fn <- sum(confusion[, positive]) - tp                         # false negatives
    fp <- sum(confusion[positive, ]) - tp                         # false positives
    tn <- sum(confusion[neg, neg])                                # true negatives
    safe_ratio <- function(num, den) if (den > 0) num / den else NA_real_ # helper to avoid zero-division
    sens <- safe_ratio(tp, tp + fn)                               # recall / sensitivity
    spec <- safe_ratio(tn, tn + fp)                               # specificity
    prec <- safe_ratio(tp, tp + fp)                               # precision
    f1 <- if (is.na(prec) || is.na(sens) || (prec + sens) == 0) NA_real_ else 2 * prec * sens / (prec + sens)
    metrics <- c(metrics,
                 list(sensitivity = sens,
                      specificity = spec,
                      precision = prec,
                      f1 = f1))
  }
  list(confusion = confusion, metrics = metrics)
}

#' Bootstrap Test Error for Logistic Regression
#'
#' Estimates the out-of-bag misclassification error for a logistic regression
#' model by repeatedly resampling the rows (with replacement) and evaluating the
#' fitted model on the observations left out of each resample. This mirrors the
#' bootstrap model assessment described in the STA 478 solutions.
#'
#' @param formula Logistic regression formula.
#' @param data Data frame containing the variables referenced in `formula`.
#' @param B Number of bootstrap replicates.
#' @param seed Optional integer seed for reproducibility.
#' @param threshold Probability cutoff used to convert fitted probabilities to
#'   class predictions.
#'
#' @details
#' For each bootstrap resample, the model is fit on the resampled data and
#' evaluated on the out-of-bag observations. The misclassification error for
#' replicate \eqn{b} is
#' \deqn{\widehat{err}_b = \frac{1}{|O_b|} \sum_{i \in O_b} I(\hat{p}_i \ge t \neq y_i),}
#' where \eqn{O_b} is the out-of-bag index set, \eqn{\hat{p}_i} is the fitted
#' probability, and \eqn{t} is the threshold. The summary returns the mean and
#' standard deviation of \eqn{\widehat{err}_b} across replicates.
#'
#' @return A list with elements:
#' \describe{
#'   \item{mean_error}{Mean out-of-bag misclassification rate across bootstrap
#'   replicates.}
#'   \item{sd_error}{Standard deviation of the misclassification rate.}
#'   \item{distribution}{Data frame of individual bootstrap errors for plotting
#'   or further summarisation.}
#'   \item{skipped}{Number of replicates that were skipped because the bootstrap
#'   sample contained every observation (no out-of-bag data).}
#' }
#'
#' @examples
#' bootstrap_performance(am ~ wt + hp, data = mtcars, B = 25, seed = 123)
#'
#' @export
bootstrap_performance <- function(formula,
                                  data,
                                  B = 1000,
                                  seed = NULL,
                                  threshold = 0.5) {

  if (!is.null(seed)) set.seed(seed)                              # support reproducible resamples

  mf <- stats::model.frame(formula, data = data)                  # construct model frame
  response_name <- all.vars(formula)[1]
  response <- mf[[response_name]]
  response_factor <- if (is.factor(response)) droplevels(response) else factor(response)
  if (nlevels(response_factor) != 2L) {
    stop("bootstrap_performance requires a binary response.")
  }

  positive_class <- levels(response_factor)[2]                    # treat the second level as positive
  negative_class <- setdiff(levels(response_factor), positive_class)

  n <- nrow(mf)
  errors <- numeric()
  skipped <- 0L

  for (b in seq_len(B)) {
    boot_idx <- sample.int(n, size = n, replace = TRUE)           # draw bootstrap sample indices
    oob_idx <- setdiff(seq_len(n), unique(boot_idx))              # out-of-bag indices for testing
    if (!length(oob_idx)) {
      skipped <- skipped + 1L                                     # skip rare resamples with no OOB data
      next
    }

    train_df <- as.data.frame(mf[boot_idx, , drop = FALSE])       # bootstrap training data
    test_df <- as.data.frame(mf[oob_idx, , drop = FALSE])         # out-of-bag test data
    train_df[[response_name]] <- factor(train_df[[response_name]], levels = levels(response_factor))
    test_df[[response_name]] <- factor(test_df[[response_name]], levels = levels(response_factor))
    fit <- tryCatch(
      suppressWarnings(
        stats::glm(formula, data = train_df, family = stats::binomial())
      ),
      error = function(e) NULL                   # skip replicate if glm fails outright
    )
    if (is.null(fit)) {
      skipped <- skipped + 1L
      next
    }

    probs <- stats::predict(fit, newdata = test_df, type = "response")
    pred_labels <- ifelse(probs >= threshold, positive_class, negative_class)
    pred_factor <- factor(pred_labels, levels = levels(response_factor))
    truth <- response_factor[oob_idx]
    errors <- c(errors, mean(pred_factor != truth))               # misclassification rate for this replicate
  }

  if (!length(errors)) {
    return(list(
      mean_error = NA_real_,
      sd_error = NA_real_,
      distribution = data.frame(iteration = integer(0), error = numeric(0)),
      skipped = skipped
    ))
  }

  list(
    mean_error = mean(errors),
    sd_error = stats::sd(errors),
    distribution = data.frame(iteration = seq_along(errors), error = errors),
    skipped = skipped
  )
}

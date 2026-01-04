#' Best-Subset Search with Cross-Validation
#'
#' Exhaustively evaluates predictor subsets up to a requested size using repeated
#' K-fold cross-validation. The function mirrors the best-subset exploration from
#' the STA 478 assignments, returning a tidy summary of mean performance for each
#' model size.
#'
#' @param formula Model formula with the response on the left-hand side.
#' @param data Data frame containing the variables referenced in `formula`.
#' @param family Generalized linear model family. Defaults to `stats::gaussian()`.
#' @param max_predictors Maximum number of predictors to include in any subset.
#' @param k Number of cross-validation folds.
#' @param repeats Number of times to repeat the K-fold splitting.
#' @param seed Optional integer for reproducibility of the resampling.
#' @param metric Optional metric name. Defaults to `"mse"` for Gaussian families
#'   and `"accuracy"` for binomial families.
#' @param max_combinations Upper bound on the number of subsets that will be
#'   evaluated. Helps prevent prohibitively large searches when the predictor
#'   count is high. Defaults to 100,000 subsets.
#'
#' @details
#' For each predictor subset, repeated \eqn{k}-fold cross-validation fits a GLM
#' and scores either mean squared error
#' \deqn{\mathrm{MSE} = \frac{1}{n} \sum_{i=1}^n (y_i - \hat{y}_i)^2}
#' or classification accuracy
#' \deqn{\frac{1}{n} \sum_{i=1}^n I(\hat{y}_i = y_i).}
#' The reported summary averages fold scores across repeats for each subset.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{summary}{Data frame with one row per subset containing the predictor
#'   combination, subset size, mean score, and standard deviation of the score.}
#'   \item{details}{Data frame with the per-fold scores for each subset, useful for
#'   plotting distributions as in the course solutions.}
#' }
#'
#' @examples
#' best_subset_search(mpg ~ wt + hp + qsec, data = mtcars,
#'                    max_predictors = 2, k = 3, repeats = 1)
#'
#' @export
best_subset_search <- function(formula,
                               data,
                               family = stats::gaussian(),
                               max_predictors = 6,
                               k = 5,
                               repeats = 1,
                               seed = NULL,
                               metric = NULL,
                               max_combinations = 1e5) {

  if (!is.null(seed)) set.seed(seed)                            # allow reproducible folds

  mf <- stats::model.frame(formula, data = data)                # construct the model frame once
  response_name <- all.vars(formula)[1]                         # grab the response column name
  terms_obj <- stats::terms(formula, data = data)
  predictors <- attr(terms_obj, "term.labels")                  # list available predictors
  if (!length(predictors)) stop("No predictor terms found in the formula.")

  max_size <- min(max_predictors, length(predictors))           # cap subset size to available columns
  total_combos <- sum(vapply(seq_len(max_size),
                             function(m) choose(length(predictors), m),
                             numeric(1)))
  if (total_combos > max_combinations) {
    stop("Requested search evaluates ", total_combos,
         " subsets; increase max_combinations or lower max_predictors.")
  }

  n <- nrow(mf)
  if (n < k) stop("Number of observations must be at least the number of folds.")

  fam_name <- family$family
  if (is.null(metric)) {
    metric <- if (fam_name == "binomial") "accuracy" else "mse"
  }
  metric <- match.arg(metric, choices = c("accuracy", "mse"))

  response <- mf[[response_name]]
  if (fam_name == "binomial") {
    response_factor <- if (is.factor(response)) droplevels(response) else factor(response)
    if (nlevels(response_factor) != 2L) stop("Binomial family requires a binary response.")
    positive_class <- levels(response_factor)[2]                # treat the second level as the positive class
  } else {
    response_factor <- NULL
    positive_class <- NULL
  }

  summary_rows <- list()
  detail_rows <- list()

  for (size in seq_len(max_size)) {
    combo_list <- utils::combn(predictors, size, simplify = FALSE)  # generate all subsets of this size
    for (vars in combo_list) {
      subset_name <- paste(vars, collapse = " + ")
      scores <- numeric()
      subset_formula <- stats::as.formula(
        paste(response_name, "~", paste(vars, collapse = " + "))
      )

      for (r in seq_len(repeats)) {
        fold_ids <- sample(rep(seq_len(k), length.out = n))     # random fold assignment per repeat
        for (fold in seq_len(k)) {
          test_idx <- which(fold_ids == fold)                   # indices for the hold-out fold
          train_idx <- setdiff(seq_len(n), test_idx)            # remaining rows form the training set
          train_df <- as.data.frame(mf[train_idx, c(response_name, vars), drop = FALSE])
          test_df <- as.data.frame(mf[test_idx, c(response_name, vars), drop = FALSE])
          if (fam_name == "binomial") {
            train_df[[response_name]] <- factor(train_df[[response_name]], levels = levels(response_factor))
          }
          fit <- tryCatch(
            suppressWarnings(stats::glm(subset_formula, data = train_df, family = family)),
            error = function(e) NULL
          )
          if (is.null(fit)) {
            next
          }

          if (fam_name == "binomial") {
            probs <- stats::predict(fit, newdata = test_df, type = "response")
            negative_class <- setdiff(levels(response_factor), positive_class)
            pred_labels <- ifelse(probs >= 0.5, positive_class, negative_class)
            pred_factor <- factor(pred_labels, levels = levels(response_factor))
            truth <- response_factor[test_idx]
            scores <- c(scores, mean(pred_factor == truth))     # store accuracy for this fold
          } else {
            preds <- stats::predict(fit, newdata = test_df, type = "response")
            truth <- test_df[[response_name]]
            scores <- c(scores, mean((truth - preds)^2))        # store MSE for this fold
          }
        }
      }

      if (!length(scores)) next

      detail_rows[[length(detail_rows) + 1]] <- data.frame(
        subset = subset_name,
        size = size,
        score = scores,
        stringsAsFactors = FALSE
      )

      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        subset = subset_name,
        size = size,
        mean_score = mean(scores),
        sd_score = stats::sd(scores),
        metric = metric,
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    summary = do.call(rbind, summary_rows),
    details = do.call(rbind, detail_rows)
  )
}

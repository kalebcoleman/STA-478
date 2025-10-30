# Utility functions for working with multicollinearity diagnostics

#' Variance Inflation Factors
#'
#' Computes the variance inflation factor for each predictor column, following the
#' helper used in Assignment 7.
#'
#' @param data Data frame or matrix of predictors.
#'
#' @return Named numeric vector of VIF values.
#'
#' @examples
#' data("Boston", package = "MASS")
#' compute_vif(subset(Boston, select = -crim))
#'
#' @export
compute_vif <- function(data) {
  df <- as.data.frame(data)                      # ensure we can index columns reliably
  p <- ncol(df)                                  # count predictors under review
  if (p < 2L) {
    stop("compute_vif requires at least two predictor columns.")
  }
  vif_values <- numeric(p)                       # storage for the VIF result per column
  names(vif_values) <- colnames(df)              # keep original column names
  for (i in seq_len(p)) {
    response <- df[[i]]                          # treat column i as the pseudo-response
    predictors <- df[-i]                         # all remaining columns act as regressors
    if (!length(predictors)) {
      vif_values[i] <- NA_real_
      next
    }
    fit <- stats::lm(response ~ ., data = predictors) # regress the column on the others
    r2 <- summary(fit)$r.squared                 # coefficient of determination for the model
    if (is.na(r2) || r2 >= 1) {
      vif_values[i] <- Inf                       # perfect collinearity gives an infinite VIF
    } else {
      vif_values[i] <- 1 / (1 - r2)              # standard VIF formula
    }
  }
  vif_values
}

#' Stepwise VIF Screening
#'
#' Iteratively removes the predictor with the largest VIF until all remaining VIFs
#' fall below the threshold, mirroring the Boston feature-selection exercise.
#'
#' @param data Data frame or matrix of predictors.
#' @param threshold Numeric cut-off for acceptable VIF values.
#'
#' @return List with elements `kept`, `removed`, and `final_vif`.
#'
#' @examples
#' data("Boston", package = "MASS")
#' stepwise_vif(subset(Boston, select = -crim), threshold = 5)
#'
#' @export
stepwise_vif <- function(data, threshold = 5) {
  df <- as.data.frame(data)                      # coerce once to avoid repeated conversions
  if (!ncol(df)) {
    stop("stepwise_vif requires at least one predictor column.")
  }
  removed <- character()                         # track which variables get discarded
  current <- df                                  # working design matrix that shrinks over iterations
  repeat {
    if (ncol(current) <= 1L) break               # stop when there is nothing left to compare
    vifs <- compute_vif(current)                 # recompute VIFs for the surviving predictors
    if (!length(vifs) || all(is.na(vifs))) {
      break
    }
    inf_vars <- names(vifs)[is.infinite(vifs)]   # prioritise dropping any perfectly collinear columns
    if (length(inf_vars)) {
      drop_var <- inf_vars[1]
    } else {
      max_vif <- max(vifs, na.rm = TRUE)         # otherwise target the largest VIF in the set
      if (is.na(max_vif) || max_vif <= threshold || ncol(current) <= 1L) {
        break
      }
      drop_var <- names(which.max(vifs))
    }
    removed <- c(removed, drop_var)              # log the variable we are removing
    current <- current[setdiff(names(current), drop_var)] # update the design matrix
  }
  finale <- switch(
    as.character(ncol(current)),
    "0" = numeric(0),                             # nothing left, return an empty vector
    "1" = setNames(NA_real_, names(current)),     # single column: VIF undefined, flag with NA
    compute_vif(current)                          # otherwise, provide the updated VIF table
  )
  list(kept = names(current), removed = removed, final_vif = finale)
}

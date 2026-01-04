#' Random Train/Test Split
#'
#' Splits a data frame into training and test subsets, matching the simple
#' random 70/30 splits
#'
#' @param data Data frame or matrix to partition.
#' @param prop Proportion of rows assigned to the training set.
#' @param seed Optional integer seed for reproducibility.
#'
#' @details
#' Rows are sampled without replacement to allocate \eqn{\lfloor p n \rfloor}
#' observations to the training set, where \eqn{p} is `prop` and \eqn{n} is the
#' number of rows. The remaining rows form the test set.
#'
#' @return List with elements `train`, `test`, and `index` (train/test row ids).
#'
#' @examples
#' set.seed(1)
#' parts <- train_test_split(mtcars, prop = 0.66)
#' str(parts$train)
#'
#' @export
train_test_split <- function(data, prop = 0.70, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)           # allow reproducible sampling when a seed is provided
  df <- data                                   # operate on a local copy of the input
  n <- NROW(df)                                # total number of rows available
  if (!is.numeric(prop) || length(prop) != 1L || prop <= 0 || prop >= 1) {
    stop("prop must be a number in (0, 1).")
  }
  if (n == 0L) {
    stop("data must have at least one row.")
  }
  train_size <- max(1L, floor(prop * n))       # training set size, never letting it drop to zero
  train_idx <- sample(n, size = train_size)    # random draw of row positions for training
  test_idx <- setdiff(seq_len(n), train_idx)   # remaining rows become the test set
  list(
    train = df[train_idx, , drop = FALSE],     # training data frame
    test = df[test_idx, , drop = FALSE],       # test data frame
    index = list(train = sort(train_idx), test = sort(test_idx)) # sorted indices for reproducibility checks
  )
}

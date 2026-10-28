#' Bootstrap horizon returns.
#'
#' Whole historical days are resampled with replacement (one index matrix shared
#' by all assets), so the cross-section is preserved as observed. Days are drawn
#' independently; the horizon simple return is exp(sum of daily log returns) - 1.
#' @return numeric matrix n_scenarios x assets, with the columns of `logret`.
simulate_horizon_returns <- function(logret, horizon, n_scenarios, seed) {
  set.seed(seed)
  idx <- sample.int(nrow(logret), n_scenarios * horizon, replace = TRUE)
  cum <- matrix(0, n_scenarios, ncol(logret))
  for (j in seq_len(ncol(logret))) {
    cum[, j] <- rowSums(matrix(logret[idx, j], n_scenarios, horizon))
  }
  out <- exp(cum) - 1
  colnames(out) <- colnames(logret)
  out
}

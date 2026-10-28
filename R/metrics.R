#' Portfolio return in each scenario (buy-and-hold weighted sum).
portfolio_scenarios <- function(R, w) as.numeric(R %*% w)

#' Empirical CVaR: mean loss over the worst (1 - level) share of scenarios, as a
#' positive number; the boundary scenario is weighted fractionally.
expected_shortfall <- function(returns, level) {
  losses <- -returns
  n <- length(losses)
  m <- (1 - level) * n
  if (m <= 0 || level <= 0) stop("level must lie strictly between 0 and 1")
  k <- floor(m)
  top <- sort(losses, decreasing = TRUE)
  tail <- sum(top[seq_len(k)]) + (m - k) * (if (k < n) top[k + 1] else 0)
  tail / m
}

#' Mean portfolio return across scenarios.
expected_return <- function(R, w) sum(colMeans(R) * w)

#' Expected excess return divided by the ES of the excess returns.
excess_ratio <- function(R, w, level, rf_h) {
  (expected_return(R, w) - rf_h) /
    expected_shortfall(portfolio_scenarios(R, w) - rf_h, level)
}

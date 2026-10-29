# Portfolio metrics on a scenario matrix R (one row per scenario, one column
# per asset, simple returns over the horizon) and a weight vector w.


#' Portfolio return in each scenario.
#'
#' Buy-and-hold: the weights are fixed at the start of the horizon, so the
#' portfolio return in scenario s is the weighted sum of the asset returns.
portfolio_scenarios <- function(R, w) as.numeric(R %*% w)


#' Expected Shortfall (ES) at `level`, as a positive loss.
#'
#' ES is the average loss in the worst (1 - level) share of scenarios: at
#' level 0.95 with 10 000 scenarios, the average of the 500 largest losses.
#' When (1 - level) * n is not a whole number, the scenario on the boundary
#' of the tail counts with a fractional weight, so that the value equals the
#' optimum of the Rockafellar-Uryasev linear program exactly.
expected_shortfall <- function(returns, level) {
  losses <- -returns                       # a loss is a negative return
  n <- length(losses)
  m <- (1 - level) * n                     # tail size in scenarios (may be fractional)
  if (m <= 0 || level <= 0) stop("level must lie strictly between 0 and 1")

  k <- floor(m)                            # whole scenarios in the tail
  top <- sort(losses, decreasing = TRUE)   # largest losses first

  tail_loss <- sum(top[seq_len(k)])        # the k largest losses in full ...
  # ... plus a fraction (m - k) of the next one. When k == n the tail is the
  # whole sample and there is no next scenario.
  if (k < n) tail_loss <- tail_loss + (m - k) * top[k + 1]
  tail_loss / m                            # average over the tail
}


#' Expected portfolio return: the weighted average of the asset means.
expected_return <- function(R, w) sum(colMeans(R) * w)


#' Return-to-risk ratio, the analogue of the Sharpe ratio.
#'
#' Expected excess return over the risk-free rate divided by the ES of the
#' excess returns. Both numerator and denominator use excess returns, which
#' is what makes the single-LP solution in max_ratio_portfolio() exact.
excess_ratio <- function(R, w, level, rf_h) {
  excess_returns <- portfolio_scenarios(R, w) - rf_h
  (expected_return(R, w) - rf_h) / expected_shortfall(excess_returns, level)
}

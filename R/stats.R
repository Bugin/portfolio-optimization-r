# Tables and small helpers used by the notebook output.


#' Weights of the two optimal portfolios in percent, one row per asset.
weights_table <- function(min_es, max_ratio) {
  data.frame(`Min-ES` = round(min_es$w * 100, 2),
             `Max-ratio` = round(max_ratio$w * 100, 2),
             row.names = min_es$tickers, check.names = FALSE)
}


#' Allocation line: expected return of "cash + the Max-ratio portfolio".
#'
#' Put a fraction t >= 0 of the money in the Max-ratio portfolio P and 1 - t
#' in cash earning rf_h (t > 1 would mean borrowing at rf_h).
#' Expected return: y = rf_h + t * (E[R_P] - rf_h).
#' Expected Shortfall of raw returns: x = t * ES(R_P) - (1 - t) * rf_h, because
#' a sure gain of rf_h is a "loss" of -rf_h. Hence x + rf_h = t * (ES(R_P) + rf_h)
#' = t * ES(R_P - rf_h), the ES of the excess returns. Eliminating t:
#'     y = rf_h + ratio * (x + rf_h),   with ratio = (E[R_P] - rf_h) / ES(R_P - rf_h),
#' a straight line through cash at (-rf_h, rf_h) and through P. `xs` are the
#' x values at which to evaluate it.
allocation_line <- function(max_ratio, rf_h, xs) {
  rf_h + max_ratio$ratio * (xs + rf_h)
}


#' Moments and tail measures of simulated horizon returns, one row per column.
#'
#' Columns: mean, median, std, var_<level>, es_<level>, prob_loss, min, max.
#' VaR is a quantile of returns (usually negative); ES is a positive loss.
horizon_stats <- function(returns, level) {
  level_pct <- trimws(format(level * 100))      # "95", "97.5", ...
  one_series <- function(x) {
    c(mean(x),
      median(x),
      sd(x),
      quantile(x, 1 - level, names = FALSE),   # VaR: the (1 - level) quantile
      expected_shortfall(x, level),            # ES: average loss beyond it
      mean(x < 0),                             # probability of a loss
      min(x),
      max(x))
  }
  stats <- t(apply(returns, 2, one_series))    # one row per column of `returns`
  out <- as.data.frame(stats)
  names(out) <- c("mean", "median", "std", paste0("var_", level_pct),
                  paste0("es_", level_pct), "prob_loss", "min", "max")
  rownames(out) <- colnames(returns)
  out
}

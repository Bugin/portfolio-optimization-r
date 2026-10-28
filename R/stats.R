#' Weights (in percent) of the two optimal portfolios, one row per asset.
weights_table <- function(min_es, max_ratio) {
  data.frame(`Min-ES` = round(min_es$w * 100, 2),
             `Max-ratio` = round(max_ratio$w * 100, 2),
             row.names = min_es$tickers, check.names = FALSE)
}

#' Expected return of cash plus the max-ratio portfolio P, at ES (of raw returns) xs.
#' A mix t*P + (1-t)*cash sits at x = t*ES(R_P) - (1-t)*rf_h, y = rf_h + t*(E[R_P] - rf_h),
#' so cash is at (-rf_h, rf_h) and the line is y = rf_h + ratio * (x + rf_h).
allocation_line <- function(max_ratio, rf_h, xs) {
  rf_h + max_ratio$ratio * (xs + rf_h)
}

#' Moments and tail measures of simulated horizon returns, one row per column.
#' The VaR and ES columns are named after the level, e.g. `var_95`, `es_97.5`.
horizon_stats <- function(returns, level) {
  lp <- trimws(format(level * 100))
  stats <- t(apply(returns, 2, function(x) {
    c(mean(x), median(x), sd(x), quantile(x, 1 - level, names = FALSE),
      expected_shortfall(x, level), mean(x < 0), min(x), max(x))
  }))
  out <- as.data.frame(stats)
  names(out) <- c("mean", "median", "std", paste0("var_", lp), paste0("es_", lp),
                  "prob_loss", "min", "max")
  rownames(out) <- colnames(returns)
  out
}

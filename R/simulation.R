# Simulation of the asset returns at the horizon by bootstrap.


#' Bootstrap the returns of every asset over `horizon` business days.
#'
#' Each scenario is built by drawing `horizon` historical days at random,
#' with replacement, and summing their daily log returns. The *same* days
#' are used for all assets: a day is resampled as a whole row of `logret`,
#' so the co-movement of the assets (correlation, joint crashes) is exactly
#' the one observed in history. Days are drawn independently of each other,
#' so the average drift is reproduced but no serial dependence is
#' (no autocorrelation, no volatility clustering).
#'
#' The sum of daily log returns is the horizon log return; exp(.) - 1 turns
#' it into a simple return. Returns a matrix n_scenarios x assets.
simulate_horizon_returns <- function(logret, horizon, n_scenarios, seed) {
  set.seed(seed)
  n_days <- nrow(logret)
  n_assets <- ncol(logret)

  # One vector of n_scenarios * horizon random day indices, shared by all
  # assets. matrix() below fills column by column, so scenario s uses the
  # draws s, s + n_scenarios, s + 2 n_scenarios, ... (not consecutive ones);
  # which draws go to which scenario is irrelevant, they are all independent.
  day_index <- sample.int(n_days, n_scenarios * horizon, replace = TRUE)

  cum_logret <- matrix(0, n_scenarios, n_assets)
  for (j in seq_len(n_assets)) {
    # logret[day_index, j] picks the daily log returns of asset j on the drawn
    # days; arranging them as n_scenarios x horizon and summing each row gives
    # the horizon log return of every scenario.
    daily <- matrix(logret[day_index, j], n_scenarios, horizon)
    cum_logret[, j] <- rowSums(daily)
  }

  scenarios <- exp(cum_logret) - 1
  colnames(scenarios) <- colnames(logret)
  scenarios
}

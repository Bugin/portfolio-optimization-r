# Time the notebook pipeline ("Asset Allocation EUR.Rmd", chunks run +
# distributions) step by step with a given number of scenarios. Figures go to
# the folder passed as second argument, so the repo's figures/ is untouched.
#
# Run from this folder:  Rscript notebook_timing.R [n_scenarios] [fig_dir]
args <- commandArgs(trailingOnly = TRUE)
N <- if (length(args) >= 1) as.integer(args[1]) else 100000L
FIG <- if (length(args) >= 2) args[2] else file.path(getwd(), sprintf("notebook_figs_%dk", N %/% 1000))
dir.create(FIG, showWarnings = FALSE)
t_all <- Sys.time()
setwd("../..")  # repository root
source("R/all.R")
N_SCENARIOS <- N
if (length(args) >= 3) N_FRONTIER <- as.integer(args[3])

tm <- function(label, expr) {
  t <- system.time(expr)[["elapsed"]]
  cat(sprintf("%-28s %8.1f s\n", label, t)); flush.console()
  invisible(t)
}

tm("data (cache)", {
  data <- get_prices_and_logret(TICKERS)
  prices <- data$prices
  logret <- data$logret
  colnames(logret) <- unname(NAMES[colnames(logret)])
  logret <- aggregate_logret(logret, RETURN_FREQ)
  horizon <- horizon_periods(HORIZON_DAYS, RETURN_FREQ)
  h_label <- horizon_label(HORIZON_DAYS, RETURN_FREQ)
  b <- check_bounds(BOUNDS, colnames(logret))
  lo <- b$lo; hi <- b$hi
  rf_h <- exp(RISK_FREE * HORIZON_DAYS / 252) - 1
})
tm("simulate", scenarios <- simulate_horizon_returns(logret, horizon, N_SCENARIOS, SIM_SEED))
tm("asset stats", {
  asset_ret <- colMeans(scenarios)
  asset_es <- apply(scenarios, 2, expected_shortfall, level = ES_LEVEL)
})
tm("random cloud (3000)", rand <- sample_random_portfolios(scenarios, ES_LEVEL, lo, hi, N_RANDOM, DIRICHLET_ALPHA, CLOUD_SEED))
tm("LP min-ES", min_es <- min_es_portfolio(scenarios, ES_LEVEL, lo, hi, rf_h))
tm("LP max-ratio", max_ratio <- max_ratio_portfolio(scenarios, ES_LEVEL, lo, hi, rf_h))
tm("max return LP", r_hi <- 0.999 * max_return(scenarios, lo, hi))
tm(sprintf("frontier (%d points)", N_FRONTIER),
   frontier <- build_es_frontier(scenarios, ES_LEVEL, lo, hi, rf_h, N_FRONTIER, min_es$ret, r_hi))
tm("plot frontier + save", {
  p_frontier <- plot_frontier(rand, frontier$es, frontier$ret, min_es, max_ratio,
                              asset_ret, asset_es, rf_h, h_label, ES_LEVEL)
  ggplot2::ggsave(file.path(FIG, "frontier.png"), p_frontier, width = 9, height = 6, dpi = 150, bg = "white")
})
tm("distributions + save", {
  returns <- cbind(scenarios,
                   "Min-ES" = portfolio_scenarios(scenarios, min_es$w),
                   "Max-ratio" = portfolio_scenarios(scenarios, max_ratio$w))
  stats <- horizon_stats(returns, ES_LEVEL)
  p_dist <- plot_distributions(returns, h_label)
  ggplot2::ggsave(file.path(FIG, "distributions.png"), p_dist, width = 9.6, height = 4.6, dpi = 150, bg = "white")
})
cat(sprintf("Min-ES:     E[R]=%.2f%%  ES=%.2f%%  ratio=%.3f\n", 100 * min_es$ret, 100 * min_es$es, min_es$ratio))
cat(sprintf("Max-ratio:  E[R]=%.2f%%  ES=%.2f%%  ratio=%.3f\n", 100 * max_ratio$ret, 100 * max_ratio$es, max_ratio$ratio))
cat(sprintf("TOTAL %.1f s (%.1f min)\n", as.numeric(difftime(Sys.time(), t_all, units = "secs")),
            as.numeric(difftime(Sys.time(), t_all, units = "mins"))))

# Computing time of one pipeline step as a function of the number of
# scenarios: simulation, Min-ES LP, Max-ratio LP, one frontier point.
# Data to 2026-10-30, daily returns, seed 42, two repetitions each.
#
# Run from this folder:  Rscript benchmark.R
OUT <- file.path(getwd(), "benchmark.csv")
setwd("../..")  # repository root
source("R/all.R")

data <- get_prices_and_logret(TICKERS, ttl_hours = 1e9)
logret <- data$logret
colnames(logret) <- unname(NAMES[colnames(logret)])
b <- check_bounds(BOUNDS, colnames(logret))
lo <- b$lo
hi <- b$hi
rf_h <- 0

tm <- function(expr) as.numeric(system.time(expr)["elapsed"])
rows <- list()
for (n in c(10000L, 25000L, 50000L, 100000L)) for (rep in 1:2) {
  t_sim <- tm(scen <- simulate_horizon_returns(logret, HORIZON_DAYS, n, SIM_SEED))
  t_min <- tm(min_es <- min_es_portfolio(scen, ES_LEVEL, lo, hi, rf_h))
  t_max <- tm(max_ratio <- max_ratio_portfolio(scen, ES_LEVEL, lo, hi, rf_h))
  target <- (min_es$ret + max_ratio$ret) / 2
  t_fr <- tm(min_es_portfolio(scen, ES_LEVEL, lo, hi, rf_h, target_return = target))
  rows[[length(rows) + 1]] <- data.frame(n_scenarios = n, rep = rep, t_simulate = t_sim,
                                         t_min_es = t_min, t_max_ratio = t_max,
                                         t_frontier_point = t_fr,
                                         mem_scen_mb = as.numeric(object.size(scen)) / 2^20,
                                         min_es_es = min_es$es, max_ratio_ratio = max_ratio$ratio)
  cat(sprintf("n=%6d rep %d: simulate %.2fs  min-ES %.2fs  max-ratio %.2fs  frontier pt %.2fs\n",
              n, rep, t_sim, t_min, t_max, t_fr))
}
write.csv(do.call(rbind, rows), OUT, row.names = FALSE)

# Stability study: re-run the asset-allocation pipeline at 13 month-end
# cut-offs (Oct 2025 .. Oct 2026), daily and weekly, and measure how the
# optimal portfolios and their metrics move. Writes CSVs to ./out_<N>k.
#
# Run from this folder:  Rscript stability_runs.R [n_scenarios]   (default 10000)
t0 <- Sys.time()
args <- commandArgs(trailingOnly = TRUE)
N_SCEN <- if (length(args) >= 1) as.integer(args[1]) else 10000L
OUT <- file.path(getwd(), sprintf("out_%dk", N_SCEN %/% 1000))
dir.create(OUT, showWarnings = FALSE)
setwd("../..")  # repository root
source("R/all.R")
N_SCENARIOS <- N_SCEN

FREQS <- c("daily", "weekly")
NOISE_SEEDS <- c(1, 2, 3, 4, 5, 42)
lvl <- ES_LEVEL
rf_h <- 0

data <- get_prices_and_logret(TICKERS, ttl_hours = 1e9)   # the cache of 2026-11-02
prices <- data$prices
logret <- data$logret
colnames(logret) <- unname(NAMES[colnames(logret)])
colnames(prices) <- unname(NAMES[colnames(prices)])
dates <- as.Date(rownames(logret))
cat(sprintf("Full sample %s to %s, %d daily returns\n", rownames(prices)[1],
            rownames(prices)[nrow(prices)], nrow(logret)))

# Cut-offs: last trading day of each month from 2025-10 to 2026-10.
months <- format(seq(as.Date("2025-10-01"), as.Date("2026-10-01"), by = "month"), "%Y-%m")
cutoffs <- as.Date(sapply(months, function(m) max(dates[format(dates, "%Y-%m") == m])),
                   origin = "1970-01-01")
names(cutoffs) <- months

b <- check_bounds(BOUNDS, colnames(logret))
lo <- b$lo
hi <- b$hi
nm <- colnames(logret)

run_one <- function(cutoff, freq, seed) {
  lr <- logret[dates <= cutoff, , drop = FALSE]
  lr <- aggregate_logret(lr, freq)
  horizon <- horizon_periods(HORIZON_DAYS, freq)
  scen <- simulate_horizon_returns(lr, horizon, N_SCENARIOS, seed)
  min_es <- min_es_portfolio(scen, lvl, lo, hi, rf_h)
  max_ratio <- max_ratio_portfolio(scen, lvl, lo, hi, rf_h)
  stats <- horizon_stats(scen, lvl)
  list(scen = scen, n = nrow(lr), last = rownames(lr)[nrow(lr)],
       min_es = min_es, max_ratio = max_ratio, stats = stats, lr = lr)
}

row_of <- function(month, cutoff, freq, seed, r) {
  one <- function(tag, p) {
    x <- c(p$ret, p$es, p$ratio, p$w)
    names(x) <- paste0(tag, "_", c("ret", "es", "ratio", nm))
    x
  }
  st <- r$stats
  asset <- c(st$mean, st$std, st[[paste0("es_", trimws(format(lvl * 100)))]])
  names(asset) <- c(paste0("mean_", nm), paste0("std_", nm), paste0("es_", nm))
  data.frame(month = month, cutoff = as.character(cutoff), freq = freq, seed = seed,
             n_periods = r$n, last_period = r$last,
             t(c(one("minES", r$min_es), one("maxR", r$max_ratio), asset)),
             check.names = FALSE, stringsAsFactors = FALSE)
}

# --- 1. Monthly runs, seed 42 ---------------------------------------------
runs <- list()
rows <- list()
for (freq in FREQS) for (m in months) {
  r <- run_one(cutoffs[m], freq, SIM_SEED)
  runs[[paste(freq, m)]] <- r
  rows[[length(rows) + 1]] <- row_of(m, cutoffs[m], freq, SIM_SEED, r)
  cat(sprintf("%s %s n=%d minES: ES=%.4f%% w=[%s] | maxR: ratio=%.4f w=[%s]\n", freq, m, r$n,
              100 * r$min_es$es, paste(sprintf("%.1f", 100 * r$min_es$w), collapse = " "),
              r$max_ratio$ratio, paste(sprintf("%.1f", 100 * r$max_ratio$w), collapse = " ")))
}
monthly <- do.call(rbind, rows)
write.csv(monthly, file.path(OUT, "monthly_runs.csv"), row.names = FALSE)

# --- 2. Monte Carlo noise: first and last cut-off, several seeds ----------
first_m <- months[1]
last_m <- months[length(months)]
rows <- list()
for (m in c(first_m, last_m)) for (freq in FREQS) for (s in NOISE_SEEDS) {
  r <- if (s == SIM_SEED) runs[[paste(freq, m)]] else run_one(cutoffs[m], freq, s)
  rows[[length(rows) + 1]] <- row_of(m, cutoffs[m], freq, s, r)
}
write.csv(do.call(rbind, rows), file.path(OUT, "noise_seeds.csv"), row.names = FALSE)

# --- 3. Cross-evaluation: cost of using last month's weights --------------
# For each month t, evaluate its optimal weights on the scenarios of month
# t+1 and on the final scenarios, against the optimum found there.
ev <- function(scen, w) c(es = expected_shortfall(portfolio_scenarios(scen, w), lvl),
                          ratio = excess_ratio(scen, w, lvl, rf_h),
                          ret = expected_return(scen, w))
rows <- list()
for (freq in FREQS) {
  fin <- runs[[paste(freq, last_m)]]
  for (i in seq_along(months)) {
    r <- runs[[paste(freq, months[i])]]
    a <- ev(fin$scen, r$min_es$w)
    bb <- ev(fin$scen, r$max_ratio$w)
    row <- data.frame(month = months[i], freq = freq,
                      minES_es_on_final = a[["es"]], minES_es_final_opt = fin$min_es$es,
                      maxR_ratio_on_final = bb[["ratio"]], maxR_ratio_final_opt = fin$max_ratio$ratio,
                      minES_es_on_next = NA, minES_es_next_opt = NA,
                      maxR_ratio_on_next = NA, maxR_ratio_next_opt = NA,
                      stringsAsFactors = FALSE)
    if (i < length(months)) {
      nxt <- runs[[paste(freq, months[i + 1])]]
      a2 <- ev(nxt$scen, r$min_es$w)
      b2 <- ev(nxt$scen, r$max_ratio$w)
      row$minES_es_on_next <- a2[["es"]]
      row$minES_es_next_opt <- nxt$min_es$es
      row$maxR_ratio_on_next <- b2[["ratio"]]
      row$maxR_ratio_next_opt <- nxt$max_ratio$ratio
    }
    rows[[length(rows) + 1]] <- row
  }
}
write.csv(do.call(rbind, rows), file.path(OUT, "cross_eval.csv"), row.names = FALSE)

# --- 4. Flatness of the ES surface at the final date ----------------------
# ES, E[R] and ratio along the Govt/IG segment (w_HY = w_Eq = 0).
rows <- list()
for (freq in FREQS) {
  scen <- runs[[paste(freq, last_m)]]$scen
  for (g in seq(0, 1, by = 0.05)) {
    w <- c(g, 1 - g, 0, 0)
    e <- ev(scen, w)
    rows[[length(rows) + 1]] <- data.frame(freq = freq, w_govt = g, es = e[["es"]],
                                           ret = e[["ret"]], ratio = e[["ratio"]])
  }
}
write.csv(do.call(rbind, rows), file.path(OUT, "govt_ig_segment.csv"), row.names = FALSE)

# --- 5. What each month added: realised asset returns and extreme days ----
rows <- list()
full_daily_q <- apply(logret, 2, quantile, probs = 0.05)
for (i in seq_along(months)) {
  from <- if (i == 1) as.Date("1900-01-01") else cutoffs[i - 1]
  sel <- dates > from & dates <= cutoffs[i]
  lr <- logret[sel, , drop = FALSE]
  monthly_ret <- exp(colSums(lr)) - 1
  worst_day <- apply(lr, 2, min)
  n_tail <- sapply(nm, function(a) sum(lr[, a] < full_daily_q[a]))
  rows[[length(rows) + 1]] <- data.frame(
    month = months[i], cutoff = as.character(cutoffs[i]), n_days = nrow(lr),
    t(c(setNames(monthly_ret, paste0("ret_", nm)),
        setNames(worst_day, paste0("worst_", nm)),
        setNames(n_tail, paste0("ntail_", nm)))),
    check.names = FALSE, stringsAsFactors = FALSE)
}
write.csv(do.call(rbind, rows), file.path(OUT, "month_added.csv"), row.names = FALSE)

# --- 6. Realised 60-business-day forward return of each optimal portfolio -
rows <- list()
pdates <- as.Date(rownames(prices))
for (freq in FREQS) for (i in seq_along(months)) {
  j <- which(pdates == cutoffs[i])
  if (j + HORIZON_DAYS > nrow(prices)) next
  ar <- prices[j + HORIZON_DAYS, ] / prices[j, ] - 1
  r <- runs[[paste(freq, months[i])]]
  rows[[length(rows) + 1]] <- data.frame(
    month = months[i], freq = freq, end = rownames(prices)[j + HORIZON_DAYS],
    minES_exp = r$min_es$ret, minES_real = sum(ar * r$min_es$w),
    minES_es = r$min_es$es,
    maxR_exp = r$max_ratio$ret, maxR_real = sum(ar * r$max_ratio$w),
    maxR_es = r$max_ratio$es,
    t(setNames(ar, paste0("real_", nm))), check.names = FALSE, stringsAsFactors = FALSE)
}
write.csv(do.call(rbind, rows), file.path(OUT, "realised_forward.csv"), row.names = FALSE)

# --- 7. Sample sizes and correlations at first and last cut-off -----------
rows <- list()
for (freq in FREQS) for (m in c(first_m, last_m)) {
  lr <- runs[[paste(freq, m)]]$lr
  C <- cor(lr)
  rows[[length(rows) + 1]] <- data.frame(
    freq = freq, month = m, n = nrow(lr),
    cor_govt_ig = C[1, 2], cor_govt_hy = C[1, 3], cor_govt_eq = C[1, 4],
    cor_ig_hy = C[2, 3], cor_ig_eq = C[2, 4], cor_hy_eq = C[3, 4],
    stringsAsFactors = FALSE)
}
write.csv(do.call(rbind, rows), file.path(OUT, "correlations.csv"), row.names = FALSE)

cat(sprintf("Elapsed %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

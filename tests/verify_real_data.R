# Verification on the real data (spec items 1-4). A script, not a testthat test.
#
# Run from the repository root:  Rscript tests/verify_real_data.R
# Exits with status 1 if any check fails.

t0 <- Sys.time()
source("R/all.R")

GRID_STEP <- 0.02
CHUNK <- 500
TIGHT_BOUNDS <- list("Europe Equity" = c(0, 0.2), "Euro Govt" = c(0.1, 1))

#' All weight vectors on a grid of the given step that sum to 1 (rows).
#' Same as `simplex_grid` in tests/testthat/helper-fixtures.R.
simplex_grid <- function(n_assets, step) {
  k <- as.integer(round(1 / step))
  g <- as.matrix(expand.grid(rep(list(0:k), n_assets)))
  g <- g[rowSums(g) == k, , drop = FALSE]
  dimnames(g) <- NULL
  g / k
}

#' Keep the rows of W that satisfy lo <= w <= hi.
apply_bounds <- function(W, lo, hi) {
  tol <- 1e-12
  ok <- apply(W, 1, function(w) all(w >= lo - tol & w <= hi + tol))
  W[ok, , drop = FALSE]
}

failures <- character(0)

#' Print a PASS/FAIL line and record failures.
report <- function(label, ok, detail) {
  cat(sprintf("%s  %s: %s\n", if (ok) "PASS" else "FAIL", label, detail))
  if (!ok) failures <<- c(failures, label)
}

fmt_w <- function(w) paste(sprintf("%.2f", w), collapse = ", ")

#' Run checks 1-4 for one set of bounds.
verify <- function(logret, scenarios, rf_h, bounds, tag) {
  level <- ES_LEVEL
  nm <- colnames(scenarios)
  b <- check_bounds(bounds, nm)
  lo <- b$lo
  hi <- b$hi
  n <- length(lo)
  cat(sprintf("\n=== %s: lo=[%s] hi=[%s]\n", tag, fmt_w(lo), fmt_w(hi)))

  min_es <- min_es_portfolio(scenarios, level, lo, hi, rf_h)
  max_ratio <- max_ratio_portfolio(scenarios, level, lo, hi, rf_h)
  r_hi <- 0.999 * max_return(scenarios, lo, hi)
  f <- build_es_frontier(scenarios, level, lo, hi, rf_h, N_FRONTIER, min_es$ret, r_hi)
  for (p in list(list("Min-ES", min_es), list("Max-ratio", max_ratio))) {
    x <- p[[2]]
    cat(sprintf("%s: E[R]=%.4f%%  ES=%.4f%%  ratio=%.6f | %s\n", p[[1]], 100 * x$ret,
                100 * x$es, x$ratio,
                paste(sprintf("%s=%.4f%%", nm, 100 * x$w), collapse = ", ")))
  }

  # 1. LP consistency: the LP objective equals the ES of the returned weights.
  es_lp <- solve_es_lp(scenarios, level, lo, hi)$es
  d1 <- abs(es_lp - min_es$es)
  report(sprintf("[%s] 1 LP consistency Min-ES", tag), d1 <= 1e-8,
         sprintf("LP objective=%.12f vs ES(w)=%.12f, |diff|=%.2e", es_lp, min_es$es, d1))
  # Max-ratio: re-solve the homogeneous LP; ES(y) / sum(y) must equal the ES of the
  # excess returns of w = y / sum(y) (positive homogeneity).
  R_ex <- scenarios - rf_h
  A_b <- rbind(lo %o% rep(1, n) - diag(n), diag(n) - hi %o% rep(1, n))
  lp <- solve_es_lp(R_ex, level, w_lower = rep(0, n), w_upper = rep(Inf, n),
                    extra_eq = list(colMeans(R_ex), 1),
                    extra_ub = list(A_b, rep(0, 2 * n)), budget = FALSE)
  es_ex_w <- expected_shortfall(as.numeric(R_ex %*% max_ratio$w), level)
  d1b <- abs(lp$es / sum(lp$w) - es_ex_w)
  report(sprintf("[%s] 1 LP consistency Max-ratio", tag), d1b <= 1e-8,
         sprintf("LP objective/sum(y)=%.12f vs ES of excess returns of w=%.12f, |diff|=%.2e",
                 lp$es / sum(lp$w), es_ex_w, d1b))

  # 2. Brute force on a 2 % simplex grid filtered by the bounds.
  W <- apply_bounds(simplex_grid(n, GRID_STEP), lo, hi)
  g_ret <- g_es <- g_es_ex <- numeric(0)
  for (i in seq(1, nrow(W), by = CHUNK)) {
    P <- W[i:min(i + CHUNK - 1, nrow(W)), , drop = FALSE] %*% t(scenarios)
    g_ret <- c(g_ret, rowMeans(P))
    e <- es_rows(P, level)
    g_es <- c(g_es, e)
    g_es_ex <- c(g_es_ex, if (rf_h == 0) e else es_rows(P - rf_h, level))
  }
  g_ratio <- (g_ret - rf_h) / g_es_ex
  j <- which.min(g_es)
  k <- which.max(g_ratio)
  report(sprintf("[%s] 2 brute force ES", tag), g_es[j] >= min_es$es - 1e-9,
         sprintf("%d grid points; grid min ES=%.10f at [%s] vs Min-ES %.10f",
                 nrow(W), g_es[j], fmt_w(W[j, ]), min_es$es))
  report(sprintf("[%s] 2 brute force ratio", tag), g_ratio[k] <= max_ratio$ratio + 1e-6,
         sprintf("grid max ratio=%.8f at [%s] vs Max-ratio %.8f",
                 g_ratio[k], fmt_w(W[k, ]), max_ratio$ratio))

  # 2b. Bounds honoured by the optimal and frontier weights; budget holds.
  all_w <- rbind(unname(min_es$w), unname(max_ratio$w), f$W)
  below <- max(sweep(-all_w, 2, lo, "+"))
  above <- max(sweep(all_w, 2, hi, "-"))
  sums <- rowSums(all_w)
  report(sprintf("[%s] 2b bounds honoured", tag),
         below <= 1e-9 && above <= 1e-9 && max(abs(sums - 1)) <= 1e-9,
         sprintf("%d weight vectors; max(lo - w)=%.2e, max(w - hi)=%.2e; sum(w) in [%.12f, %.12f]",
                 nrow(all_w), below, above, min(sums), max(sums)))

  # 3. Frontier sanity.
  steps <- diff(f$es[order(f$ret)])
  report(sprintf("[%s] 3 frontier ES non-decreasing", tag), min(steps) >= -1e-10,
         sprintf("%d points, smallest ES step=%.2e", length(f$es), min(steps)))
  on <- min_es_portfolio(scenarios, level, lo, hi, rf_h, target_return = max_ratio$ret)
  d3 <- abs(on$es - max_ratio$es)
  report(sprintf("[%s] 3 Max-ratio on frontier", tag), d3 <= 1e-6,
         sprintf("frontier ES at E[R]=%.6f%%: %.10f vs Max-ratio %.10f, |diff|=%.2e",
                 100 * max_ratio$ret, on$es, max_ratio$es, d3))

  # 4. Reproducibility: same seed, same scenarios, same Min-ES weights.
  scen2 <- simulate_horizon_returns(logret, horizon, N_SCENARIOS, SIM_SEED)
  again <- min_es_portfolio(scen2, level, lo, hi, rf_h)
  same_s <- identical(scen2, scenarios)
  same_w <- identical(again$w, min_es$w)
  report(sprintf("[%s] 4 reproducibility", tag), same_s && same_w,
         sprintf("scenarios identical=%s, Min-ES w identical=%s", same_s, same_w))
}

data <- get_prices_and_logret(TICKERS)
prices <- data$prices
logret <- data$logret
colnames(logret) <- unname(NAMES[colnames(logret)])
logret <- aggregate_logret(logret, RETURN_FREQ)
horizon <- horizon_periods(HORIZON_DAYS, RETURN_FREQ)
scenarios <- simulate_horizon_returns(logret, horizon, N_SCENARIOS, SIM_SEED)
rf_h <- 0
cat(sprintf("Sample %s to %s, %d %s returns; %d scenarios x %d %s\n",
            rownames(prices)[1], rownames(prices)[nrow(prices)], nrow(logret), RETURN_FREQ,
            N_SCENARIOS, horizon, if (RETURN_FREQ == "daily") "days" else "weeks"))
verify(logret, scenarios, rf_h, BOUNDS, "BOUNDS (trivial)")
verify(logret, scenarios, rf_h, TIGHT_BOUNDS, "Equity<=20%, Govt>=10%")
cat(sprintf("\nElapsed: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
if (length(failures) == 0) {
  cat("ALL PASS\n")
} else {
  cat(sprintf("%d FAIL: %s\n", length(failures), paste(failures, collapse = "; ")))
  quit(status = 1)
}

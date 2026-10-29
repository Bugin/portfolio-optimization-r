# Random portfolios. They only illustrate the feasible region in the chart;
# the optimal portfolios come from the linear programs in optimisation.R.


#' Expected Shortfall of each row of a portfolios x scenarios matrix.
#'
#' Row i of `P` holds the returns of portfolio i in every scenario. The ES of
#' each row is computed with the same `expected_shortfall()` used everywhere
#' else, so there is a single definition of ES in the project.
es_rows <- function(P, level) {
  es <- numeric(nrow(P))
  for (i in seq_len(nrow(P))) es[i] <- expected_shortfall(P[i, ], level)
  es
}


#' Expected return and ES of `n` random portfolios within the per-asset bounds.
#'
#' Weights are drawn from a Dirichlet distribution (non-negative, summing to
#' one), then shifted so that every lower bound holds, and finally rejected
#' when they break an upper bound. Returns a data.frame with one row per
#' portfolio: its expected return `ret` and its Expected Shortfall `es`.
sample_random_portfolios <- function(R, level, lo, hi, n, alpha, seed) {
  set.seed(seed)
  n_assets <- length(lo)

  # --- Draw weights in batches until `n` of them satisfy the bounds ---------
  batches <- list()
  n_kept <- 0
  first_batch <- TRUE
  while (n_kept < n) {
    # A Dirichlet(alpha, ..., alpha) draw is a vector of gamma draws divided
    # by their sum: non-negative numbers that add up to 1.
    g <- matrix(rgamma(n * n_assets, alpha), n, n_assets)
    d <- g / rowSums(g)

    # Shift towards the lower bounds: w = lo + (1 - sum(lo)) * d still sums
    # to 1 and has w_i >= lo_i by construction.
    W <- d * (1 - sum(lo))
    for (j in seq_len(n_assets)) W[, j] <- W[, j] + lo[j]

    # Keep only the rows that also respect every upper bound (the 1e-12 is
    # floating-point slack: a weight that equals hi_i exactly must pass).
    keep <- logical(nrow(W))
    for (i in seq_len(nrow(W))) keep[i] <- all(W[i, ] <= hi + 1e-12)
    W <- W[keep, , drop = FALSE]

    batches[[length(batches) + 1]] <- W
    n_kept <- n_kept + nrow(W)

    # If almost nothing survives the first batch, the bounds leave a tiny
    # feasible region: stop here instead of looping for a long time.
    if (first_batch && nrow(W) < 0.05 * n) {
      message(sprintf("[cloud] only %d of %d random portfolios satisfy the bounds",
                      nrow(W), n))
      break
    }
    first_batch <- FALSE
  }
  W <- do.call(rbind, batches)
  W <- W[seq_len(min(n, nrow(W))), , drop = FALSE]   # at most n rows (fewer after an early stop)

  # --- Evaluate every portfolio on all scenarios ----------------------------
  # P = W %*% t(R) holds one portfolio per row and one scenario per column.
  # It is computed in chunks of 500 portfolios to keep memory small.
  ret <- numeric(0)
  es <- numeric(0)
  if (nrow(W) > 0) {
    for (start in seq(1, nrow(W), by = 500)) {
      end <- min(start + 499, nrow(W))
      P <- W[start:end, , drop = FALSE] %*% t(R)
      ret <- c(ret, rowMeans(P))
      es <- c(es, es_rows(P, level))
    }
  }
  data.frame(ret = ret, es = es)
}

# Test helpers: source the code from the repo root and define fixtures.
local({
  root <- normalizePath(testthat::test_path("../.."))
  old <- setwd(root)
  on.exit(setwd(old))
  source("R/all.R")
})

#' Synthetic daily log returns: 500 days x 4 asset classes, seed 0.
synthetic_logret <- function() {
  set.seed(0)
  mu <- c(0.0001, 0.0001, 0.0002, 0.0004)
  sd <- c(0.003, 0.003, 0.004, 0.01)
  m <- sapply(1:4, function(j) rnorm(500, mu[j], sd[j]))
  colnames(m) <- c("Euro Govt", "Euro Corp IG", "Euro Corp HY", "Europe Equity")
  rownames(m) <- format(as.Date("2020-01-01") + 0:499)
  m
}

#' Bootstrap scenarios from a log-return matrix (needs Task 2's function).
sim <- function(logret, n = 2000, seed = 1) {
  simulate_horizon_returns(logret, 60, n, seed)
}

#' All weight vectors on a grid of the given step that sum to 1 (rows).
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

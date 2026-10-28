lo4 <- rep(0, 4)
hi4 <- rep(1, 4)
lo_nt <- c(0.1, 0, 0, 0)
hi_nt <- c(1, 1, 1, 0.2)

check_lp_matches_direct <- function(s) {
  p <- min_es_portfolio(s, 0.95, lo4, hi4, 0)
  lp <- solve_es_lp(s, 0.95, lo4, hi4)
  direct <- expected_shortfall(portfolio_scenarios(s, p$w), 0.95)
  expect_equal(p$es, direct, tolerance = 1e-8)
  expect_equal(lp$es, direct, tolerance = 1e-8)
  expect_equal(sum(p$w), 1, tolerance = 1e-9)
  expect_true(all(p$w >= -1e-9))
}

test_that("LP ES equals direct ES (S = 2000)", {
  check_lp_matches_direct(sim(synthetic_logret()))
})

test_that("LP ES equals direct ES (S = 1001)", {
  check_lp_matches_direct(sim(synthetic_logret(), n = 1001))
})

test_that("min-ES beats every point of a 5% grid", {
  s <- sim(synthetic_logret())
  p <- min_es_portfolio(s, 0.95, lo4, hi4, 0)
  grid_es <- apply(simplex_grid(4, 0.05), 1,
                   function(w) expected_shortfall(portfolio_scenarios(s, w), 0.95))
  expect_lte(p$es, min(grid_es) + 1e-9)
})

test_that("target return is honoured and costs ES", {
  s <- sim(synthetic_logret())
  base <- min_es_portfolio(s, 0.95, lo4, hi4, 0)
  target <- max_return(s, lo4, hi4) * 0.9
  p <- min_es_portfolio(s, 0.95, lo4, hi4, 0, target_return = target)
  expect_gte(p$ret, target - 1e-9)
  expect_gte(p$es, base$es - 1e-9)
})

test_that("per-asset bounds are honoured", {
  s <- sim(synthetic_logret())
  p <- min_es_portfolio(s, 0.95, lo_nt, hi_nt, 0)
  expect_true(all(p$w >= lo_nt - 1e-9))
  expect_true(all(p$w <= hi_nt + 1e-9))
  expect_equal(sum(p$w), 1, tolerance = 1e-9)
  expect_identical(names(p$w), colnames(s))
})

test_that("constant asset is chosen fully", {
  s <- sim(synthetic_logret())
  s[, 1] <- 0.001
  p <- min_es_portfolio(s, 0.95, lo4, hi4, 0)
  expect_equal(unname(p$w[1]), 1, tolerance = 1e-6)
  expect_equal(p$es, -0.001, tolerance = 1e-6)
})

check_ratio_beats_grid <- function(s, lo, hi, rf_h) {
  p <- max_ratio_portfolio(s, 0.95, lo, hi, rf_h)
  W <- apply_bounds(simplex_grid(4, 0.05), lo, hi)
  grid <- apply(W, 1, function(w) excess_ratio(s, w, 0.95, rf_h))
  expect_gte(p$ratio, max(grid) - 1e-9)
  expect_equal(sum(p$w), 1, tolerance = 1e-9)
  expect_true(all(p$w >= lo - 1e-9) && all(p$w <= hi + 1e-9))
}

test_that("max-ratio beats the grid", {
  s <- sim(synthetic_logret())
  check_ratio_beats_grid(s, lo4, hi4, 0)
  check_ratio_beats_grid(s, lo_nt, hi_nt, 0)
  check_ratio_beats_grid(s, lo4, hi4, 0.002)
})

test_that("max-ratio precondition errors", {
  s <- sim(synthetic_logret())
  expect_error(max_ratio_portfolio(s - 1, 0.95, lo4, hi4, 0), "positive expected excess return")
  cash <- s
  cash[, 1] <- 0.01
  expect_error(max_ratio_portfolio(cash, 0.95, lo4, hi4, 0), "non-positive")
})

test_that("max_return respects bounds", {
  s <- sim(synthetic_logret())
  expect_equal(max_return(s, lo4, hi4), max(colMeans(s)), tolerance = 1e-9)
  hi <- hi4
  hi[4] <- 0.2
  expect_lt(max_return(s, lo4, hi), max(colMeans(s)))
})

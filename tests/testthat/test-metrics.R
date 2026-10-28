r10 <- c(-0.10, -0.05, 0, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08)

test_that("expected shortfall hand examples", {
  expect_equal(expected_shortfall(r10, 0.80), 0.075)
  expect_equal(expected_shortfall(r10, 0.75), 0.06)
  expect_equal(expected_shortfall(rep(0.01, 100), 0.95), -0.01)
})

test_that("expected shortfall matches tail mean", {
  set.seed(5)
  r <- rnorm(10000)
  expect_equal(expected_shortfall(r, 0.95), -mean(sort(r)[1:500]))
})

test_that("invalid levels error", {
  expect_error(expected_shortfall(r10, 1), "strictly between")
  expect_error(expected_shortfall(r10, 0), "strictly between")
})

test_that("portfolio helpers are consistent", {
  R <- sim(synthetic_logret(), 500)
  w <- c(0.1, 0.2, 0.3, 0.4)
  p <- portfolio_scenarios(R, w)
  expect_equal(p, as.numeric(R %*% w))
  expect_equal(expected_return(R, w), mean(p))
  expect_equal(excess_ratio(R, w, 0.95, 0), mean(p) / expected_shortfall(p, 0.95))
})

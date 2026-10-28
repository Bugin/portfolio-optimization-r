test_that("simulation has the right shape and column names", {
  s <- sim(synthetic_logret(), n = 1000)
  expect_equal(dim(s), c(1000, 4))
  expect_equal(colnames(s), colnames(synthetic_logret()))
})

test_that("horizon 1 returns are whole historical days", {
  lr <- synthetic_logret()
  s <- simulate_horizon_returns(lr, 1, 200, 3)
  hist <- exp(lr) - 1
  d <- apply(s, 1, function(r) min(apply(hist, 1, function(h) max(abs(h - r)))))
  expect_lt(max(d), 1e-12)
})

test_that("same seed gives identical matrices", {
  lr <- synthetic_logret()
  expect_identical(sim(lr, 500, 7), sim(lr, 500, 7))
})

test_that("perfectly correlated assets stay correlated", {
  lr <- synthetic_logret()
  lr[, 2] <- 2 * lr[, 1]
  s <- simulate_horizon_returns(lr, 1, 1000, 1)
  expect_gt(cor(log1p(s[, 1]), log1p(s[, 2])), 0.9999)
})

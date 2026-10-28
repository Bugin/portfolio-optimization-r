lo4 <- rep(0, 4)
hi4 <- rep(1, 4)

test_that("frontier is monotone, starts at min-ES, has requested size", {
  s <- sim(synthetic_logret())
  m <- min_es_portfolio(s, 0.95, lo4, hi4, 0)
  r_hi <- 0.999 * max_return(s, lo4, hi4)
  f <- build_es_frontier(s, 0.95, lo4, hi4, 0, 20, m$ret, r_hi)
  expect_length(f$es, 20)
  expect_equal(dim(f$W), c(20, 4))
  expect_true(all(diff(f$es) >= -1e-9))
  expect_true(all(diff(f$ret) > 0))
  expect_equal(f$es[1], m$es, tolerance = 1e-8)
})

test_that("max-ratio portfolio lies on the frontier", {
  s <- sim(synthetic_logret())
  t <- max_ratio_portfolio(s, 0.95, lo4, hi4, 0)
  f <- build_es_frontier(s, 0.95, lo4, hi4, 0, 3, t$ret, t$ret + 1e-4)
  expect_equal(f$es[1], t$es, tolerance = 1e-6)
})

test_that("infeasible targets are skipped with a message", {
  s <- sim(synthetic_logret())
  rmax <- max_return(s, lo4, hi4)
  expect_message(f <- build_es_frontier(s, 0.95, lo4, hi4, 0, 3, 0.5 * rmax, 2 * rmax),
                 "[[]frontier[]] skipped")
  expect_true(length(f$es) > 0 && length(f$es) < 3)
})

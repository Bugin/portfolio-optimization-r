test_that("cloud has the right shape", {
  s <- sim(synthetic_logret())
  r <- sample_random_portfolios(s, 0.95, rep(0, 4), rep(1, 4), 300, 0.5, 7)
  expect_equal(colnames(r), c("ret", "es"))
  expect_equal(nrow(r), 300)
  expect_true(all(r$es > -1))
})

test_that("cloud respects the bounds", {
  s <- sim(synthetic_logret())
  lo <- c(0.2, 0, 0, 0)
  hi <- c(1, 1, 1, 0.3)
  r <- sample_random_portfolios(s, 0.95, lo, hi, 300, 0.5, 7)
  expect_equal(nrow(r), 300)
  expect_lte(max(r$ret), max_return(s, lo, hi) + 1e-12)
})

test_that("tight bounds give fewer rows and a message", {
  s <- sim(synthetic_logret())
  expect_message(r <- sample_random_portfolios(s, 0.95, rep(0, 4), rep(0.26, 4), 300, 0.5, 7),
                 "[[]cloud[]] only")
  expect_lte(nrow(r), 300)
})

test_that("es_rows matches expected_shortfall row by row (S = 1001)", {
  s <- sim(synthetic_logret(), n = 1001)
  set.seed(3)
  W <- matrix(runif(20), 5, 4)
  W <- W / rowSums(W)
  P <- W %*% t(s)
  want <- apply(P, 1, function(x) expected_shortfall(x, 0.95))
  expect_equal(es_rows(P, 0.95), want, tolerance = 1e-10)
})

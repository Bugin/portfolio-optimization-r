test_that("horizon_stats matches the hand example", {
  x <- c(-0.10, -0.05, 0, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08)
  m <- matrix(x, ncol = 1, dimnames = list(NULL, "a"))
  st <- horizon_stats(m, 0.8)
  expect_equal(colnames(st), c("mean", "median", "std", "var_80", "es_80",
                               "prob_loss", "min", "max"))
  expect_equal(rownames(st), "a")
  expect_equal(st$es_80, 0.075)
  expect_equal(st$prob_loss, 0.2)
  expect_equal(st$var_80, quantile(x, 0.2, names = FALSE))
  expect_equal(st$std, sd(x))
  st2 <- horizon_stats(m, 0.975)
  expect_true(all(c("var_97.5", "es_97.5") %in% colnames(st2)))
  expect_true(all(c("var_95", "es_95") %in% colnames(horizon_stats(m, 0.95))))
})

test_that("allocation_line passes through the max-ratio point and cash", {
  s <- sim(synthetic_logret())
  rf_h <- 0.002
  t <- max_ratio_portfolio(s, 0.95, rep(0, 4), rep(1, 4), rf_h)
  expect_equal(allocation_line(t, rf_h, t$es), t$ret, tolerance = 1e-9)
  expect_equal(allocation_line(t, rf_h, -rf_h), rf_h)
})

test_that("weights_table has the right shape", {
  s <- sim(synthetic_logret())
  mn <- min_es_portfolio(s, 0.95, rep(0, 4), rep(1, 4), 0.002)
  mr <- max_ratio_portfolio(s, 0.95, rep(0, 4), rep(1, 4), 0.002)
  wt <- weights_table(mn, mr)
  expect_equal(colnames(wt), c("Min-ES", "Max-ratio"))
  expect_equal(rownames(wt), mn$tickers)
  expect_equal(unname(colSums(wt)), c(100, 100), tolerance = 0.05)
})

test_that("both plots build and render headless", {
  s <- sim(synthetic_logret())
  lo <- rep(0, 4); hi <- rep(1, 4); rf_h <- 0.002
  mn <- min_es_portfolio(s, 0.95, lo, hi, rf_h)
  mr <- max_ratio_portfolio(s, 0.95, lo, hi, rf_h)
  fr <- build_es_frontier(s, 0.95, lo, hi, rf_h, 5, mn$ret, max_return(s, lo, hi))
  rand <- data.frame(ret = mn$ret + seq_len(50) * 1e-5, es = mn$es + seq_len(50) * 1e-4)
  asset_ret <- colMeans(s)
  asset_es <- apply(s, 2, expected_shortfall, level = 0.95)
  p <- plot_frontier(rand, fr$es, fr$ret, mn, mr, asset_ret, asset_es, rf_h, 60, 0.95)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  f1 <- file.path(tempdir(), "frontier.png")
  ggplot2::ggsave(f1, p, width = 9, height = 6)
  expect_true(file.exists(f1))

  d <- plot_distributions(cbind(s, "Min-ES" = portfolio_scenarios(s, mn$w)), 60)
  expect_s3_class(d, "ggplot")
  expect_no_error(ggplot2::ggplot_build(d))
  f2 <- file.path(tempdir(), "dist.png")
  ggplot2::ggsave(f2, d, width = 9, height = 6)
  expect_true(file.exists(f2))
  out <- file.path(testthat::test_path("../.."), ".superpowers/sdd/2026-10-28-r-translation")
  if (dir.exists(out)) {
    ggplot2::ggsave(file.path(out, "t5_frontier.png"), p, width = 9, height = 6)
    ggplot2::ggsave(file.path(out, "t5_distributions.png"), d, width = 9, height = 6)
  }
})

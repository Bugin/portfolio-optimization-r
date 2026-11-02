lr <- synthetic_logret()   # 500 calendar days from 2020-01-01 (a Wednesday), weekends included

test_that("daily frequency leaves the log returns untouched", {
  expect_identical(aggregate_logret(lr, "daily"), lr)
  expect_equal(horizon_periods(60, "daily"), 60)
  expect_equal(horizon_label(60, "daily"), "60-day")
})

test_that("weekly log returns are the log ratio of week-end prices", {
  w <- aggregate_logret(lr, "weekly")
  expect_identical(colnames(w), colnames(lr))

  # On a calendar-day series every ISO week has 7 rows and ends on a Sunday,
  # so consecutive weekly rows must be exactly 7 days apart: a check that does
  # not reuse the code's own week labels.
  ends <- as.Date(rownames(w))
  expect_true(all(diff(ends) == 7))
  expect_true(all(format(ends, "%u") == "7"))

  # Rebuild prices from the daily log returns: the weekly return must equal
  # log(P at the week's end / P at the previous week's end).
  prices <- rbind(1, exp(apply(lr, 2, cumsum)))
  rownames(prices) <- c("2019-12-31", rownames(lr))
  want <- log(prices[rownames(w), ] / prices[format(ends - 7), ])
  expect_equal(unname(w), unname(want), tolerance = 1e-12)
})

test_that("weekly aggregation handles a split year-end week and a one-day week", {
  # Hand-built: Thu 2020-12-31 and Fri 2021-01-01 are both in ISO week 2020-53,
  # then a single trading day (Tue 2021-01-05, a week with holidays) in
  # 2021-01, then a full week 2021-02; a first (Wed 2020-12-23) and a last
  # (Mon 2021-01-18) row that must be dropped as partial end weeks.
  days <- c("2020-12-23",                               # week 2020-52 (dropped)
            "2020-12-31", "2021-01-01",                 # week 2020-53
            "2021-01-05",                               # week 2021-01, one day
            "2021-01-11", "2021-01-12", "2021-01-13", "2021-01-14", "2021-01-15",   # 2021-02
            "2021-01-18")                               # week 2021-03 (dropped)
  x <- matrix(seq_along(days) / 100, ncol = 1, dimnames = list(days, "a"))
  w <- aggregate_logret(x, "weekly")
  expect_equal(rownames(w), c("2021-01-01", "2021-01-05", "2021-01-15"))
  expect_equal(unname(w[, 1]), c(0.02 + 0.03, 0.04, sum(5:9) / 100))
})

test_that("the first and last (possibly partial) weeks are dropped", {
  w <- aggregate_logret(lr, "weekly")
  week_of <- format(as.Date(rownames(lr)), "%G-%V")
  weeks_in <- unique(week_of)
  expect_equal(nrow(w), length(weeks_in) - 2)
  w_weeks <- format(as.Date(rownames(w)), "%G-%V")
  expect_false(any(w_weeks %in% weeks_in[c(1, length(weeks_in))]))
})

test_that("horizon conversion and label for weekly returns", {
  expect_equal(horizon_periods(60, "weekly"), 12L)
  expect_equal(horizon_label(60, "weekly"), "12-week (60 business days)")
  expect_error(horizon_periods(61, "weekly"), "whole number of weeks")
  expect_error(horizon_periods(60, "monthly"), "daily.*weekly")
  expect_error(aggregate_logret(lr, "monthly"), "daily.*weekly")
  expect_error(aggregate_logret(unname(lr), "weekly"), "rownames")
})

test_that("weekly scenarios have the right shape", {
  s_w <- simulate_horizon_returns(aggregate_logret(lr, "weekly"), 12, 2000, 1)
  expect_equal(dim(s_w), c(2000, 4))
  expect_identical(colnames(s_w), colnames(lr))
})

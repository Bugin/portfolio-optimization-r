nm <- c("Euro Govt", "Euro Corp IG", "Euro Corp HY", "Europe Equity")

test_that("parameters exist", {
  for (x in c("NAMES", "TICKERS", "BOUNDS", "HORIZON_DAYS", "ES_LEVEL", "check_bounds",
              "load_close", "get_prices_and_logret"))
    expect_true(exists(x), info = x)
  expect_equal(TICKERS, names(NAMES))
})

test_that("check_bounds defaults and order", {
  b <- check_bounds(list("Euro Govt" = c(0.1, 0.5)), c("Euro Corp IG", "Euro Govt"))
  expect_equal(b$lo, c(0, 0.1))
  expect_equal(b$hi, c(1, 0.5))
})

test_that("check_bounds rejects invalid bounds", {
  expect_error(check_bounds(list("Euro Govt" = c(.6, 1), "Euro Corp IG" = c(.6, 1)), nm),
               "Lower bounds sum")
  expect_error(check_bounds(list("Euro Govt" = c(0, .2), "Euro Corp IG" = c(0, .2),
                                 "Euro Corp HY" = c(0, .2), "Europe Equity" = c(0, .2)), nm),
               "Upper bounds sum")
  expect_error(check_bounds(list("Euro Govt" = c(.5, .2)), nm), "Euro Govt.*exceeds")
  expect_error(check_bounds(list("Euro Govt" = c(-.1, 1)), nm), "Euro Govt.*below 0")
  expect_error(check_bounds(list("Euro Govt" = c(0, 1.1)), nm), "Euro Govt.*above 1")
})

test_that("check_bounds rejects unknown asset", {
  expect_error(check_bounds(list("Europe equity" = c(0, .2)), nm), "unknown")
})

test_that("check_bounds accepts sums at the float edge", {
  expect_silent(check_bounds(list("Euro Govt" = c(0, .6), "Euro Corp IG" = c(0, .3),
                                  "Euro Corp HY" = c(0, .1), "Europe Equity" = c(0, 0)), nm))
  expect_silent(check_bounds(list("Euro Govt" = c(.6, 1), "Euro Corp IG" = c(.3, 1),
                                  "Euro Corp HY" = c(.1, 1)), nm))
})

test_that("check_bounds rejects malformed bounds", {
  expect_error(check_bounds(list("Euro Govt" = 0.2), nm), "must be c(min, max)", fixed = TRUE)
  expect_error(check_bounds(list("Euro Govt" = c(NA, 1)), nm), "must be c(min, max)", fixed = TRUE)
})

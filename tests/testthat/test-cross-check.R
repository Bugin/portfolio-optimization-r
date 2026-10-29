# Cross-check against an independent reference implementation: same scenarios
# (tests/cross_check/reference_scenarios.csv), same optima.
# Two frontiers of 50 LPs on 10 000 scenarios: this file takes about a minute.

csv <- test_path("../cross_check/reference_scenarios.csv")
json <- test_path("../cross_check/reference_expected.json")

#' Absolute-tolerance comparison of two scalars.
expect_close <- function(a, b, tol = 1e-6) expect_lt(abs(a - b), tol)

for (label in c("trivial", "tight")) {
  test_that(paste("min-ES, max-ratio and frontier match the reference,", label, "bounds"), {
    skip_if_not(file.exists(csv) && file.exists(json), "cross-check fixtures missing")
    R <- as.matrix(read.csv(csv, check.names = FALSE))
    J <- jsonlite::fromJSON(json, simplifyVector = TRUE)
    expect_equal(colnames(R), J$assets)
    B <- J$bounds[[label]]

    for (kind in c("min_es", "max_ratio")) {
      p <- if (kind == "min_es") {
        min_es_portfolio(R, J$level, B$lo, B$hi, J$rf_h)
      } else {
        max_ratio_portfolio(R, J$level, B$lo, B$hi, J$rf_h)
      }
      e <- B[[kind]]
      expect_lt(max(abs(unname(p$w) - e$w)), 1e-5)
      expect_close(p$ret, e$ret)
      expect_close(p$es, e$es)
      expect_close(p$ratio, e$ratio)
    }

    f <- build_es_frontier(R, J$level, B$lo, B$hi, J$rf_h, J$n_frontier, B$r_lo, B$r_hi)
    expect_length(f$es, length(B$frontier_es))
    expect_lt(max(abs(f$es - B$frontier_es)), 1e-6)
    expect_lt(max(abs(f$ret - B$frontier_ret)), 1e-6)
  })
}

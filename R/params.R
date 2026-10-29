# Parameters of the asset-allocation notebook.
# After changing anything here, re-run the notebook from the top.

# Asset class -> ETF proxy (Xetra, EUR). The common history starts in
# December 2010, when the high-yield ETF was launched.
NAMES <- c(EUNH.DE = "Euro Govt",       # iShares Core EUR Govt Bond
           EUN5.DE = "Euro Corp IG",    # iShares Core EUR Corp Bond
           EUNW.DE = "Euro Corp HY",    # iShares EUR High Yield Corp Bond
           EXSA.DE = "Europe Equity")   # iShares STOXX Europe 600
TICKERS <- names(NAMES)
START_DATE <- "2010-01-01"

HORIZON_DAYS <- 60          # business days, about 3 months
N_SCENARIOS <- 10000        # bootstrap paths
ES_LEVEL <- 0.95            # Expected Shortfall confidence level
N_RANDOM <- 3000            # random portfolios in the cloud
DIRICHLET_ALPHA <- 0.5      # weight concentration of the cloud
N_FRONTIER <- 50            # target returns along the frontier
RISK_FREE <- 0              # annual
SIM_SEED <- 42              # bootstrap paths
CLOUD_SEED <- 7             # random portfolio weights

# Minimum and maximum weight per asset class (fraction of the portfolio).
# Assets not listed get c(0, 1). The notebook `Asset Allocation EUR.Rmd` restates
# BOUNDS in its `parameters` chunk and that value wins when knitting; edit it
# there. This default is used by the tests and by tests/verify_real_data.R.
BOUNDS <- list("Euro Govt" = c(0, 1), "Euro Corp IG" = c(0, 1),
               "Euro Corp HY" = c(0, 1), "Europe Equity" = c(0, 1))

CACHE_DIR <- "data/cache"   # relative to the notebook folder
CACHE_TTL_HOURS <- 12

#' Validate per-asset weight bounds.
#'
#' Returns `list(lo, hi)` as numeric vectors ordered like `names`. Assets missing
#' from `bounds` get c(0, 1). Stops with a message naming the asset when
#' lo < 0, hi > 1 or lo > hi, when `bounds` has unknown assets, or when no fully
#' invested portfolio exists (sum of lows > 1 or sum of highs < 1).
check_bounds <- function(bounds, names) {
  unknown <- setdiff(base::names(bounds), names)
  if (length(unknown) > 0)
    stop("BOUNDS has unknown assets: ", paste(sort(unknown), collapse = ", "))
  lo <- numeric(length(names))
  hi <- numeric(length(names))
  for (i in seq_along(names)) {
    b <- if (names[i] %in% base::names(bounds)) bounds[[names[i]]] else c(0, 1)
    if (!is.numeric(b) || length(b) != 2 || anyNA(b))
      stop(sprintf("BOUNDS[['%s']] must be c(min, max)", names[i]))
    lo[i] <- b[1]
    hi[i] <- b[2]
    if (lo[i] < 0)
      stop(sprintf("BOUNDS['%s']: lower bound %s is below 0", names[i], lo[i]))
    if (hi[i] > 1)
      stop(sprintf("BOUNDS['%s']: upper bound %s is above 1", names[i], hi[i]))
    if (lo[i] > hi[i])
      stop(sprintf("BOUNDS['%s']: lower bound %s exceeds upper bound %s", names[i], lo[i], hi[i]))
  }
  if (sum(lo) > 1 + 1e-9)
    stop(sprintf("Lower bounds sum to %.3f > 1: no fully invested portfolio exists", sum(lo)))
  if (sum(hi) < 1 - 1e-9)
    stop(sprintf("Upper bounds sum to %.3f < 1: no fully invested portfolio exists", sum(hi)))
  list(lo = lo, hi = hi)
}

# Data layer: download of adjusted prices from Yahoo Finance, on-disk cache,
# aligned prices and daily log returns.
#
# Lookup order in load_close(): fresh cache -> download -> stale cache -> error.
# Two rules keep the cache trustworthy:
#   1. the full history is always re-downloaded, never appended: Yahoo restates
#      the whole adjusted series after every dividend or split, so old and new
#      rows would not be on the same basis;
#   2. only a complete download (every ticker present) overwrites the cache,
#      so a transient failure never turns into a permanent one.


#' Download adjusted close prices from Yahoo Finance.
#'
#' Returns a numeric matrix with one row per date (rownames "YYYY-MM-DD") and
#' one column per ticker. "Adjusted" closes already account for dividends
#' and splits, so each series behaves like a total-return index. A ticker
#' that fails or returns nothing gives a column of NA, not an error.
yahoo_download <- function(tickers, start) {
  # --- One series per ticker --------------------------------------------------
  series <- list()
  for (t in tickers) {
    # Any failure (network, unknown ticker, no "Adjusted" column) gives NULL,
    # so one bad ticker never aborts the whole download.
    adjusted <- tryCatch({
      x <- quantmod::getSymbols(t, src = "yahoo", from = start, auto.assign = FALSE)
      a <- quantmod::Ad(x)             # the "Adjusted" column
      colnames(a) <- t
      a
    }, error = function(e) NULL)
    if (is.null(adjusted)) next
    series[[t]] <- adjusted
  }
  if (length(series) == 0)
    return(matrix(NA_real_, 0, length(tickers), dimnames = list(NULL, tickers)))

  # --- Align all series on the union of their dates -------------------------
  merged <- do.call(xts::merge.xts, c(unname(series), list(all = TRUE)))

  # quantmod returns all-NA rows on exchange holidays (e.g. 25 December).
  # Drop them, otherwise forward-filling would turn them into zero-return days.
  merged <- merged[rowSums(!is.na(merged)) > 0, , drop = FALSE]

  # --- Plain matrix with every requested ticker as a column -----------------
  m <- matrix(NA_real_, nrow(merged), length(tickers),
              dimnames = list(format(zoo::index(merged)), tickers))
  m[, colnames(merged)] <- zoo::coredata(merged)
  m
}


#' Cache key: first 12 hex characters of the MD5 of "sorted tickers|start".
#' A different universe or start date therefore gets its own cache file.
.cache_key <- function(tickers, start) {
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(paste(c(sort(tickers), start), collapse = "|"), f, sep = "")
  substr(unname(tools::md5sum(f)), 1, 12)
}

.utc_now <- function() as.POSIXct(Sys.time(), tz = "UTC")


#' Close prices with an on-disk cache.
#'
#' Returns list(close, meta); `meta$source` says where the data came from.
#' `downloader(tickers, start)` must return a numeric matrix with date
#' rownames and ticker colnames; the tests inject a fake one.
load_close <- function(tickers, start = START_DATE, ttl_hours = CACHE_TTL_HOURS,
                       force_refresh = FALSE, cache_dir = CACHE_DIR,
                       downloader = yahoo_download) {
  key <- .cache_key(tickers, start)
  rds_path <- file.path(cache_dir, paste0("close_", key, ".rds"))    # the prices
  meta_path <- file.path(cache_dir, paste0("close_", key, ".json"))  # when they were fetched

  # --- 1. Fresh cache: return it and stop here --------------------------------
  cached <- NULL
  meta <- NULL
  if (file.exists(rds_path) && file.exists(meta_path)) {
    meta <- jsonlite::fromJSON(meta_path, simplifyVector = FALSE)
    cached <- readRDS(rds_path)
    fetched_at <- as.POSIXct(meta$fetched_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    age_hours <- as.numeric(difftime(.utc_now(), fetched_at, units = "hours"))
    if (!force_refresh && age_hours < ttl_hours) {
      meta$source <- sprintf("cache (%.1fh old)", age_hours)
      return(list(close = cached, meta = meta))
    }
  }

  # --- 2. Download, noting which tickers came back empty ---------------------
  res <- tryCatch({
    close <- downloader(tickers, start)
    missing <- setdiff(tickers, colnames(close))                 # not even a column
    all_na <- colnames(close)[colSums(!is.na(close)) == 0]       # a column of NA
    list(close = close, failed = c(intersect(tickers, all_na), missing))
  }, error = function(e) e)

  # --- 3a. Download crashed: fall back to the stale cache, or give up --------
  if (inherits(res, "error")) {
    if (is.null(cached)) stop(res)
    meta$source <- sprintf("stale cache - download failed (%s)", conditionMessage(res))
    return(list(close = cached, meta = meta))
  }

  # --- 3b. Partial download: never overwrite a good cache -------------------
  close <- res$close
  failed <- res$failed
  if (length(failed) > 0) {
    failed_list <- paste0("[", paste(sprintf("'%s'", failed), collapse = ", "), "]")
    if (!is.null(cached)) {
      meta$source <- paste0("cache - partial download, failed ", failed_list)
      return(list(close = cached, meta = meta))
    }
    return(list(close = close, meta = list(source = paste0("partial download, failed ", failed_list))))
  }

  # --- 3c. Complete download: write the cache and return the data -----------
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(close, rds_path)
  writeLines(jsonlite::toJSON(list(
    tickers = sort(tickers), start = start,
    fetched_at = format(.utc_now(), "%Y-%m-%dT%H:%M:%SZ"),
    rows = nrow(close), first = rownames(close)[1], last = rownames(close)[nrow(close)]
  ), auto_unbox = TRUE, pretty = TRUE), meta_path)
  list(close = close, meta = list(source = "download", rows = nrow(close)))
}


#' Aligned close prices and daily log returns.
#'
#' Returns list(prices, logret), both matrices with date rownames. The series
#' are aligned on their common dates, so the sample starts at the youngest
#' listing in the universe. Prints one "[data] <source>" line. Extra
#' arguments go to load_close() (e.g. `downloader`, `cache_dir`).
get_prices_and_logret <- function(tickers, start = START_DATE, force_refresh = FALSE, ...) {
  res <- load_close(tickers, start = start, force_refresh = force_refresh, ...)
  cat(sprintf("[data] %s\n", res$meta$source))
  close <- res$close

  close <- close[order(rownames(close)), , drop = FALSE]          # chronological order
  close <- close[, colSums(!is.na(close)) > 0, drop = FALSE]      # drop empty tickers

  # Forward-fill: a missing price (an asset's exchange closed that day) is
  # replaced by the last available one.
  for (j in seq_len(ncol(close))) {
    x <- close[, j]
    for (i in seq_along(x)[-1]) if (is.na(x[i])) x[i] <- x[i - 1]
    close[, j] <- x
  }

  # Keep only the dates on which every asset has a price. The sample starts
  # where the youngest series starts.
  prices <- close[rowSums(is.na(close)) == 0, , drop = FALSE]
  if (nrow(prices) < 2 || ncol(prices) == 0) stop("Download failed: no usable data")

  # Daily log return: log(P_t / P_{t-1}) = log(P_t) - log(P_{t-1}).
  logret <- diff(log(prices))
  list(prices = prices, logret = logret)
}

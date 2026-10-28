# Data layer: Yahoo download, on-disk cache, aligned prices and log returns.
# Lookup order: fresh cache -> download -> stale cache -> error.
# Rules: always refetch the full history (never append, Yahoo restates adjusted
# prices after every dividend), and only a complete download overwrites the cache.

#' Download adjusted closes from Yahoo Finance.
#'
#' Returns a numeric matrix (dates as `YYYY-MM-DD` rownames, one column per
#' ticker). A ticker that fails or returns no data yields an all-NA column.
yahoo_download <- function(tickers, start) {
  cols <- lapply(tickers, function(t) {
    tryCatch({
      x <- quantmod::getSymbols(t, src = "yahoo", from = start, auto.assign = FALSE)
      ad <- quantmod::Ad(x)
      colnames(ad) <- t
      ad
    }, error = function(e) NULL)
  })
  ok <- !vapply(cols, is.null, logical(1))
  if (!any(ok))
    return(matrix(NA_real_, 0, length(tickers), dimnames = list(NULL, tickers)))
  merged <- do.call(xts::merge.xts, c(cols[ok], list(all = TRUE)))
  m <- matrix(NA_real_, nrow(merged), length(tickers),
              dimnames = list(format(zoo::index(merged)), tickers))
  m[, colnames(merged)] <- zoo::coredata(merged)
  m
}

#' First 12 hex characters of the MD5 of "sorted tickers|start".
.cache_key <- function(tickers, start) {
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(paste(c(sort(tickers), start), collapse = "|"), f, sep = "")
  substr(unname(tools::md5sum(f)), 1, 12)
}

.utc_now <- function() as.POSIXct(Sys.time(), tz = "UTC")

#' Close prices with an on-disk cache.
#'
#' Returns `list(close, meta)`; `meta$source` says where the data came from.
#' `downloader(tickers, start)` must return a numeric matrix with date rownames
#' and ticker colnames; it is injectable for tests.
load_close <- function(tickers, start = START_DATE, ttl_hours = CACHE_TTL_HOURS,
                       force_refresh = FALSE, cache_dir = CACHE_DIR,
                       downloader = yahoo_download) {
  key <- .cache_key(tickers, start)
  rds_path <- file.path(cache_dir, paste0("close_", key, ".rds"))
  meta_path <- file.path(cache_dir, paste0("close_", key, ".json"))

  cached <- meta <- NULL
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

  res <- tryCatch({
    close <- downloader(tickers, start)
    missing <- setdiff(tickers, colnames(close))
    all_na <- colnames(close)[colSums(!is.na(close)) == 0]
    list(close = close, failed = c(intersect(tickers, all_na), missing))
  }, error = function(e) e)

  if (inherits(res, "error")) {
    if (is.null(cached)) stop(res)
    meta$source <- sprintf("stale cache - download failed (%s)", conditionMessage(res))
    return(list(close = cached, meta = meta))
  }

  close <- res$close
  failed <- res$failed
  if (length(failed) > 0) {
    fl <- paste0("[", paste(sprintf("'%s'", failed), collapse = ", "), "]")
    if (!is.null(cached)) {
      meta$source <- paste0("cache - partial download, failed ", fl)
      return(list(close = cached, meta = meta))
    }
    return(list(close = close, meta = list(source = paste0("partial download, failed ", fl))))
  }

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
#' Series are aligned on common dates, so the sample starts at the youngest
#' listing in the universe. Prints one `[data] <source>` line. `...` goes to
#' `load_close` (e.g. `downloader`, `cache_dir`).
get_prices_and_logret <- function(tickers, start = START_DATE, force_refresh = FALSE, ...) {
  res <- load_close(tickers, start = start, force_refresh = force_refresh, ...)
  cat(sprintf("[data] %s\n", res$meta$source))
  close <- res$close
  close <- close[order(rownames(close)), , drop = FALSE]
  close <- close[, colSums(!is.na(close)) > 0, drop = FALSE]
  for (j in seq_len(ncol(close))) {          # forward fill
    x <- close[, j]
    for (i in seq_along(x)[-1]) if (is.na(x[i])) x[i] <- x[i - 1]
    close[, j] <- x
  }
  prices <- close[rowSums(is.na(close)) == 0, , drop = FALSE]
  if (nrow(prices) < 2 || ncol(prices) == 0) stop("Download failed: no usable data")
  logret <- diff(log(prices))
  list(prices = prices, logret = logret)
}

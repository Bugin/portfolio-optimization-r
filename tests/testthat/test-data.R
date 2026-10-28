dates10 <- format(as.Date("2024-01-01") + 0:9)
fake_close <- function() {
  m <- cbind(A = 100 * cumprod(c(1, rep(1.01, 9))), B = 50 * cumprod(c(1, rep(0.995, 9))))
  rownames(m) <- dates10
  m
}
good_dl <- function(tickers, start) fake_close()
new_cache <- function() {
  d <- tempfile("cache"); dir.create(d); d
}

test_that("load_close downloads, writes cache, then serves it", {
  d <- new_cache()
  r1 <- load_close(c("A", "B"), cache_dir = d, downloader = good_dl)
  expect_equal(r1$meta$source, "download")
  expect_equal(length(list.files(d, "[.]rds$")), 1)
  expect_equal(length(list.files(d, "[.]json$")), 1)
  r2 <- load_close(c("B", "A"), cache_dir = d, downloader = function(...) stop("no"))
  expect_true(startsWith(r2$meta$source, "cache"))
  expect_equal(r2$close, fake_close())
})

test_that("load_close falls back to stale cache when download fails", {
  d <- new_cache()
  load_close(c("A", "B"), cache_dir = d, downloader = good_dl)
  r <- load_close(c("A", "B"), cache_dir = d, force_refresh = TRUE,
                  downloader = function(...) stop("boom"))
  expect_true(startsWith(r$meta$source, "stale cache"))
  expect_equal(r$close, fake_close())
})

test_that("load_close errors when download fails and no cache", {
  expect_error(load_close(c("A", "B"), cache_dir = new_cache(),
                          downloader = function(...) stop("boom")), "boom")
})

test_that("partial download does not overwrite the cache", {
  d <- new_cache()
  load_close(c("A", "B"), cache_dir = d, downloader = good_dl)
  partial <- function(tickers, start) { m <- fake_close(); m[, "B"] <- NA_real_; m }
  r <- load_close(c("A", "B"), cache_dir = d, force_refresh = TRUE, downloader = partial)
  expect_true(startsWith(r$meta$source, "cache - partial download"))
  expect_equal(r$close, fake_close())
  r2 <- load_close(c("A", "B"), cache_dir = d, downloader = function(...) stop("no"))
  expect_equal(r2$close, fake_close())
})

test_that("partial download without cache is returned and not written", {
  d <- new_cache()
  partial <- function(tickers, start) { m <- fake_close(); m[, "B"] <- NA_real_; m }
  r <- load_close(c("A", "B"), cache_dir = d, downloader = partial)
  expect_true(startsWith(r$meta$source, "partial download"))
  expect_equal(length(list.files(d)), 0)
})

test_that("get_prices_and_logret returns log returns", {
  out <- capture.output(r <- get_prices_and_logret(c("A", "B"), cache_dir = new_cache(),
                                                   downloader = good_dl))
  expect_match(out, "^[[]data[]] download")
  expect_equal(nrow(r$logret), 9)
  expect_false(anyNA(r$logret))
  expect_equal(colnames(r$logret), c("A", "B"))
  expect_equal(unname(r$logret[1, "A"]), log(1.01))
  expect_equal(rownames(r$logret), dates10[-1])
})

test_that("get_prices_and_logret drops all-NA columns", {
  dl <- function(tickers, start) { m <- fake_close(); m <- cbind(m, C = NA_real_); m }
  capture.output(r <- get_prices_and_logret(c("A", "B", "C"), cache_dir = new_cache(),
                                            downloader = dl))
  expect_equal(colnames(r$prices), c("A", "B"))
  expect_equal(ncol(r$logret), 2)
})

test_that("get_prices_and_logret forward-fills and errors on empty", {
  dl <- function(tickers, start) { m <- fake_close(); m[5, "B"] <- NA; m }
  capture.output(r <- get_prices_and_logret(c("A", "B"), cache_dir = new_cache(), downloader = dl))
  expect_equal(nrow(r$logret), 9)
  empty <- function(tickers, start) matrix(NA_real_, 3, 2, dimnames = list(dates10[1:3], c("A", "B")))
  expect_error(capture.output(get_prices_and_logret(c("A", "B"), cache_dir = new_cache(),
                                                    downloader = empty)), "no usable data")
})

test_that("yahoo_download drops all-NA holiday rows before they reach prices/logret", {
  holiday <- "2024-01-05"
  fake_getSymbols <- function(t, ...) {
    d <- as.Date(dates10)
    x <- xts::xts(cbind(seq_along(d) + if (t == "A") 100 else 50), order.by = d)
    x[holiday, 1] <- NA
    colnames(x) <- paste0(t, ".Adjusted")
    x
  }
  testthat::local_mocked_bindings(getSymbols = fake_getSymbols, .package = "quantmod")
  m <- yahoo_download(c("A", "B"), "2024-01-01")
  expect_false(holiday %in% rownames(m))
  expect_equal(nrow(m), 9)
  capture.output(res <- get_prices_and_logret(c("A", "B"), cache_dir = new_cache(), downloader = yahoo_download))
  expect_false(holiday %in% rownames(res$prices))
  expect_false(holiday %in% rownames(res$logret))
  expect_true(all(res$logret != 0))
})

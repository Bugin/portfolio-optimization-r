#' Row-wise exact ES (same definition as `expected_shortfall`) of a
#' portfolios x scenarios matrix of returns.
es_rows <- function(P, level) {
  S <- ncol(P)
  m <- (1 - level) * S
  if (m <= 0 || level <= 0 || m > S) stop("level must lie strictly between 0 and 1")
  k <- floor(m)
  apply(-P, 1, function(x) {
    top <- sort(x, decreasing = TRUE)
    (sum(top[seq_len(k)]) + (m - k) * (if (k < S) top[k + 1] else 0)) / m
  })
}

#' Expected return and ES of `n` random portfolios within the per-asset bounds.
#'
#' Dirichlet draws are shifted so the lower bounds hold by construction and
#' rejected when they break an upper bound. Returns data.frame(ret, es).
sample_random_portfolios <- function(R, level, lo, hi, n, alpha, seed) {
  set.seed(seed)
  p <- length(lo)
  kept <- list()
  total <- 0
  first <- TRUE
  while (total < n) {
    g <- matrix(rgamma(n * p, alpha), n, p)
    d <- g / rowSums(g)
    W <- sweep(d * (1 - sum(lo)), 2, lo, "+")
    W <- W[rowSums(sweep(W, 2, hi + 1e-12, ">")) == 0, , drop = FALSE]
    kept[[length(kept) + 1]] <- W
    total <- total + nrow(W)
    if (first && nrow(W) < 0.05 * n) {
      message(sprintf("[cloud] only %d of %d random portfolios satisfy the bounds",
                      nrow(W), n))
      break
    }
    first <- FALSE
  }
  W <- do.call(rbind, kept)
  W <- W[seq_len(min(n, nrow(W))), , drop = FALSE]
  ret <- numeric(0)
  es <- numeric(0)
  for (i in 500 * seq_len(ceiling(nrow(W) / 500)) - 499) {
    P <- W[i:min(i + 499, nrow(W)), , drop = FALSE] %*% t(R)
    ret <- c(ret, rowMeans(P))
    es <- c(es, es_rows(P, level))
  }
  data.frame(ret = ret, es = es)
}

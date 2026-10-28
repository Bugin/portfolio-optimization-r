# Linear programs (HiGHS). highs::highs_solve status codes: 7 = "Optimal",
# 8 = "Infeasible"; we test on status_message == "Optimal".

#' Pad a constraint over the weights with `extra` zero columns; returns sparse A.
pad_w <- function(A, extra) {
  A <- Matrix::Matrix(matrix(as.numeric(A), nrow = if (is.matrix(A)) nrow(A) else 1), sparse = TRUE)
  cbind(A, Matrix::Matrix(0, nrow(A), extra, sparse = TRUE))
}

#' Minimise the ES of a portfolio (Rockafellar-Uryasev LP).
#'
#' Variables are [w (n), zeta (1), u (S)]; scenario rows -R w - zeta - u <= 0.
#' `extra_ub` and `extra_eq` are list(A, b) constraints over `w` only.
#' Returns list(w, es).
solve_es_lp <- function(R, level, w_lower, w_upper, extra_eq = NULL, extra_ub = NULL,
                        budget = TRUE) {
  S <- nrow(R)
  n <- ncol(R)
  obj <- c(rep(0, n), 1, rep(1 / ((1 - level) * S), S))
  A <- cbind(-Matrix::Matrix(R, sparse = TRUE),
             Matrix::Matrix(-1, S, 1, sparse = TRUE),
             -Matrix::Diagonal(S))
  lhs <- rep(-Inf, S)
  rhs <- rep(0, S)
  if (!is.null(extra_ub)) {
    Au <- pad_w(extra_ub[[1]], 1 + S)
    A <- rbind(A, Au)
    lhs <- c(lhs, rep(-Inf, nrow(Au)))
    rhs <- c(rhs, extra_ub[[2]])
  }
  if (budget) {
    A <- rbind(A, Matrix::Matrix(matrix(c(rep(1, n), rep(0, 1 + S)), nrow = 1), sparse = TRUE))
    lhs <- c(lhs, 1)
    rhs <- c(rhs, 1)
  }
  if (!is.null(extra_eq)) {
    Ae <- pad_w(extra_eq[[1]], 1 + S)
    A <- rbind(A, Ae)
    lhs <- c(lhs, extra_eq[[2]])
    rhs <- c(rhs, extra_eq[[2]])
  }
  res <- highs::highs_solve(L = obj, lower = c(w_lower, -Inf, rep(0, S)),
                            upper = c(w_upper, Inf, rep(Inf, S)),
                            A = A, lhs = lhs, rhs = rhs)
  if (!identical(res$status_message, "Optimal")) stop(res$status_message)
  list(w = res$primal_solution[seq_len(n)], es = res$objective_value)
}

#' Result list for weights `w` on scenarios `R`; es/ret/ratio recomputed on `w`.
portfolio_result <- function(R, w, level, rf_h) {
  names(w) <- colnames(R)
  list(w = w, ret = expected_return(R, w),
       es = expected_shortfall(portfolio_scenarios(R, w), level),
       ratio = excess_ratio(R, w, level, rf_h), tickers = colnames(R))
}

#' Minimum-ES portfolio within the per-asset bounds, optionally with a minimum
#' expected return.
min_es_portfolio <- function(R, level, lo, hi, rf_h, target_return = NULL) {
  extra_ub <- if (is.null(target_return)) NULL else list(-colMeans(R), -target_return)
  w <- solve_es_lp(R, level, lo, hi, extra_ub = extra_ub)$w
  portfolio_result(R, w, level, rf_h)
}

#' Maximum expected return under the per-asset bounds and the budget constraint.
max_return <- function(R, lo, hi) {
  n <- ncol(R)
  res <- highs::highs_solve(L = -colMeans(R), lower = lo, upper = hi,
                            A = matrix(1, 1, n), lhs = 1, rhs = 1)
  if (!identical(res$status_message, "Optimal")) stop(res$status_message)
  -res$objective_value
}

#' Maximise expected excess return over the ES of excess returns with one LP.
#'
#' Charnes-Cooper scaling: ES is positively homogeneous in the weights, so we
#' minimise the ES of excess returns over y = t * w subject to E[excess] = 1
#' (instead of sum(w) = 1), with the bounds made homogeneous
#' (lo_i * sum(y) <= y_i <= hi_i * sum(y)); then w = y / sum(y).
max_ratio_portfolio <- function(R, level, lo, hi, rf_h) {
  if (max_return(R, lo, hi) - rf_h <= 0)
    stop("No feasible portfolio with positive expected excess return")
  n <- ncol(R)
  R_ex <- R - rf_h
  rbar_ex <- colMeans(R_ex)
  A_b <- rbind(lo %o% rep(1, n) - diag(n), diag(n) - hi %o% rep(1, n))
  lp <- solve_es_lp(R_ex, level, w_lower = rep(0, n), w_upper = rep(Inf, n),
                    extra_eq = list(rbar_ex, 1),
                    extra_ub = list(A_b, rep(0, 2 * n)), budget = FALSE)
  if (lp$es <= 0)
    stop("A feasible portfolio has non-positive Expected Shortfall of excess returns: ",
         "the return/ES ratio is unbounded")
  portfolio_result(R, lp$w / sum(lp$w), level, rf_h)
}

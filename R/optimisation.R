# Linear programs, solved with HiGHS through the `highs` package.
#
# Every optimisation in this project is a linear program (LP). A linear
# program minimises a linear objective  c'x  over a vector of variables x,
# subject to linear constraints  lhs <= A x <= rhs  and simple bounds
# lower <= x <= upper. `highs::highs_solve()` takes exactly these pieces:
#
#   highs_solve(L = c, lower, upper, A, lhs, rhs)
#
# and returns a list with `status_message` ("Optimal" when it succeeded),
# `primal_solution` (the optimal x) and `objective_value` (c'x at the optimum).
# Status codes for reference: 7 = "Optimal", 8 = "Infeasible".


#' Minimise the Expected Shortfall of a portfolio (Rockafellar-Uryasev LP).
#'
#' Rockafellar and Uryasev (2000) showed that the ES at level alpha equals
#'
#'     min over zeta of   zeta + mean( max(L_s - zeta, 0) ) / (1 - alpha)
#'
#' where L_s = -(R_s . w) is the portfolio loss in scenario s. The max(., 0)
#' is replaced by one auxiliary variable u_s per scenario with the constraints
#' u_s >= L_s - zeta and u_s >= 0: because u_s is minimised, it settles on
#' exactly max(L_s - zeta, 0). The problem is then linear in all variables.
#'
#' Variable vector (length n + 1 + S):
#'
#'     x = [ w_1 ... w_n | zeta | u_1 ... u_S ]
#'           weights       threshold (at the optimum: the VaR of the loss)
#'                                  one slack per scenario
#'
#' `R` is the S x n scenario matrix of returns (simple returns, or excess
#' returns over the risk-free rate in max_ratio_portfolio). `w_lower`/`w_upper`
#' are the per-asset bounds. `extra_ub` and `extra_eq` are optional
#' constraints on the weights only, given as list(A, b): rows A w <= b and
#' A w == b respectively. `budget = TRUE` adds sum(w) = 1.
#' Returns list(w, es) with the optimal weights and the optimal ES.
solve_es_lp <- function(R, level, w_lower, w_upper, extra_eq = NULL, extra_ub = NULL,
                        budget = TRUE) {
  S <- nrow(R)   # number of scenarios
  n <- ncol(R)   # number of assets

  # --- Objective: zeta + (1 / ((1 - alpha) S)) * sum(u) ---------------------
  # Weights have coefficient 0, zeta has 1, every u_s has 1 / ((1 - alpha) S).
  objective <- c(rep(0, n), 1, rep(1 / ((1 - level) * S), S))

  # --- Scenario rows: -R_s . w - zeta - u_s <= 0  (one row per scenario) ----
  # Written as a sparse matrix because the identity block is S x S.
  # Columns: [ -R (S x n) | -1 (S x 1) | -I (S x S) ]
  A <- cbind(-Matrix::Matrix(R, sparse = TRUE),
             Matrix::Matrix(-1, S, 1, sparse = TRUE),
             -Matrix::Diagonal(S))
  lhs <- rep(-Inf, S)   # no lower limit on these rows
  rhs <- rep(0, S)      # ... <= 0

  # Helpers for the extra rows. A constraint on the weights only needs zero
  # columns for zeta and for every u_s to fit the full variable vector.
  # as_rows() turns a plain vector into a 1-row sparse matrix and leaves a
  # matrix with several rows (e.g. the bound rows of the max-ratio LP) as is.
  as_rows <- function(M) {
    M <- matrix(as.numeric(M), nrow = if (is.matrix(M)) nrow(M) else 1)
    Matrix::Matrix(M, sparse = TRUE)
  }
  zeros_for_zeta_and_u <- function(n_rows) Matrix::Matrix(0, n_rows, 1 + S, sparse = TRUE)

  # --- Optional inequality rows on the weights: A_ub w <= b_ub --------------
  if (!is.null(extra_ub)) {
    A_ub <- as_rows(extra_ub[[1]])
    A <- rbind(A, cbind(A_ub, zeros_for_zeta_and_u(nrow(A_ub))))
    lhs <- c(lhs, rep(-Inf, nrow(A_ub)))
    rhs <- c(rhs, extra_ub[[2]])
  }

  # --- Budget row: w_1 + ... + w_n = 1 ----------------------------------------
  if (budget) {
    budget_row <- as_rows(rep(1, n))
    A <- rbind(A, cbind(budget_row, zeros_for_zeta_and_u(1)))
    lhs <- c(lhs, 1)
    rhs <- c(rhs, 1)
  }

  # --- Optional equality rows on the weights: A_eq w = b_eq -----------------
  if (!is.null(extra_eq)) {
    A_eq <- as_rows(extra_eq[[1]])
    A <- rbind(A, cbind(A_eq, zeros_for_zeta_and_u(nrow(A_eq))))
    lhs <- c(lhs, extra_eq[[2]])   # equality: lhs == rhs
    rhs <- c(rhs, extra_eq[[2]])
  }

  # --- Variable bounds: weights within [lower, upper], zeta free, u >= 0 ----
  lower <- c(w_lower, -Inf, rep(0, S))
  upper <- c(w_upper, Inf, rep(Inf, S))

  # --- Solve and read the result --------------------------------------------
  res <- highs::highs_solve(L = objective, lower = lower, upper = upper,
                            A = A, lhs = lhs, rhs = rhs)
  if (!identical(res$status_message, "Optimal")) stop(res$status_message)

  weights <- res$primal_solution[seq_len(n)]   # the first n variables are the weights
  list(w = weights, es = res$objective_value)
}


#' Package the weights `w` with the statistics the notebook reports.
#'
#' Expected return, ES and return/ES ratio are recomputed directly on `w`
#' (not read from the LP), so every portfolio is described the same way.
portfolio_result <- function(R, w, level, rf_h) {
  names(w) <- colnames(R)
  list(w = w,
       ret = expected_return(R, w),
       es = expected_shortfall(portfolio_scenarios(R, w), level),
       ratio = excess_ratio(R, w, level, rf_h),
       tickers = colnames(R))
}


#' Minimum-ES portfolio within the per-asset bounds.
#'
#' With `target_return` the portfolio must also have an expected return of at
#' least that value: this is how each point of the frontier is computed.
min_es_portfolio <- function(R, level, lo, hi, rf_h, target_return = NULL) {
  # The LP accepts "<=" rows only, so "mean(R) . w >= target" is written as
  # "-mean(R) . w <= -target".
  if (is.null(target_return)) {
    extra_ub <- NULL
  } else {
    extra_ub <- list(-colMeans(R), -target_return)
  }
  lp <- solve_es_lp(R, level, lo, hi, extra_ub = extra_ub)
  portfolio_result(R, lp$w, level, rf_h)
}


#' Highest expected return attainable under the bounds and the budget.
#'
#' A tiny LP: maximise mean(R) . w (HiGHS minimises, so the sign is flipped)
#' subject to sum(w) = 1 and lo <= w <= hi. Used to set the top of the frontier.
max_return <- function(R, lo, hi) {
  n <- ncol(R)
  res <- highs::highs_solve(L = -colMeans(R), lower = lo, upper = hi,
                            A = matrix(1, 1, n), lhs = 1, rhs = 1)
  if (!identical(res$status_message, "Optimal")) stop(res$status_message)
  -res$objective_value
}


#' Portfolio with the highest ratio  (E[R] - rf) / ES(R - rf)  in one LP.
#'
#' Step 1: rewrite the ratio with excess returns. Because sum(w) = 1,
#' E[R] - rf = mean(R_ex) . w with R_ex = R - rf, and the denominator is
#' ES(R_ex . w). In this form numerator and denominator are both
#' *homogeneous* in the weights: doubling all weights doubles both, leaving
#' the ratio unchanged.
#'
#' Step 2 (Charnes-Cooper, 1962): since only the direction of w matters, we
#' can drop sum(w) = 1, fix the numerator to 1 and minimise the denominator:
#'
#'     minimise ES(R_ex . y)   subject to   mean(R_ex) . y = 1,  y >= 0
#'
#' where y = t * w is a scaled copy of the weights. The optimal ratio is
#' 1 / ES(y) and the weights are w = y / sum(y). The per-asset bounds
#' lo_i <= w_i <= hi_i become, in terms of y (multiplying by sum(y) > 0):
#'
#'     lo_i * sum(y) - y_i <= 0        and        y_i - hi_i * sum(y) <= 0
#'
#' which are linear in y as well.
max_ratio_portfolio <- function(R, level, lo, hi, rf_h) {
  # Precondition 1: the normalisation mean(R_ex) . y = 1 needs some feasible
  # portfolio with positive expected excess return.
  if (max_return(R, lo, hi) - rf_h <= 0)
    stop("No feasible portfolio with positive expected excess return")

  n <- ncol(R)
  R_ex <- R - rf_h              # excess returns, scenario by scenario
  mean_ex <- colMeans(R_ex)     # expected excess return per asset

  # Bound rows in the scaled variable y: one row per asset for the lower
  # bound, then one row per asset for the upper bound (2n rows, rhs 0).
  A_bounds <- matrix(0, 2 * n, n)
  for (i in seq_len(n)) {
    A_bounds[i, ] <- lo[i]          # lo_i * sum(y) ...
    A_bounds[i, i] <- lo[i] - 1     # ... - y_i            <= 0
    A_bounds[n + i, ] <- -hi[i]     # -hi_i * sum(y) ...
    A_bounds[n + i, i] <- 1 - hi[i] # ... + y_i            <= 0
  }

  lp <- solve_es_lp(R_ex, level,
                    w_lower = rep(0, n), w_upper = rep(Inf, n),   # y >= 0, no cap
                    extra_eq = list(mean_ex, 1),                  # numerator = 1
                    extra_ub = list(A_bounds, rep(0, 2 * n)),     # bounds on w
                    budget = FALSE)                               # no sum(y) = 1

  # Precondition 2: the trick needs a positive denominator at the optimum.
  # ES <= 0 means that some feasible portfolio has, on average over its
  # worst scenarios, an excess return of zero or more: the ratio is then
  # unbounded and the LP would return a wrong point.
  if (lp$es <= 0)
    stop("A feasible portfolio has non-positive Expected Shortfall of excess returns: ",
         "the return/ES ratio is unbounded")

  y <- lp$w
  w <- y / sum(y)                 # back to weights that sum to 1
  portfolio_result(R, w, level, rf_h)
}


#' Mean-ES efficient frontier.
#'
#' For `n_points` target returns between `r_lo` and `r_hi`, the minimum-ES
#' portfolio with at least that expected return. A target for which the
#' optimisation fails (typically: infeasible) is skipped with a message.
#' Returns list(es, ret, W): the ES and
#' expected return of each point and the weights, one row per point.
build_es_frontier <- function(R, level, lo, hi, rf_h, n_points, r_lo, r_hi) {
  targets <- seq(r_lo, r_hi, length.out = n_points)
  es <- numeric(0)
  ret <- numeric(0)
  W <- matrix(numeric(0), 0, ncol(R))
  for (target in targets) {
    p <- tryCatch(
      min_es_portfolio(R, level, lo, hi, rf_h, target_return = target),
      error = function(e) {
        message(sprintf("[frontier] skipped target r=%.4f%%: %s", 100 * target,
                        conditionMessage(e)))
        NULL
      })
    if (is.null(p)) next
    es <- c(es, p$es)
    ret <- c(ret, p$ret)
    W <- rbind(W, unname(p$w))
  }
  list(es = es, ret = ret, W = W)
}

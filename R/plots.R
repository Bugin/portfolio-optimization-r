# Charts (ggplot2). Each function returns a ggplot object; the notebook prints
# it and also saves it with ggsave().


#' The main chart: random cloud, mean-ES frontier, single assets, the two
#' optimal portfolios and the allocation line, in the (ES, expected return)
#' plane at the horizon.
plot_frontier <- function(rand, frontier_es, frontier_ret, min_es, max_ratio,
                          asset_ret, asset_es, rf_h, horizon, level) {
  # --- Series names and colours (the legend follows this order) -------------
  rf_name <- sprintf("Risk-free (%.2f%%)", rf_h * 100)
  # The strings below use escapes for the en dash (u2013), the plus-minus sign (u00b1)
  # and sigma (u03c3), so the file stays plain ASCII and renders the same in every locale.
  series <- c("Random portfolios", "Mean\u2013ES frontier", "Individual assets",
              "Min-ES", "Max-ratio", "Allocation line", rf_name)
  cols <- c("blue", "#1f77b4", "grey20", "gold", "darkred", "#ff7f0e", "black")
  names(cols) <- series

  # --- One small data frame per series, all with columns es, ret, series ----
  cloud <- data.frame(es = rand$es, ret = rand$ret,
                      series = rep(series[1], nrow(rand)))     # may have 0 rows

  ord <- order(frontier_es)                                    # draw the frontier left to right
  front <- data.frame(es = frontier_es[ord], ret = frontier_ret[ord], series = series[2])

  assets <- data.frame(es = unname(asset_es), ret = unname(asset_ret),
                       label = names(asset_es), series = series[3])

  mn <- data.frame(es = min_es$es, ret = min_es$ret, series = series[4])
  mr <- data.frame(es = max_ratio$es, ret = max_ratio$ret, series = series[5])

  # The allocation line runs from the risk-free point to a bit beyond the
  # riskiest thing on the chart.
  xmax <- 1.05 * max(asset_es, frontier_es, if (nrow(rand)) rand$es else 0, max_ratio$es)
  xs <- seq(-rf_h, xmax, length.out = 120)
  line <- data.frame(es = xs, ret = allocation_line(max_ratio, rf_h, xs), series = series[6])

  rf <- data.frame(es = -rf_h, ret = rf_h, series = series[7])   # cash sits at (-rf_h, rf_h)

  # Asset labels, plus the three marked points with an empty label so that
  # ggrepel pushes the text away from them too.
  labels <- rbind(assets[c("es", "ret", "label")],
                  data.frame(es = c(min_es$es, max_ratio$es, -rf_h),
                             ret = c(min_es$ret, max_ratio$ret, rf_h), label = ""))

  aes_xy <- ggplot2::aes(es, ret, colour = series)

  # --- Layers, from background to foreground --------------------------------
  ggplot2::ggplot() +
    ggplot2::geom_point(aes_xy, cloud, size = 0.6, alpha = 0.2) +
    ggplot2::geom_path(aes_xy, front, linewidth = 1) +
    ggplot2::geom_line(aes_xy, line, linetype = "dashed") +
    ggplot2::geom_point(aes_xy, assets, shape = 21, fill = "white", size = 2.6) +
    ggplot2::geom_point(aes_xy, mn, size = 4) +
    ggplot2::geom_point(aes_xy, mr, shape = 23, fill = "red", size = 4) +
    ggplot2::geom_point(aes_xy, rf, size = 3.5) +
    ggrepel::geom_text_repel(ggplot2::aes(es, ret, label = label), labels,
                             size = 2.6, colour = "grey20", segment.colour = "grey60",
                             bg.colour = "white", bg.r = 0.15,     # white halo
                             point.padding = 0.4, box.padding = 0.4,
                             max.overlaps = Inf) +
    # --- Legend, axes, titles ------------------------------------------------
    ggplot2::scale_colour_manual(values = cols, limits = series, name = NULL) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(
      alpha = 1, size = c(2.5, 1, 2.6, 4, 4, 1, 3.5)))) +      # readable legend keys
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(x = sprintf("Expected Shortfall %s%% at the horizon (loss)",
                              format(level * 100)),
                  y = "Expected return at the horizon",
                  title = sprintf("Mean\u2013ES frontier, %d-day horizon", horizon)) +
    ggplot2::theme_minimal()
}


#' Histogram of the horizon returns of each column of `returns`, with the
#' normal density of the same mean and standard deviation drawn on top.
#'
#' Each panel spans mean +/- `n_sigma` standard deviations of its own series,
#' so the shape (skew, fat tails) is readable whatever the volatility.
plot_distributions <- function(returns, horizon, n_sigma = 4) {
  series_names <- colnames(returns)

  # --- Build the data: clipped observations and a normal curve per series ---
  hist_df <- list()
  line_df <- list()
  for (nm in series_names) {
    x <- returns[, nm]
    m <- mean(x)
    s <- sd(x)
    lo <- m - n_sigma * s
    hi <- m + n_sigma * s
    grid <- seq(lo, hi, length.out = 300)
    hist_df[[nm]] <- data.frame(series = nm, x = pmin(pmax(x, lo), hi))   # clip to the window
    line_df[[nm]] <- data.frame(series = nm, x = grid, d = dnorm(grid, m, s))
  }
  hist_df <- do.call(rbind, hist_df)
  line_df <- do.call(rbind, line_df)
  # Keep the panels in column order rather than alphabetical.
  hist_df$series <- factor(hist_df$series, levels = series_names)
  line_df$series <- factor(line_df$series, levels = series_names)

  # --- Plot: one panel per series, density histogram + normal fit -----------
  ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x, y = ggplot2::after_stat(density)), hist_df,
                            bins = 60, fill = "steelblue", alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(x, d), line_df, colour = "grey30", linewidth = 0.4) +
    ggplot2::facet_wrap(~ series, ncol = 3, scales = "free") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(signif(x * 100, 2), "%")) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = sprintf("Simulated %d-day returns, \u00b1%d\u03c3 per panel (normal fit in grey)",
                                  horizon, n_sigma)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 7))
}

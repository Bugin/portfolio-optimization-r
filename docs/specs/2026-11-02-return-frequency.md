# Return frequency: daily or weekly resampling

Date: 2026-11-02. Status: implemented.

## Goal

Let one parameter, `RETURN_FREQ` in `R/params.R`, choose whether the bootstrap
resamples historical **days** (current behaviour) or historical **weeks**.
With `"daily"` every number the project prints stays bit-identical.

## Design

- **Aggregation, not a second download.** `aggregate_logret(logret, freq)` in
  `R/data.R` turns the daily log-return matrix into a weekly one by summing the
  daily log returns that fall in the same ISO week (`format(date, "%G-%V")`).
  The sum equals the log return between the last prices of consecutive weeks.
  Weeks shortened by holidays stay weeks; the first and the last week of the
  sample are always dropped because they may be partial (at most two weeks
  lost, sometimes complete ones). Each weekly row is
  dated at its last trading day. For `"daily"` the function returns its input.
- **Horizon stays in business days.** `HORIZON_DAYS` remains the reference and
  `rf_h` (converted with /252) does not depend on the frequency.
  `horizon_periods(HORIZON_DAYS, RETURN_FREQ)` gives the number of periods to
  resample: the days themselves, or days / 5, which must be an integer (60
  days = 12 weeks). Known gap: a resampled week has on average about 4.86
  trading days (holidays), so 12 weeks cover about 58 trading days, not 60;
  drift and dispersion at the horizon are about 3 % lower than in daily mode.
  Documented rather than corrected, because the empirical equivalent (about
  12.3 weeks) is not an integer.
- **Labels.** `horizon_label(HORIZON_DAYS, RETURN_FREQ)` gives "60-day" or
  "12-week (60 business days)"; the plots take this string instead of an
  integer, and the notebook prints "daily"/"weekly returns" in its sample line.
- **Untouched:** simulation, metrics, optimisation, cloud, stats, the cache,
  the cross-check fixtures.

## What the choice means

Resampling whole weeks keeps the serial dependence inside a week (part of the
volatility clustering that the daily bootstrap ignores) and smooths the
asynchronous closing times of the four ETFs. The price is the sample: about
827 weeks instead of 4 027 days, so the empirical distribution being resampled
has five times fewer support points.

## Tests

`tests/testthat/test-frequency.R`: daily is the identity; weekly sums equal
the log ratio of week-end prices on a synthetic series with known dates;
consecutive weekly rows are seven calendar days apart on a calendar-day
fixture; a hand-built case covers an ISO week split across two years and a
week with a single trading day; the first and last weeks are dropped;
`horizon_periods` converts 60 to 12 and rejects a horizon that is not a whole
number of weeks; an unknown frequency errors.

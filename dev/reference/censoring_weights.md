# Calculations for inverse probability of censoring weights (IPCW)

The method of Graf *et al* (1999) is used to compute weights at specific
evaluation times that can be used to help measure a model's
time-dependent performance (e.g. the time-dependent Brier score or the
area under the ROC curve). This is an internal function.

## Usage

``` r
.censoring_weights_graf(object, ...)

# Default S3 method
.censoring_weights_graf(object, ...)

# S3 method for class 'model_fit'
.censoring_weights_graf(
  object,
  predictions,
  cens_predictors = NULL,
  trunc = 0.05,
  eps = 10^-10,
  ...
)
```

## Arguments

- object:

  A fitted parsnip model object or fitted workflow with a mode of
  "censored regression".

- predictions:

  A data frame with a column containing a
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  object as well as a list column called `.pred` that contains the data
  structure produced by
  [`predict.model_fit()`](https://parsnip.tidymodels.org/dev/reference/predict.model_fit.md).

- cens_predictors:

  Not currently used. A potential future slot for models with
  informative censoring based on columns in `predictions`.

- trunc:

  A potential lower bound for the probability of censoring to avoid very
  large weight values.

- eps:

  A small value that is subtracted from the evaluation time when
  computing the censoring probabilities. See Details below.

## Value

The same data are returned with the `pred` tibbles containing several
new columns:

- `.weight_time`: the time at which the inverse censoring probability
  weights are computed. This is a function of the observed time and the
  time of analysis (i.e., `eval_time`). See Details for more
  information.

- `.pred_censored`: the probability of being censored at `.weight_time`.

- `.weight_censored`: The inverse of the censoring probability.

## Details

A probability that the data are censored immediately prior to a specific
time is computed. To do this, we must determine what time to make the
prediction. There are two time values for each row of the data set: the
observed time (either censored or not) and the time that the model is
being evaluated at (e.g. the survival function prediction at some time
point), which is constant across rows. .

From Graf *et al* (1999) there are three cases:

- If the observed time is a censoring time and that is before the
  evaluation time, the data point should make no contribution to the
  performance metric (their "category 3"). These values have a missing
  value for their probability estimate (and also for their weight
  column).

- If the observed time corresponds to an actual event, and that time is
  prior to the evaluation time (category 1), the probability of being
  censored is predicted at the observed time (minus an epsilon).

- If the observed time is *after* the evaluation time (category 2),
  regardless of the status, the probability of being censored is
  predicted at the evaluation time (minus an epsilon).

The epsilon is used since, we would not have actual information at time
`t` for a data point being predicted at time `t` (only data prior to
time `t` should be available).

After the censoring probability is computed, the `trunc` option is used
to avoid using numbers pathologically close to zero. After this, the
weight is computed by inverting the censoring probability.

The `eps` argument is used to avoid information leakage when computing
the censoring probability. Subtracting a small number avoids using data
that would not be known at the time of prediction. For example, if we
are making survival probability predictions at `eval_time = 3.0`, we
would *not* know the about the probability of being censored at that
exact time (since it has not occurred yet).

When creating weights by inverting probabilities, there is the risk that
a few cases will have severe outliers due to probabilities close to
zero. To mitigate this, the `trunc` argument can be used to put a cap on
the weights. If the smallest probability is greater than `trunc`, the
probabilities with values less than `trunc` are given that value.
Otherwise, `trunc` is adjusted to be half of the smallest probability
and that value is used as the lower bound..

Note that if there are `n` rows in `data` and `t` time points, the
resulting data, once unnested, has `n * t` rows. Computations will not
easily scale well as `t` becomes very large.

## References

Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999),
Assessment and comparison of prognostic classification schemes for
survival data. *Statist. Med.*, 18: 2529-2545.

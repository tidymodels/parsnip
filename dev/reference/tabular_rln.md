# Regularization Learning Network

`tabular_rln()` defines a single-hidden-layer neural network where each
weight learns its own adaptive regularization coefficient. This function
can fit regression models only.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`brulee`](https://parsnip.tidymodels.org/dev/reference/details_tabular_rln_brulee.md)`¹²`

¹ The default engine. ² Requires a parsnip extension package.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
tabular_rln(
  mode = "regression",
  engine = "brulee",
  hidden_units = NULL,
  penalty_type = NULL,
  penalty_average = NULL,
  step_rate = NULL,
  activation = NULL,
  epochs = NULL,
  learn_rate = NULL,
  rate_schedule = NULL,
  momentum = NULL,
  batch_size = NULL,
  stop_iter = NULL
)
```

## Arguments

- mode:

  A single character string for the type of model. The only valid value
  is `"regression"`.

- engine:

  A single character string specifying what computational engine to use
  for fitting. The only valid value is `"brulee"`.

- hidden_units:

  An integer for the number of units in the single hidden layer (\>= 1).

- penalty_type:

  A string for the regularization norm: `"L1"` (default in `brulee`) or
  `"L2"`. L1 is recommended by the original paper.

- penalty_average:

  A positive numeric value for the target geometric mean of the
  per-weight regularization coefficients, on the natural scale. Best
  tuned on the log10 scale via
  [`dials::penalty_average()`](https://dials.tidymodels.org/reference/rln-param.html).

- step_rate:

  A positive numeric value for the step size used to update the
  per-weight regularization coefficients, on the natural scale. Best
  tuned on the log10 scale via
  [`dials::step_rate()`](https://dials.tidymodels.org/reference/rln-param.html).

- activation:

  A character string for the activation function between the hidden and
  output layers (e.g., `"relu"`, `"elu"`, `"tanh"`).

- epochs:

  An integer for the number of training iterations.

- learn_rate:

  A positive number for the learning rate.

- rate_schedule:

  A character string for the learning rate schedule (e.g., `"none"`,
  `"decay_time"`, `"cyclic"`).

- momentum:

  A number between 0 (inclusive) and 1 for the momentum parameter used
  by the optimizer.

- batch_size:

  An integer for the number of training samples used per gradient update
  step.

- stop_iter:

  A non-negative integer for the number of iterations without
  improvement before stopping training early.

## References

Shavitt, I., & Segal, E. (2018). Regularization learning networks: Deep
learning for tabular datasets. *Advances in Neural Information
Processing Systems*, 31, 1379-1389.

## See also

[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`brulee engine details`](https://parsnip.tidymodels.org/dev/reference/details_tabular_rln_brulee.md)

## Examples

``` r
show_engines("tabular_rln")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: engine <chr>, mode <chr>

tabular_rln(hidden_units = 10L, penalty_average = 1e-8)
#> ! parsnip could not locate an implementation for `tabular_rln` model
#>   specifications.
#> ℹ The parsnip extension package tabby implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> tabular rln Model Specification (regression)
#> 
#> Main Arguments:
#>   hidden_units = 10
#>   penalty_average = 1e-08
#> 
#> Computational engine: brulee 
#> 
```

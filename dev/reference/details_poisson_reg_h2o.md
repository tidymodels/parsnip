# Poisson regression via h2o

`h2o::h2o.glm()` uses penalized maximum likelihood to fit a model for
count data.

## Details

For this engine, there is a single mode: regression

### Tuning Parameters

This model has 2 tuning parameters:

- `mixture`: Proportion of Lasso Penalty (type: double, default: see
  below)

- `penalty`: Amount of Regularization (type: double, default: see below)

By default, when not given a fixed `penalty`, `h2o::h2o.glm()` uses a
heuristic approach to select the optimal value of `penalty` based on
training data. Setting the engine parameter `lambda_search` to `TRUE`
enables an efficient version of the grid search, see more details at
<https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/lambda_search.html>.

The choice of `mixture` depends on the engine parameter `solver`, which
is automatically chosen given training data and the specification of
other model parameters. When `solver` is set to `'L-BFGS'`, `mixture`
defaults to 0 (ridge regression) and 0.5 otherwise.

### Translation from parsnip to the original package

`agua::h2o_train_glm()` for
[`poisson_reg()`](https://parsnip.tidymodels.org/dev/reference/poisson_reg.md)
is a wrapper around `h2o::h2o.glm()` with `family = 'poisson'`.

The **agua** extension package is required to fit this model.

    library(poissonreg)

    poisson_reg(penalty = double(1), mixture = double(1)) |>
      set_engine("h2o") |>
      translate()

    ## Poisson Regression Model Specification (regression)
    ##
    ## Main Arguments:
    ##   penalty = double(1)
    ##   mixture = double(1)
    ##
    ## Computational engine: h2o
    ##
    ## Model fit template:
    ## agua::h2o_train_glm(x = missing_arg(), y = missing_arg(), weights = missing_arg(),
    ##     validation_frame = missing_arg(), lambda = double(1), alpha = double(1),
    ##     family = "poisson")

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md), parsnip
will convert factor columns to indicators.

Predictors should have the same scale. One way to achieve this is to
center and scale each so that each predictor has mean zero and a
variance of one.

By default, `h2o::h2o.glm()` uses the argument `standardize = TRUE` to
center and scale all numerical columns.

### Case weights

This model can utilize case weights during model fitting. To use them,
see the documentation in
[case_weights](https://parsnip.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

The [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) arguments
have arguments called `case_weights` that expect vectors of case
weights.

### Prediction types

    parsnip:::get_from_env("poisson_reg_predict") |>
      dplyr::filter(engine == "h2o") |>
      dplyr::select(mode, type)

    ## # A tibble: 2 x 2
    ##   mode       type
    ##   <chr>      <chr>
    ## 1 regression numeric
    ## 2 regression raw

### Initializing h2o

To use the h2o engine with tidymodels, please run `h2o::h2o.init()`
first. By default, This connects R to the local h2o server. This needs
to be done in every new R session. You can also connect to a remote h2o
server with an IP address, for more details see `h2o::h2o.init()`.

You can control the number of threads in the thread pool used by h2o
with the `nthreads` argument. By default, it uses all CPUs on the host.
This is different from the usual parallel processing mechanism in
tidymodels for tuning, while tidymodels parallelizes over resamples, h2o
parallelizes over hyperparameter combinations for a given resample.

h2o will automatically shut down the local h2o instance started by R
when R is terminated. To manually stop the h2o server, run
`h2o::h2o.shutdown()`.

### Saving fitted model objects

Models fitted with this engine may require native serialization methods
to be properly saved and/or passed between R sessions. To learn more
about preparing fitted models for serialization, see the bundle package.

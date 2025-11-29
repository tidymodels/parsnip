# Linear regression via Bayesian Methods

The `"stan"` engine estimates regression parameters using Bayesian
estimation.

## Details

For this engine, there is a single mode: regression

### Tuning Parameters

This engine has no tuning parameters.

### Important engine-specific options

Some relevant arguments that can be passed to
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md):

- `chains`: A positive integer specifying the number of Markov chains.
  The default is 4.

- `iter`: A positive integer specifying the number of iterations for
  each chain (including warmup). The default is 2000.

- `seed`: The seed for random number generation.

- `cores`: Number of cores to use when executing the chains in parallel.

- `prior`: The prior distribution for the (non-hierarchical) regression
  coefficients. The `"stan"` engine does not fit any hierarchical terms.
  See the `"stan_glmer"` engine from the multilevelmod package for that
  type of model.

- `prior_intercept`: The prior distribution for the intercept (after
  centering all predictors).

See
[`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
and
[`rstanarm::priors()`](https://mc-stan.org/rstanarm/reference/priors.html)
for more information on these and other options.

### Translation from parsnip to the original package

    linear_reg() |>
      set_engine("stan") |>
      translate()

    ## Linear Regression Model Specification (regression)
    ##
    ## Computational engine: stan
    ##
    ## Model fit template:
    ## rstanarm::stan_glm(formula = missing_arg(), data = missing_arg(),
    ##     weights = missing_arg(), family = stats::gaussian, refresh = 0)

Note that the `refresh` default prevents logging of the estimation
process. Change this value in
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md)
to show the MCMC logs.

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md), parsnip
will convert factor columns to indicators.

### Other details

For prediction, the `"stan"` engine can compute posterior intervals
analogous to confidence and prediction intervals. In these instances,
the units are the original outcome and when `std_error = TRUE`, the
standard deviation of the posterior distribution (or posterior
predictive distribution as appropriate) is returned.

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

    parsnip:::get_from_env("linear_reg_predict") |>
      dplyr::filter(engine == "stan") |>
      dplyr::select(mode, type)

    ## # A tibble: 4 x 2
    ##   mode       type
    ##   <chr>      <chr>
    ## 1 regression numeric
    ## 2 regression conf_int
    ## 3 regression pred_int
    ## 4 regression raw

### Examples

The “Fitting and Predicting with parsnip” article contains
[examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#linear-reg-stan)
for
[`linear_reg()`](https://parsnip.tidymodels.org/dev/reference/linear_reg.md)
with the `"stan"` engine.

### References

- McElreath, R. 2020 *Statistical Rethinking*. CRC Press.

# Logistic regression via stan

[`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html)
fits a generalized linear model for binary outcomes. A linear
combination of the predictors is used to model the log odds of an event.

## Details

For this engine, there is a single mode: classification

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
  coefficients. This `"stan"` engine does not fit any hierarchical
  terms.

- `prior_intercept`: The prior distribution for the intercept (after
  centering all predictors).

See
[`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
and
[`rstanarm::priors()`](https://mc-stan.org/rstanarm/reference/priors.html)
for more information on these and other options.

### Translation from parsnip to the original package

    logistic_reg() |>
      set_engine("stan") |>
      translate()

    ## Logistic Regression Model Specification (classification)
    ##
    ## Computational engine: stan
    ##
    ## Model fit template:
    ## rstanarm::stan_glm(formula = missing_arg(), data = missing_arg(),
    ##     weights = missing_arg(), family = stats::binomial, refresh = 0)

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

    parsnip:::get_from_env("logistic_reg_predict") |>
      dplyr::filter(engine == "stan") |>
      dplyr::select(mode, type)

    ## # A tibble: 5 x 2
    ##   mode           type
    ##   <chr>          <chr>
    ## 1 classification class
    ## 2 classification prob
    ## 3 classification raw
    ## 4 classification conf_int
    ## 5 classification pred_int

### Examples

The “Fitting and Predicting with parsnip” article contains
[examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#logistic-reg-stan)
for
[`logistic_reg()`](https://parsnip.tidymodels.org/dev/reference/logistic_reg.md)
with the `"stan"` engine.

### References

- McElreath, R. 2020 *Statistical Rethinking*. CRC Press.

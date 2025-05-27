


For this engine, there is a single mode: regression

## Tuning Parameters

This engine has no tuning parameters.

## Important engine-specific options

Some relevant arguments that can be passed to `set_engine()`: 

 * `chains`: A positive integer specifying the number of Markov chains. The default is 4.
 * `iter`: A positive integer specifying the number of iterations for each chain (including warmup). The default is 2000.
 * `seed`: The seed for random number generation. 
 * `cores`: Number of cores to use when executing the chains in parallel.
 * `prior`: The prior distribution for the (non-hierarchical) regression coefficients. The `"stan"` engine does not fit any hierarchical terms. 
 * `prior_intercept`: The prior distribution for the intercept (after centering all predictors). 
 
See [rstan::sampling()] and [rstanarm::priors()] for more information on these and other options.  

## Translation from parsnip to the original package

The **poissonreg** extension package is required to fit this model.


``` r
library(poissonreg)

poisson_reg() |> 
  set_engine("stan") |> 
  translate()
```

```
## Poisson Regression Model Specification (regression)
## 
## Computational engine: stan 
## 
## Model fit template:
## rstanarm::stan_glm(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), family = stats::poisson)
```

Note that the `refresh` default prevents logging of the estimation process. Change this value in `set_engine()` to show the MCMC logs.

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Other details

For prediction, the `"stan"` engine can compute posterior intervals analogous to confidence and prediction intervals. In these instances, the units are the original outcome. When `std_error = TRUE`, the standard deviation of the posterior  distribution (or posterior predictive distribution as  appropriate) is returned.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#linear-reg-stan) for `poisson_reg()` with the `"stan"` engine.

## References

 - McElreath, R. 2020 _Statistical Rethinking_. CRC Press.

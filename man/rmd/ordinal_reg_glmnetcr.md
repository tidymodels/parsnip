


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 2 tuning parameters:

- `penalty`: Amount of Regularization (type: double, default: see below)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 1.0)

`penalty` has no fixed default and a single value is required at fit time.
For more details, see [`glmnet-details`].

## Translation from parsnip to the original package

The **ordered** extension package is required to fit this model.


``` r
library(ordered)

ordinal_reg(penalty = double(1), mixture = double(1)) |>
  set_engine("glmnetcr") |>
  translate()
```

```
## Ordinal Regression Model Specification (classification)
## 
## Main Arguments:
##   penalty = 0
##   mixture = double(1)
## 
## Computational engine: glmnetcr 
## 
## Model fit template:
## glmnetcr::glmnetcr(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     alpha = double(1))
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

By default, [glmnetcr::glmnetcr()] uses `standardize = TRUE` to center and scale the data.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` functions have arguments called `case_weights` that expect vectors of case weights. 

## Controlling penalty values

`glmnetcr()` computes coefficient estimates over a path of penalty values.
The \pkg{ordered} package can interpolate probabilities between adjacent path
values. Penalties outside the fitted path are clamped to the nearest endpoint
rather than extrapolated.

We suggest setting the penalty path when fitting the model. This is important
when tuning the model because different data sets and mixture values pair
best with different penalty ranges.

```r
library(ordered)

penalties <- 10^seq(-4, 0, length.out = 20)

ordinal_reg(penalty = tune()) |>
  set_engine("glmnetcr", path_values = !!penalties)
```

`multi_predict()` can return predictions for the full path. When a penalty is
not supplied to prediction, `criteria = "bic"` selects a path step;
`criteria = "aic"` is also available in that case.

## Prediction types


``` r
parsnip:::get_from_env("ordinal_reg_predict") |>
  dplyr::filter(engine == "glmnetcr") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 2 x 2
##   mode           type 
##   <chr>          <chr>
## 1 classification class
## 2 classification prob
```

## References

- Archer KJ, Williams AAA. 2012. L1 penalized continuation ratio models for
  ordinal response prediction using high-dimensional datasets.
  _Statistics in Medicine_ 31:2632-2642. \doi{10.1002/sim.4484}.

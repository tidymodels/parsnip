


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 1 tuning parameters:

- `penalty`: Amount of Regularization (type: double, default: 0)

`penalty` is passed directly to the native `penalty` argument.

## Translation from parsnip to the original package

The **ordered** extension package is required to fit this model.


``` r
library(ordered)

ordinal_reg(penalty = double(0)) |>
  set_engine("lrm") |>
  translate()
```

```
## Ordinal Regression Model Specification (classification)
## 
## Main Arguments:
##   penalty = double(0)
## 
## Computational engine: lrm 
## 
## Model fit template:
## rms::lrm(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     penalty = double(0))
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` functions have arguments called `case_weights` that expect vectors of case weights. 

Case weights require `rms` 7.0-0 or later.

## Prediction types


``` r
parsnip:::get_from_env("ordinal_reg_predict") |>
  dplyr::filter(engine == "lrm") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 3 x 2
##   mode           type       
##   <chr>          <chr>      
## 1 classification class      
## 2 classification prob       
## 3 classification linear_pred
```

## References

- Harrell Jr FE. 2026. _rms: Regression Modeling Strategies_. R package
  version 8.1-1. \doi{10.32614/CRAN.package.rms}.

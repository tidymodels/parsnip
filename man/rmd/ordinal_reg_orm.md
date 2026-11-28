


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 2 tuning parameters:

- `ordinal_link`: Ordinal Link (type: character, default: logistic)

- `penalty`: Amount of Regularization (type: double, default: 0)

`ordinal_link` is passed to the native `family` argument and `penalty` is
passed directly to the native `penalty` argument.

## Translation from parsnip to the original package

The **ordered** extension package is required to fit this model.


``` r
library(ordered)

ordinal_reg(
  ordinal_link = character(0),
  penalty = double(0)
) |>
  set_engine("orm") |>
  translate()
```

```
## Ordinal Regression Model Specification (classification)
## 
## Main Arguments:
##   ordinal_link = character(0)
##   penalty = double(0)
## 
## Computational engine: orm 
## 
## Model fit template:
## ordered::rms_orm_wrapper(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), family = character(0), penalty = double(0))
```

The wrapper checks the installed `rms` version before fitting.

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` functions have arguments called `case_weights` that expect vectors of case weights. 

Case weights and nonzero penalties require `rms` 7.0-0 or later.

## Prediction types


``` r
parsnip:::get_from_env("ordinal_reg_predict") |>
  dplyr::filter(engine == "orm") |>
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

- Harrell Jr FE. 2015. _Regression Modeling Strategies: With Applications to
  Linear Models, Logistic and Ordinal Regression, and Survival Analysis_.
  Springer International Publishing, Cham. \doi{10.1007/978-3-319-19425-7}.

- Harrell Jr FE. 2026. _rms: Regression Modeling Strategies_. R package
  version 8.1-1. \doi{10.32614/CRAN.package.rms}.

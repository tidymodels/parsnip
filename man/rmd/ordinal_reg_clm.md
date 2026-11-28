


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 3 tuning parameters:

- `ordinal_link`: Ordinal Link (type: character, default: logit)

- `threshold_structure`: Threshold Structure (type: character, default: flexible)

- `parallel_reg`: Parallel Regression (type: logical, default: TRUE)

`ordinal_link` is translated to the native `link` argument. In addition to
the standard links, this engine supports `"Aranda-Ordaz"` and `"log-gamma"`.
The dial value `"logistic"` is translated to the native value `"logit"`.

`threshold_structure` is translated to the native `threshold` argument. Its
values are `"flexible"`, `"symmetric_median"`, `"symmetric_zero"`, and
`"equidistant"`, which map to the native values `"flexible"`, `"symmetric"`,
`"symmetric2"`, and `"equidistant"`, respectively.

`parallel_reg = FALSE` puts every predictor in the native `nominal` formula
and leaves an intercept-only `location` formula; at least one predictor is
required. The native `nominal` argument can be passed through [set_engine()]
only when `parallel_reg` is not set.

## Translation from parsnip to the original package

The **ordered** extension package is required to fit this model.


``` r
library(ordered)

ordinal_reg(
  ordinal_link = character(0),
  threshold_structure = character(0),
  parallel_reg = logical(0)
) |>
  set_engine("clm") |>
  translate()
```

```
## Ordinal Regression Model Specification (classification)
## 
## Main Arguments:
##   ordinal_link = character(0)
##   threshold_structure = character(0)
##   parallel_reg = logical(0)
## 
## Computational engine: clm 
## 
## Model fit template:
## ordered::clm_wrapper(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), link = character(0), threshold = character(0), 
##     parallel_reg = logical(0))
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` functions have arguments called `case_weights` that expect vectors of case weights. 

## Prediction types


``` r
parsnip:::get_from_env("ordinal_reg_predict") |>
  dplyr::filter(engine == "clm") |>
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

- Christensen RHB. 2025. _ordinal — Regression Models for Ordinal Data_. R
  package version 2025.12-29. \url{https://CRAN.R-project.org/package=ordinal}.

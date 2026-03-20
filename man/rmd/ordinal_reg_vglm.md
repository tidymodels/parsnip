


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 2 tuning parameters:

- `ordinal_link`: Ordinal Link (type: character, default: logit)

- `odds_link`: Odds Link (type: character, default: cumulative)

## Translation from parsnip to the original package

The **ordered** extension package is required to fit this model.


``` r
library(ordered)

ordinal_reg(ordinal_link = character(0), odds_link = character(0)) %>% 
  set_engine("vglm") %>% 
  set_mode("classification") %>% 
  translate()
```

```
## Ordinal Regression Model Specification (classification)
## 
## Main Arguments:
##   ordinal_link = character(0)
##   odds_link = character(0)
## 
## Computational engine: vglm 
## 
## Model fit template:
## ordered::VGAM_vglm_wrapper(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), link = character(0), family = character(0), 
##     parallel = TRUE)
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

_However_, the documentation in [VGAM::vglm()] notes that matrix of case weights can be passed so that different classes have different weights. tidymodels assumes vector of a weights; a matrix cannot be passed in. 

Also, the engine documentation notes that: "The values of weights must be positive; try setting a very small value such as 1.0e-8 to effectively delete an observation."

## Prediction types


``` r
parsnip:::get_from_env("ordinal_reg_predict") |>
  dplyr::filter(engine == "vglm") |>
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

- Yee T. 2010. The VGAM Package for Categorical Data Analysis. _Journal of Statistical Software_ 32(10):1--34. \doi{10.18637/jss.v032.i10}.

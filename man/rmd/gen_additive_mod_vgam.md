


For this engine, there is a single mode: classification

## Tuning Parameters

There are no main tuning parameters for this model. Two relevant engine parameters are:

 - `link`: the link function such as `logistic`, `probit`, `loglog`, `cloglog`, or `cauchit`. 
 - `family`: the function to contrast levels such as `cumulative_link`, `adjacent_categories`, `continuation_ratio`, or `stopping_ratio`

## Translation from parsnip to the original package




``` r
library(ordered)

gen_additive_mod() %>% 
  set_engine("vgam") %>% 
  set_mode("classification") %>% 
  translate()
```

```
## GAM Model Specification (classification)
## 
## Computational engine: vgam 
## 
## Model fit template:
## ordered::VGAM_vgam_wrapper(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), parallel = TRUE)
```


## Model fitting

This model should be used with a model formula so that smooth terms can be specified. For example:


``` r
library(VGAM)
# Make number of cylinders and ordered factor
ord_cars <- mtcars[, -1]
ord_cars$cyl <- as.ordered(ord_cars$cyl)

car_fit <- 
  gen_additive_mod() |> 
  set_engine("vgam") |> 
  set_mode("classification") |> 
  fit(cyl ~ disp + s(wt) + am, data = ord_cars)
```

The smoothness of the terms will need to be manually specified (e.g., using `s(x, df = 10)`) in the formula. 

When using a workflow, pass the _model formula_ to [workflows::add_model()]'s `formula` argument, and a simplified _preprocessing formula_ elsewhere.


``` r
spec <- 
  gen_additive_mod() |> 
  set_engine("vgam") |> 
  set_mode("classification")

workflow() |> 
  add_model(spec, formula = cyl ~ disp + s(wt) + am) |> 
  add_formula(cyl ~ disp + wt + am) |> 
  fit(data = ord_cars) |> 
  extract_fit_engine()
```

To learn more about the differences between these formulas, see [`?model_formula`][parsnip::model_formula].

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## Prediction types


``` r
parsnip:::get_from_env("gen_additive_mod_predict") |>
  dplyr::filter(engine == "vgam") |> 
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

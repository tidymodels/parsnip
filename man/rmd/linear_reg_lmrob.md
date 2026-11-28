

For this engine, there is a single mode: regression.

## Tuning Parameters

This engine has no tuning parameters. Arguments to [robustbase::lmrob()], such as `method` or `control`, can be supplied through `set_engine()`. The `penalty` and `mixture` arguments to `linear_reg()` do not apply to this engine.

## Translation from parsnip to the original package


``` r
linear_reg() |>
  set_engine("lmrob") |>
  translate()
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lmrob 
#> 
#> Model fit template:
#> robustbase::lmrob(formula = missing_arg(), data = missing_arg(), 
#>     weights = missing_arg())
```

The default estimator is an MM-estimator. See [robustbase::lmrob()] and [robustbase::lmrob.control()] for the available estimators and their controls. The initial robust fit uses random subsampling, so set the random seed to reproduce a fit.

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` functions have arguments called `case_weights` that expect vectors of case weights. 

Case weights are passed to the `weights` argument of [robustbase::lmrob()]. They are prior weights, distinct from the robustness weights calculated by the estimator. See the robustbase documentation for their interpretation.

## Prediction types


``` r
parsnip:::get_from_env("linear_reg_predict") |>
  dplyr::filter(engine == "lmrob") |>
  dplyr::select(mode, type)
#> # A tibble: 4 x 2
#>   mode       type    
#>   <chr>      <chr>   
#> 1 regression numeric 
#> 2 regression conf_int
#> 3 regression pred_int
#> 4 regression raw
```

Confidence and prediction intervals use [robustbase::predict.lmrob()] and its covariance and scale estimates. The `level` argument to `predict()` sets their nominal coverage. These intervals retain the assumptions of the underlying method; robust estimation does not by itself guarantee coverage under arbitrary contamination or heteroscedasticity.

Use `type = "raw"` with `opts` to pass prediction arguments directly to [robustbase::predict.lmrob()].

## Examples


``` r
set.seed(123)
robust_fit <- linear_reg() |>
  set_engine("lmrob") |>
  fit(stack.loss ~ ., data = stackloss)

predict(robust_fit, new_data = stackloss[1:3, ])
#> # A tibble: 3 x 1
#>   .pred
#>   <dbl>
#> 1  39.2
#> 2  39.3
#> 3  33.2
predict(robust_fit, new_data = stackloss[1:3, ], type = "conf_int", level = 0.90)
#> # A tibble: 3 x 2
#>   .pred_lower .pred_upper
#>         <dbl>       <dbl>
#> 1        34.0        44.4
#> 2        34.0        44.6
#> 3        29.3        37.2
```




For this engine, there is a single mode: regression

## Tuning Parameters

This engine has no tuning parameters. 

## Translation from parsnip to the original package


``` r
linear_reg() |> 
  set_engine("lm") |> 
  translate()
```

```
## Linear Regression Model Specification (regression)
## 
## Computational engine: lm 
## 
## Model fit template:
## stats::lm(formula = missing_arg(), data = missing_arg(), weights = missing_arg())
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

_However_, the documentation in [stats::lm()] assumes that is specific type of case weights are being used: "Non-NULL weights can be used to indicate that different observations have different variances (with the values in weights being inversely proportional to the variances); or equivalently, when the elements of weights are positive integers `w_i`, that each response `y_i` is the mean of `w_i` unit-weight observations (including the case that there are w_i observations equal to `y_i` and the data have been summarized). However, in the latter case, notice that within-group variation is not used. Therefore, the sigma estimate and residual degrees of freedom may be suboptimal; in the case of replication weights, **even wrong**. Hence, standard errors and analysis of variance tables should be treated with care" (emphasis added)

Depending on your application, the degrees of freedom for the model (and other statistics) might be incorrect. 

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#linear-reg-lm) for `linear_reg()` with the `"lm"` engine.

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

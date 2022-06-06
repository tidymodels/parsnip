


For this engine, there is a single mode: classification

## Tuning Parameters

This engine has no tuning parameters but you can set the `family` parameter (and/or `link`) as an engine argument (see below). 

## Translation from parsnip to the original package


```r
logistic_reg() %>% 
  set_engine("glm") %>% 
  translate()
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm 
## 
## Model fit template:
## stats::glm(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     family = stats::binomial)
```

To use a non-default `family` and/or `link`, pass in as an argument to `set_engine()`:


```r
linear_reg() %>% 
  set_engine("glm", family = stats::binomial(link = "probit")) %>% 
  translate()
```

```
## Linear Regression Model Specification (regression)
## 
## Engine-Specific Arguments:
##   family = stats::binomial(link = "probit")
## 
## Computational engine: glm 
## 
## Model fit template:
## stats::glm(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     family = stats::binomial(link = "probit"))
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

_However_, the documentation in [stats::glm()] assumes that is specific type of case weights are being used:"Non-NULL weights can be used to indicate that different observations have different dispersions (with the values in weights being inversely proportional to the dispersions); or equivalently, when the elements of weights are positive integers `w_i`, that each response `y_i` is the mean of `w_i` unit-weight observations. For a binomial GLM prior weights are used to give the number of trials when the response is the proportion of successes: they would rarely be used for a Poisson GLM."

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#logistic-reg-glm) for `logistic_reg()` with the `"glm"` engine.

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

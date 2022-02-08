


For this engine, there is a single mode: regression

## Tuning Parameters

This engine has no tuning parameters but you can set the `family` parameter (and/or `link`) as an engine argument (see below). 

## Translation from parsnip to the original package


```r
linear_reg() %>% 
  set_engine("glm") %>% 
  translate()
```

```
## Linear Regression Model Specification (regression)
## 
## Computational engine: glm 
## 
## Model fit template:
## stats::glm(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     family = stats::gaussian)
```

To use a non-default `family` and/or `link`, pass in as an argument to `set_engine()`:


```r
linear_reg() %>% 
  set_engine("glm", family = stats::poisson(link = "sqrt")) %>% 
  translate()
```

```
## Linear Regression Model Specification (regression)
## 
## Engine-Specific Arguments:
##   family = stats::poisson(link = "sqrt")
## 
## Computational engine: glm 
## 
## Model fit template:
## stats::glm(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     family = stats::poisson(link = "sqrt"))
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit.model_spec()}}, parsnip will convert factor columns to indicators.

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#linear-reg-glm) for `linear_reg()` with the `"glm"` engine.

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

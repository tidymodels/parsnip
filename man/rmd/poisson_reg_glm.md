


For this engine, there is a single mode: regression

## Tuning Parameters

This engine has no tuning parameters. 

## Translation from parsnip to the underlying model call  (regression)

There is a parsnip extension package required to fit this model to this mode: **poissonreg**.


```r
library(poissonreg)

poisson_reg() %>%
  set_engine("glm") %>%
  translate()
```

```
## Poisson Regression Model Specification (regression)
## 
## Computational engine: glm 
## 
## Model fit template:
## stats::glm(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     family = stats::poisson)
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit.model_spec()}}, parsnip will convert factor columns to indicators.



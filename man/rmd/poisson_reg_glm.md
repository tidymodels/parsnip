


For this engine, there is a single mode: regression

## Tuning Parameters

This engine has no tuning parameters. 

## Translation from parsnip to the underlying model call  (regression)

The **poissonreg** extension package is required to fit this model.


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


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.






For this engine, there is a single mode: regression

## Tuning Parameters



This model has 2 tuning parameters:

- `penalty`: Amount of Regularization (type: double, default: see below)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 1.0)

A value of `mixture = 1` corresponds to a pure lasso model, while `mixture = 0` indicates ridge regression.

The `penalty` parameter has no default and requires a single numeric value. For more details about this, and the `glmnet` model in general, see [glmnet-details].

## Translation from parsnip to the original package

There is a parsnip extension package required to fit this model to this mode: **poissonreg**.


```r
library(poissonreg)

poisson_reg(penalty = double(1), mixture = double(1)) %>% 
  set_engine("glmnet") %>% 
  translate()
```

```
## Poisson Regression Model Specification (regression)
## 
## Main Arguments:
##   penalty = 0
##   mixture = double(1)
## 
## Computational engine: glmnet 
## 
## Model fit template:
## glmnet::glmnet(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     alpha = double(1), family = "poisson")
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit.model_spec()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.
By default, `glmnet::glmnet()` uses the argument `standardize = TRUE` to center and scale the data. 


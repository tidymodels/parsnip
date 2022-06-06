


For this engine, there is a single mode: regression

## Tuning Parameters



This model has 2 tuning parameters:

- `mixture`: Proportion of Lasso Penalty (type: double, default: see below)

- `penalty`: Amount of Regularization (type: double, default: 0)

By default h2o applies no regularization and `penalty` is set to zero. When the engine parameter `solver` is set to `'L-BFGS'`, `mixture` defaults to 0 (ridge regression) and 0.5 otherwise. 

## Translation from parsnip to the original package

The h2o engine uses `family = "gaussian"` in [h2o::h2o.glm()] to fit linear models. 


```r
linear_reg(penalty = 1, mixture = 0.5) %>% 
  set_engine("h2o") %>% 
  translate()
```

```
## Linear Regression Model Specification (regression)
## 
## Main Arguments:
##   penalty = 1
##   mixture = 0.5
## 
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_glm(x = missing_arg(), y = missing_arg(), lambda = 1, 
##     alpha = 0.5, family = "gaussian")
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.
By default, [h2o::h2o.glm()] uses the argument `standardize = TRUE` to center and scale the data. 


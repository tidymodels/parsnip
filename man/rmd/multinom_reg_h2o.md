


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 2 tuning parameters:

- `mixture`: Proportion of Lasso Penalty (type: double, default: see below)

- `penalty`: Amount of Regularization (type: double, default: 0)

By default [h2o::h2o.glm()] applies no regularization and `penalty` is set to zero. When the engine parameter `solver` is set to `'L-BFGS'`, `mixture` defaults to 0 (ridge regression) and 0.5 otherwise. 

## Translation from parsnip to the original package

[agua::h2o_train_glm()] for `multinom_reg()` is a wrapper around [h2o::h2o.glm()] with `family = 'multinomial'`. 


```r
multinom_reg(penalty = double(1), mixture = double(1)) %>% 
  set_engine("h2o") %>% 
  translate()
```

```
## Multinomial Regression Model Specification (classification)
## 
## Main Arguments:
##   penalty = double(1)
##   mixture = double(1)
## 
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_glm(x = missing_arg(), y = missing_arg(), family = missing_arg(), 
##     lambda = double(1), alpha = double(1), family = "multinomial")
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

By default, [h2o::h2o.glm()] uses the argument `standardize = TRUE` to center and scale the data. 


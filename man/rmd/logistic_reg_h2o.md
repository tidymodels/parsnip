



For this engine, there is a single mode: classification

## Tuning Parameters



This model has 2 tuning parameters:

- `mixture`: Proportion of Lasso Penalty (type: double, default: 0)

- `penalty`: Amount of Regularization (type: double, default: 0)


## Translation from parsnip to the original package

`agua` uses the [agua::h2o_train_glm()] wrapper function for fitting the logistic regression model, which calls [h2o::h2o.glm()]. h2o will automatically picks the link function and distribution family or binomial responses. 


```r
logistic_reg() %>% 
  set_engine("h2o") %>% 
  translate()
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_glm(x = missing_arg(), y = missing_arg())
```

To use a non-default argument in [h2o::h2o.glm()], pass in as an engine argument to `set_engine()`:


```r
logistic_reg() %>% 
  set_engine("h2o", compute_p_values = TRUE) %>% 
  translate()
```

```
## Logistic Regression Model Specification (classification)
## 
## Engine-Specific Arguments:
##   compute_p_values = TRUE
## 
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_glm(x = missing_arg(), y = missing_arg(), compute_p_values = TRUE)
```

Other h2o engine arguments of interest: 


- `solver`: The solver method used in optimization, the default `"auto"` works well in most cases


## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

By default, [h2o::h2o.glm()] uses the argument `standardize = TRUE` to center and scale all numeric columns. 

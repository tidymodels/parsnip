


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 2 tuning parameters:

- `mixture`: Proportion of Lasso Penalty (type: double, default: see below)

- `penalty`: Amount of Regularization (type: double, default: see below)


By default, when not given a fixed `penalty`, [h2o::h2o.glm()] uses a heuristic approach to select the optimal value of `penalty` based on training data. Setting the engine parameter `lambda_search` to `TRUE` enables an efficient version of the grid search, see more details at <https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/lambda_search.html>. 

The choice of `mixture` depends on the engine parameter `solver`, which is automatically chosen given training data and the specification of other model parameters. When `solver` is set to `'L-BFGS'`, `mixture` defaults to 0 (ridge regression) and 0.5 otherwise. 

## Translation from parsnip to the original package

[agua::h2o_train_glm()] for `multinom_reg()` is a wrapper around [h2o::h2o.glm()] with `family = 'multinomial'`. 


``` r
multinom_reg(penalty = double(1), mixture = double(1)) |> 
  set_engine("h2o") |> 
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
## agua::h2o_train_glm(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     validation_frame = missing_arg(), lambda = double(1), alpha = double(1), 
##     family = "multinomial")
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

By default, [h2o::h2o.glm()] uses the argument `standardize = TRUE` to center and scale the data. 

## Initializing h2o 


To use the h2o engine with tidymodels, please run `h2o::h2o.init()` first. By default, This connects R to the local h2o server. This needs to be done in every new R session. You can also connect to a remote h2o server with an IP address, for more details see [h2o::h2o.init()]. 

You can control the number of threads in the thread pool used by h2o with the `nthreads` argument. By default, it uses all CPUs on the host. This is different from the usual parallel processing mechanism in tidymodels for tuning, while tidymodels parallelizes over resamples, h2o parallelizes over hyperparameter combinations for a given resample. 

h2o will automatically shut down the local h2o instance started by R when R is terminated. To manually stop the h2o server, run `h2o::h2o.shutdown()`. 

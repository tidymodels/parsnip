


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 3 tuning parameters:

- `trees`: # Trees (type: integer, default: 50L)

- `tree_depth`: Tree Depth (type: integer, default: 3L)

- `penalty`: Amount of Regularization (type: double, default: 0)
Note that `penalty` for the h2o engine in `rule_fit()`` corresponds to the L1 penalty (LASSO). 


Other engine arguments of interest: 

- `algorithm`: The algorithm to use to generate rules. should be one of "AUTO", "DRF", "GBM", defaults to "AUTO".

- `min_rule_length`: Minimum length of tree depth, opposite of `tree_dpeth`, defaults to 3.

- `max_num_rules`: The maximum number of rules to return. The default value of -1 means the number of rules is selected by diminishing returns in model deviance. 

- `model_type`: The type of base learners in the ensemble, should be one of: "rules_and_linear", "rules", "linear", defaults to "rules_and_linear".


## Translation from parsnip to the underlying model call  (regression)

[agua::h2o_train_rule()] is a wrapper around [h2o::h2o.rulefit()]. 

The **agua** extension package is required to fit this model.


``` r
library(rules)

rule_fit(
  trees = integer(1),
  tree_depth = integer(1),
  penalty = numeric(1)
) |>
  set_engine("h2o") |>
  set_mode("regression") |>
  translate()
```

```
## RuleFit Model Specification (regression)
## 
## Main Arguments:
##   trees = integer(1)
##   tree_depth = integer(1)
##   penalty = numeric(1)
## 
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_rule(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     validation_frame = missing_arg(), rule_generation_ntrees = integer(1), 
##     max_rule_length = integer(1), lambda = numeric(1))
```

## Translation from parsnip to the underlying model call  (classification)



[agua::h2o_train_rule()] for `rule_fit()` is a wrapper around [h2o::h2o.rulefit()]. 

The **agua** extension package is required to fit this model.


``` r
rule_fit(
  trees = integer(1),
  tree_depth = integer(1),
  penalty = numeric(1)
) |>
  set_engine("h2o") |>
  set_mode("classification") |>
  translate()
```

```
## RuleFit Model Specification (classification)
## 
## Main Arguments:
##   trees = integer(1)
##   tree_depth = integer(1)
##   penalty = numeric(1)
## 
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_rule(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     validation_frame = missing_arg(), rule_generation_ntrees = integer(1), 
##     max_rule_length = integer(1), lambda = numeric(1))
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Other details


To use the h2o engine with tidymodels, please run `h2o::h2o.init()` first. By default, This connects R to the local h2o server. This needs to be done in every new R session. You can also connect to a remote h2o server with an IP address, for more details see [h2o::h2o.init()]. 

You can control the number of threads in the thread pool used by h2o with the `nthreads` argument. By default, it uses all CPUs on the host. This is different from the usual parallel processing mechanism in tidymodels for tuning, while tidymodels parallelizes over resamples, h2o parallelizes over hyperparameter combinations for a given resample. 

h2o will automatically shut down the local h2o instance started by R when R is terminated. To manually stop the h2o server, run `h2o::h2o.shutdown()`. 

## Saving fitted model objects


Models fitted with this engine may require native serialization methods to be properly saved and/or passed between R sessions. To learn more about preparing fitted models for serialization, see the bundle package.

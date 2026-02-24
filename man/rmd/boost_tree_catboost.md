


For this engine, there are multiple modes: regression and classification

## Tuning Parameters



This model has 3 tuning parameters:

- `tree_depth`: Tree Depth (type: integer, default: 6L)

- `trees`: # Trees (type: integer, default: 1000L)

- `learn_rate`: Learning Rate (type: double, default: 0.03)

The `mtry` parameter controls the proportion of predictors that will be randomly sampled at each split. catboost's `rsm` argument natively expects a proportion between 0 and 1. The default is to use all predictors (`rsm = 1`).

Unlike lightgbm and xgboost, bonsai does not currently convert `mtry` from a count to a proportion for catboost. Users should set `counts = FALSE` in `set_engine()` and supply `mtry` as a proportion directly. For example, `mtry = 0.5` with `counts = FALSE` means 50% of predictors are considered at each split.

### Engine-Specific Parameters

CatBoost has a large number of engine parameters. The current list is found at [`https://catboost.ai/docs/en/references/training-parameters`](https://catboost.ai/docs/en/references/training-parameters).

Two in particular are: 

- `max_leaves`: Maximum number of leaves in each tree (only used when the grow policy is `Lossguide`).

- `l2_leaf_reg`: L2 regularization coefficient for leaf values (default: 3.0).

## Translation from parsnip to the original package (regression)

The **bonsai** extension package is required to fit this model.


``` r
boost_tree(
  mtry = integer(), trees = integer(), min_n = integer(), tree_depth = integer(),
  learn_rate = numeric(), sample_size = numeric(), stop_iter = integer()
) |>
  set_engine("catboost") |>
  set_mode("regression") |>
  translate()
```

```
## Boosted Tree Model Specification (regression)
## 
## Main Arguments:
##   mtry = integer()
##   trees = integer()
##   min_n = integer()
##   tree_depth = integer()
##   learn_rate = numeric()
##   sample_size = numeric()
##   stop_iter = integer()
## 
## Computational engine: catboost 
## 
## Model fit template:
## bonsai::train_catboost(x = missing_arg(), y = missing_arg(), 
##     weights = missing_arg(), iterations = integer(), depth = integer(), 
##     learning_rate = numeric(), thread_count = 1, allow_writing_files = FALSE, 
##     random_seed = sample.int(10^5, 1))
```

## Translation from parsnip to the original package (classification)

The **bonsai** extension package is required to fit this model.


``` r
boost_tree(
  mtry = integer(), trees = integer(), min_n = integer(), tree_depth = integer(),
  learn_rate = numeric(), sample_size = numeric(), stop_iter = integer()
) |>
  set_engine("catboost") |>
  set_mode("classification") |>
  translate()
```

```
## Boosted Tree Model Specification (classification)
## 
## Main Arguments:
##   mtry = integer()
##   trees = integer()
##   min_n = integer()
##   tree_depth = integer()
##   learn_rate = numeric()
##   sample_size = numeric()
##   stop_iter = integer()
## 
## Computational engine: catboost 
## 
## Model fit template:
## bonsai::train_catboost(x = missing_arg(), y = missing_arg(), 
##     weights = missing_arg(), iterations = integer(), depth = integer(), 
##     learning_rate = numeric(), thread_count = 1, allow_writing_files = FALSE, 
##     random_seed = sample.int(10^5, 1))
```

[bonsai::train_catboost()] is a wrapper around `catboost::catboost.train()` (and other functions) that makes it easier to run this model.

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

Unlike many other boosting engines, catboost has native support for categorical predictors. When a factor predictor is passed to the model, catboost will compute target-based statistics to create numeric features from the factor levels. This often provides better performance than using dummy variables.

Non-numeric predictors (i.e., factors) are internally converted to numeric using catboost's native categorical feature handling. In the classification context, non-numeric outcomes (i.e., factors) are also internally converted to numeric.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## Prediction types


``` r
parsnip:::get_from_env("boost_tree_predict") |>
  dplyr::filter(engine == "catboost") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 4 x 2
##   mode           type   
##   <chr>          <chr>  
## 1 regression     numeric
## 2 classification class  
## 3 classification prob   
## 4 classification raw
```

## Other details

### Bagging

The `sample_size` argument is translated to the `subsample` parameter in catboost. The argument is interpreted by catboost as a _proportion_ rather than a count, so bonsai internally reparameterizes the `sample_size` argument with [dials::sample_prop()] during tuning.

The default value for `subsample` depends on the dataset size and the bootstrap type. For datasets with fewer than 100 observations, no sampling is performed (equivalent to `sample_size = 1`). For larger datasets, the default is 0.66 for Poisson or Bernoulli bootstrap and 0.8 for MVS bootstrap.

### Verbosity

bonsai quiets much of the logging output from `catboost::catboost.train()` by default. With default settings, logged warnings and errors will still be passed on to the user. To print out all logs during training, set `quiet = FALSE`.

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.

## Examples

The "Introduction to bonsai" article contains [examples](https://bonsai.tidymodels.org/articles/bonsai.html) of `boost_tree()` with the `"catboost"` engine.

## References

 - [CatBoost: unbiased boosting with categorical features](https://arxiv.org/abs/1706.09516)

 - [CatBoost: gradient boosting with categorical features support](https://catboost.ai/)

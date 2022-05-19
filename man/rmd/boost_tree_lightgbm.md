


For this engine, there are multiple modes: regression and classification

## Tuning Parameters



This model has 6 tuning parameters:

- `tree_depth`: Tree Depth (type: integer, default: -1)

- `trees`: # Trees (type: integer, default: 100)

- `learn_rate`: Learning Rate (type: double, default: 0.1)

- `mtry`: # Randomly Selected Predictors (type: integer, default: see below)

- `min_n`: Minimal Node Size (type: integer, default: 20)

- `loss_reduction`: Minimum Loss Reduction (type: double, default: 0)

The `mtry` parameter gives the _number_ of predictors that will be randomly sampled at each split. The default is to use all predictors. 

Rather than as a number, [lightgbm::lgb.train()]'s `feature_fraction` argument encodes `mtry` as the _proportion_ of predictors that will be randomly sampled at each split. parsnip translates `mtry`, supplied as the _number_ of predictors, to a proportion under the hood. That is, the user should still supply the argument as `mtry` to `boost_tree()`, and do so in its sense as a number rather than a proportion; before passing `mtry` to [lightgbm::lgb.train()], parsnip will convert the `mtry` value to a proportion. 

Note that parsnip's translation can be overridden via the `counts` argument, supplied to `set_engine()`. By default, `counts` is set to `TRUE`, but supplying the argument `counts = FALSE` allows the user to supply `mtry` as a proportion rather than a number.

## Translation from parsnip to the original package (regression)

The **bonsai** extension package is required to fit this model.


```r
boost_tree(
  mtry = integer(), trees = integer(), tree_depth = integer(), 
  learn_rate = numeric(), min_n = integer(), loss_reduction = numeric()
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression") %>%
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
##   loss_reduction = numeric()
## 
## Computational engine: lightgbm 
## 
## Model fit template:
## bonsai::train_lightgbm(x = missing_arg(), y = missing_arg(), 
##     feature_fraction = integer(), num_iterations = integer(), 
##     min_data_in_leaf = integer(), max_depth = integer(), learning_rate = numeric(), 
##     min_gain_to_split = numeric(), verbose = -1, num_threads = 0, 
##     seed = sample.int(10^5, 1), deterministic = TRUE)
```

## Translation from parsnip to the original package (classification)

The **bonsai** extension package is required to fit this model.


```r
boost_tree(
  mtry = integer(), trees = integer(), tree_depth = integer(), 
  learn_rate = numeric(), min_n = integer(), loss_reduction = numeric()
) %>% 
  set_engine("lightgbm") %>% 
  set_mode("classification") %>% 
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
##   loss_reduction = numeric()
## 
## Computational engine: lightgbm 
## 
## Model fit template:
## bonsai::train_lightgbm(x = missing_arg(), y = missing_arg(), 
##     feature_fraction = integer(), num_iterations = integer(), 
##     min_data_in_leaf = integer(), max_depth = integer(), learning_rate = numeric(), 
##     min_gain_to_split = numeric(), verbose = -1, num_threads = 0, 
##     seed = sample.int(10^5, 1), deterministic = TRUE)
```

[train_lightgbm()] is a wrapper around [lightgbm::lgb.train()] (and other functions) that make it easier to run this model. 

## Other details

### Preprocessing


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

Non-numeric predictors (i.e., factors) are internally converted to numeric. In the classification context, non-numeric outcomes (i.e., factors) are also internally converted to numeric. 

### Verbosity

bonsai quiets much of the logging output from [lightgbm::lgb.train()] by default. With default settings, logged warnings and errors will still be passed on to the user. To print out all logs during training, set `quiet = TRUE`.

## Examples 

<!-- TODO: update url to bonsai pkgdown site -->
The "Introduction to bonsai" article contains [examples](https://github.com/tidymodels/bonsai) of `boost_tree()` with the `"lightgbm"` engine.

## References

 - [LightGBM: A Highly Efficient Gradient Boosting Decision Tree](https://papers.nips.cc/paper/2017/hash/6449f44a102fde848669bdd9eb6b76fa-Abstract.html)
 
- Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.




For this engine, there is a single mode: censored regression

## Tuning Parameters



This model has 2 tuning parameters:

- `tree_depth`: Tree Depth (type: integer, default: see below)

- `min_n`: Minimal Node Size (type: integer, default: 20L)

The `tree_depth` parameter defaults to `0` which means no restrictions are applied to tree depth.

An engine-specific parameter for this model is: 

 * `mtry`: the number of predictors, selected at random, that are evaluated for splitting. The default is to use all predictors.

## Translation from parsnip to the original package (censored regression)

The **censored** extension package is required to fit this model.


```r
library(censored)

decision_tree(tree_depth = integer(1), min_n = integer(1)) %>% 
  set_engine("party") %>% 
  set_mode("censored regression") %>% 
  translate()
```

```
## Decision Tree Model Specification (censored regression)
## 
## Main Arguments:
##   tree_depth = integer(1)
##   min_n = integer(1)
## 
## Computational engine: party 
## 
## Model fit template:
## censored::cond_inference_surv_ctree(formula = missing_arg(), 
##     data = missing_arg(), weights = missing_arg(), maxdepth = integer(1), 
##     minsplit = min_rows(0L, data))
```

`censored::cond_inference_surv_ctree()` is a wrapper around [party::ctree()] (and other functions) that makes it easier to run this model. 

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## References

 - Hothorn T, Hornik K, Zeileis A. 2006. Unbiased Recursive Partitioning: A Conditional Inference Framework. _Journal of Computational and Graphical Statistics_, 15(3), 651–674.

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

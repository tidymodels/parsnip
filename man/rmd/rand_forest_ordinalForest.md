


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 3 tuning parameters:

- `mtry`: # Randomly Selected Predictors (type: integer, default: see below)

- `trees`: # Trees (type: integer, default: 500L)

- `min_n`: Minimal Node Size (type: integer, default: see below)

`mtry` depends on the number of columns and the model mode. The default in [ordinalForest::ordfor()] is `floor(sqrt(ncol(x)))`.

`min_n` depends on the mode. For regression, a value of 5 is the default. For classification, a value of 10 is used. 

## Translation from parsnip to the original package (classification)


``` r
rand_forest(
  mtry = integer(1),
  trees = integer(1),
  min_n = integer(1)
) |> 
  set_engine("ordinalForest") |> 
  set_mode("classification") |> 
  translate()
```

```
## Random Forest Model Specification (classification)
## 
## Main Arguments:
##   mtry = integer(1)
##   trees = integer(1)
##   min_n = integer(1)
## 
## Computational engine: ordinalForest 
## 
## Model fit template:
## ordered::ordinalForest_wrapper(x = missing_arg(), y = missing_arg(), 
##     mtry = min_cols(~integer(1), x), ntreefinal = integer(1), 
##     min.node.size = min_rows(~integer(1), x), num.threads = 1, 
##     perffunction = "probability")
```

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## Other notes

By default, parallel processing is turned off. When tuning, it is more efficient to parallelize over the resamples and tuning parameters. To parallelize the construction of the trees within the model, change the `num.threads` argument via [set_engine()]. 

## References

- Hornung R. 2020. Ordinal Forests. _Journal of Classification_ 37:4-17. \doi{10.1007/s00357-018-9302-x}.

- Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

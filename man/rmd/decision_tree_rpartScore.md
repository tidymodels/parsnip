


For this engine, there is a single mode: classification

## Tuning parameters



This model has 3 tuning parameters:

- `tree_depth`: Tree Depth (type: integer, default: 30L)

- `min_n`: Minimal Node Size (type: integer, default: 2L)

- `cost_complexity`: Cost-Complexity Parameter (type: double, default: 0.01)

## Translation from parsnip to the original package


``` r
decision_tree(tree_depth = integer(1), min_n = integer(1), cost_complexity = double(1)) |> 
  set_engine("rpartScore") |> 
  set_mode("classification") |> 
  translate()
```

```
## Decision Tree Model Specification (classification)
## 
## Main Arguments:
##   cost_complexity = double(1)
##   tree_depth = integer(1)
##   min_n = integer(1)
## 
## Computational engine: rpartScore 
## 
## Model fit template:
## ordered::rpartScore_wrapper(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), cp = double(1), maxdepth = integer(1), 
##     minsplit = min_rows(0L, data))
```

<!--
REVIEW: Add any needed sections here.
-->

## References

- Galimberti G, Soffritti G, Di Maso M. 2012. Classification Trees for Ordinal Responses in R: The rpartScore Package. _Journal of Statistical Software_ 47(10):1-25. \doi{10.18637/jss.v047.i10}.

- Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.

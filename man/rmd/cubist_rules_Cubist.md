


For this engine, there is a single mode: regression

## Tuning Parameters



This model has 3 tuning parameters:

- `committees`: # Committees (type: integer, default: 1L)

- `neighbors`: # Nearest Neighbors (type: integer, default: 0L)

- `max_rules`: Max. Rules (type: integer, default: NA_integer)


## Translation from parsnip to the underlying model call  (regression)

The **rules** extension package is required to fit this model.


``` r
library(rules)

cubist_rules(
  committees = integer(1),
  neighbors = integer(1),
  max_rules = integer(1)
) |>
  set_engine("Cubist") |>
  set_mode("regression") |>
  translate()
```

```
## Cubist Model Specification (regression)
## 
## Main Arguments:
##   committees = integer(1)
##   neighbors = integer(1)
##   max_rules = integer(1)
## 
## Computational engine: Cubist 
## 
## Model fit template:
## rules::cubist_fit(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     committees = integer(1), neighbors = integer(1), max_rules = integer(1))
```

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## References

 - Quinlan R (1992). "Learning with Continuous Classes." Proceedings of the 5th Australian Joint Conference On Artificial Intelligence, pp. 343-348.

 - Quinlan R (1993)."Combining Instance-Based and Model-Based Learning." Proceedings of the Tenth International Conference on Machine Learning, pp. 236-243.

 - Kuhn M and Johnson K (2013). _Applied Predictive Modeling_. Springer.

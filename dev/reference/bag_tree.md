# Ensembles of decision trees

`bag_tree()` defines an ensemble of decision trees. This function can
fit classification, regression, and censored regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`rpart`](https://parsnip.tidymodels.org/dev/reference/details_bag_tree_rpart.md)`¹²`

- [`C5.0`](https://parsnip.tidymodels.org/dev/reference/details_bag_tree_C5.0.md)`²`

¹ The default engine. ² Requires a parsnip extension package for
censored regression, classification, and regression.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
bag_tree(
  mode = "unknown",
  cost_complexity = 0,
  tree_depth = NULL,
  min_n = 2,
  class_cost = NULL,
  engine = "rpart"
)
```

## Arguments

- mode:

  A single character string for the prediction outcome mode. Possible
  values for this model are "unknown", "regression", "classification",
  or "censored regression".

- cost_complexity:

  A positive number for the the cost/complexity parameter (a.k.a. `Cp`)
  used by CART models (specific engines only).

- tree_depth:

  An integer for the maximum depth of the tree (i.e. number of splits)
  (specific engines only).

- min_n:

  An integer for the minimum number of data points in a node that is
  required for the node to be split further.

- class_cost:

  A non-negative scalar for a class cost (where a cost of 1 means no
  extra cost). This is useful for when the first level of the outcome
  factor is the minority class. If this is not the case, values between
  zero and one can be used to bias to the second level of the factor.

- engine:

  A single character string specifying what computational engine to use
  for fitting.

## Details

This function only defines what *type* of model is being fit. Once an
engine is specified, the *method* to fit the model is also defined. See
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md)
for more on setting the engine, including how to set engine arguments.

The model is not trained or fit until the
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md) function
is used with the data.

Each of the arguments in this function other than `mode` and `engine`
are captured as
[quosures](https://rlang.r-lib.org/reference/topic-quosure.html). To
pass values programmatically, use the [injection
operator](https://rlang.r-lib.org/reference/injection-operator.html)
like so:

    value <- 1
    bag_tree(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`rpart engine details`](https://parsnip.tidymodels.org/dev/reference/details_bag_tree_rpart.md),
[`C5.0 engine details`](https://parsnip.tidymodels.org/dev/reference/details_bag_tree_C5.0.md)

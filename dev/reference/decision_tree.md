# Decision trees

`decision_tree()` defines a model as a set of `if/then` statements that
creates a tree-based structure. This function can fit classification,
regression, and censored regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`rpart`](https://parsnip.tidymodels.org/dev/reference/details_decision_tree_rpart.md)`¹²`

- [`C5.0`](https://parsnip.tidymodels.org/dev/reference/details_decision_tree_C5.0.md)

- [`partykit`](https://parsnip.tidymodels.org/dev/reference/details_decision_tree_partykit.md)`²`

- [`spark`](https://parsnip.tidymodels.org/dev/reference/details_decision_tree_spark.md)

¹ The default engine. ² Requires a parsnip extension package for
censored regression, classification, and regression.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
decision_tree(
  mode = "unknown",
  engine = "rpart",
  cost_complexity = NULL,
  tree_depth = NULL,
  min_n = NULL
)
```

## Arguments

- mode:

  A single character string for the prediction outcome mode. Possible
  values for this model are "unknown", "regression", "classification",
  or "censored regression".

- engine:

  A single character string specifying what computational engine to use
  for fitting.

- cost_complexity:

  A positive number for the the cost/complexity parameter (a.k.a. `Cp`)
  used by CART models (specific engines only).

- tree_depth:

  An integer for maximum depth of the tree.

- min_n:

  An integer for the minimum number of data points in a node that are
  required for the node to be split further.

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
    decision_tree(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`rpart engine details`](https://parsnip.tidymodels.org/dev/reference/details_decision_tree_rpart.md),
[`C5.0 engine details`](https://parsnip.tidymodels.org/dev/reference/details_decision_tree_C5.0.md),
[`partykit engine details`](https://parsnip.tidymodels.org/dev/reference/details_decision_tree_partykit.md),
[`spark engine details`](https://parsnip.tidymodels.org/dev/reference/details_decision_tree_spark.md)

## Examples

``` r
show_engines("decision_tree")
#> # A tibble: 5 × 2
#>   engine mode          
#>   <chr>  <chr>         
#> 1 rpart  classification
#> 2 rpart  regression    
#> 3 C5.0   classification
#> 4 spark  classification
#> 5 spark  regression    

decision_tree(mode = "classification", tree_depth = 5)
#> Decision Tree Model Specification (classification)
#> 
#> Main Arguments:
#>   tree_depth = 5
#> 
#> Computational engine: rpart 
#> 
```

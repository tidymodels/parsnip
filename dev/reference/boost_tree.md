# Boosted trees

`boost_tree()` defines a model that creates a series of decision trees
forming an ensemble. Each tree depends on the results of previous trees.
All trees in the ensemble are combined to produce a final prediction.
This function can fit classification, regression, and censored
regression models.

`Rd parsnip:::make_engine_list("boost_tree")`

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
boost_tree(
  mode = "unknown",
  engine = "xgboost",
  mtry = NULL,
  trees = NULL,
  min_n = NULL,
  tree_depth = NULL,
  learn_rate = NULL,
  loss_reduction = NULL,
  sample_size = NULL,
  stop_iter = NULL
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

- mtry:

  A number for the number (or proportion) of predictors that will be
  randomly sampled at each split when creating the tree models (specific
  engines only).

- trees:

  An integer for the number of trees contained in the ensemble.

- min_n:

  An integer for the minimum number of data points in a node that is
  required for the node to be split further.

- tree_depth:

  An integer for the maximum depth of the tree (i.e. number of splits)
  (specific engines only).

- learn_rate:

  A number for the rate at which the boosting algorithm adapts from
  iteration-to-iteration (specific engines only). This is sometimes
  referred to as the shrinkage parameter.

- loss_reduction:

  A number for the reduction in the loss function required to split
  further (specific engines only).

- sample_size:

  A number for the number (or proportion) of data that is exposed to the
  fitting routine. For `xgboost`, the sampling is done at each iteration
  while `C5.0` samples once during training.

- stop_iter:

  The number of iterations without improvement before stopping (specific
  engines only).

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
    boost_tree(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

`Rd parsnip:::make_seealso_list("boost_tree")`,
[`xgb_train()`](https://parsnip.tidymodels.org/dev/reference/xgb_train.md),
[`C5.0_train()`](https://parsnip.tidymodels.org/dev/reference/C5.0_train.md)

## Examples

``` r
show_engines("boost_tree")
#> # A tibble: 6 × 2
#>   engine  mode               
#>   <chr>   <chr>              
#> 1 xgboost classification     
#> 2 xgboost regression         
#> 3 xgboost quantile regression
#> 4 C5.0    classification     
#> 5 spark   classification     
#> 6 spark   regression         

boost_tree(mode = "classification", trees = 20)
#> Boosted Tree Model Specification (classification)
#> 
#> Main Arguments:
#>   trees = 20
#> 
#> Computational engine: xgboost 
#> 
```

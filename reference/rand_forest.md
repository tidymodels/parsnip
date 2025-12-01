# Random forest

`rand_forest()` defines a model that creates a large number of decision
trees, each independent of the others. The final prediction uses all
predictions from the individual trees and combines them. This function
can fit classification, regression, and censored regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`ranger`](https://parsnip.tidymodels.org/reference/details_rand_forest_ranger.md)`¹`

- [`aorsf`](https://parsnip.tidymodels.org/reference/details_rand_forest_aorsf.md)`²`

- [`grf`](https://parsnip.tidymodels.org/reference/details_rand_forest_grf.md)

- [`h2o`](https://parsnip.tidymodels.org/reference/details_rand_forest_h2o.md)`²`

- [`partykit`](https://parsnip.tidymodels.org/reference/details_rand_forest_partykit.md)`²`

- [`randomForest`](https://parsnip.tidymodels.org/reference/details_rand_forest_randomForest.md)

- [`spark`](https://parsnip.tidymodels.org/reference/details_rand_forest_spark.md)

¹ The default engine. ² Requires a parsnip extension package for
censored regression, classification, and regression.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
rand_forest(
  mode = "unknown",
  engine = "ranger",
  mtry = NULL,
  trees = NULL,
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

- mtry:

  An integer for the number of predictors that will be randomly sampled
  at each split when creating the tree models.

- trees:

  An integer for the number of trees contained in the ensemble.

- min_n:

  An integer for the minimum number of data points in a node that are
  required for the node to be split further.

## Details

This function only defines what *type* of model is being fit. Once an
engine is specified, the *method* to fit the model is also defined. See
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md)
for more on setting the engine, including how to set engine arguments.

The model is not trained or fit until the
[`fit()`](https://parsnip.tidymodels.org/reference/fit.md) function is
used with the data.

Each of the arguments in this function other than `mode` and `engine`
are captured as
[quosures](https://rlang.r-lib.org/reference/topic-quosure.html). To
pass values programmatically, use the [injection
operator](https://rlang.r-lib.org/reference/injection-operator.html)
like so:

    value <- 1
    rand_forest(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`ranger engine details`](https://parsnip.tidymodels.org/reference/details_rand_forest_ranger.md),
[`aorsf engine details`](https://parsnip.tidymodels.org/reference/details_rand_forest_aorsf.md),
[`grf engine details`](https://parsnip.tidymodels.org/reference/details_rand_forest_grf.md),
[`h2o engine details`](https://parsnip.tidymodels.org/reference/details_rand_forest_h2o.md),
[`partykit engine details`](https://parsnip.tidymodels.org/reference/details_rand_forest_partykit.md),
[`randomForest engine details`](https://parsnip.tidymodels.org/reference/details_rand_forest_randomForest.md),
[`spark engine details`](https://parsnip.tidymodels.org/reference/details_rand_forest_spark.md)

## Examples

``` r
show_engines("rand_forest")
#> # A tibble: 9 × 2
#>   engine       mode               
#>   <chr>        <chr>              
#> 1 ranger       classification     
#> 2 ranger       regression         
#> 3 randomForest classification     
#> 4 randomForest regression         
#> 5 spark        classification     
#> 6 spark        regression         
#> 7 grf          classification     
#> 8 grf          regression         
#> 9 grf          quantile regression

rand_forest(mode = "classification", trees = 2000)
#> Random Forest Model Specification (classification)
#> 
#> Main Arguments:
#>   trees = 2000
#> 
#> Computational engine: ranger 
#> 
```

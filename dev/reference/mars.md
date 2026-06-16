# Multivariate adaptive regression splines (MARS)

`mars()` defines a generalized linear model that uses artificial
features for some predictors. These features resemble hinge functions
and the result is a model that is a segmented regression in small
dimensions. This function can fit classification and regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`earth`](https://parsnip.tidymodels.org/dev/reference/details_mars_earth.md)`¹`

¹ The default engine.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
mars(
  mode = "unknown",
  engine = "earth",
  num_terms = NULL,
  prod_degree = NULL,
  prune_method = NULL
)
```

## Arguments

- mode:

  A single character string for the prediction outcome mode. Possible
  values for this model are "unknown", "regression", or
  "classification".

- engine:

  A single character string specifying what computational engine to use
  for fitting.

- num_terms:

  The number of features that will be retained in the final model,
  including the intercept.

- prod_degree:

  The highest possible interaction degree.

- prune_method:

  The pruning method.

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
    mars(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`earth engine details`](https://parsnip.tidymodels.org/dev/reference/details_mars_earth.md)

## Examples

``` r
show_engines("mars")
#> # A tibble: 2 × 2
#>   engine mode          
#>   <chr>  <chr>         
#> 1 earth  classification
#> 2 earth  regression    

mars(mode = "regression", num_terms = 5)
#> MARS Model Specification (regression)
#> 
#> Main Arguments:
#>   num_terms = 5
#> 
#> Computational engine: earth 
#> 
```

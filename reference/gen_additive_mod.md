# Generalized additive models (GAMs)

`gen_additive_mod()` defines a model that can use smoothed functions of
numeric predictors in a generalized linear model. This function can fit
classification and regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`mgcv`](https://parsnip.tidymodels.org/reference/details_gen_additive_mod_mgcv.md)`¹`

¹ The default engine.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
gen_additive_mod(
  mode = "unknown",
  select_features = NULL,
  adjust_deg_free = NULL,
  engine = "mgcv"
)
```

## Arguments

- mode:

  A single character string for the prediction outcome mode. Possible
  values for this model are "unknown", "regression", or
  "classification".

- select_features:

  `TRUE` or `FALSE.` If `TRUE`, the model has the ability to eliminate a
  predictor (via penalization). Increasing `adjust_deg_free` will
  increase the likelihood of removing predictors.

- adjust_deg_free:

  If `select_features = TRUE`, then acts as a multiplier for smoothness.
  Increase this beyond 1 to produce smoother models.

- engine:

  A single character string specifying what computational engine to use
  for fitting.

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
    gen_additive_mod(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`mgcv engine details`](https://parsnip.tidymodels.org/reference/details_gen_additive_mod_mgcv.md)

## Examples

``` r
show_engines("gen_additive_mod")
#> # A tibble: 2 × 2
#>   engine mode          
#>   <chr>  <chr>         
#> 1 mgcv   regression    
#> 2 mgcv   classification

gen_additive_mod()
#> GAM Model Specification (unknown mode)
#> 
#> Computational engine: mgcv 
#> 
```

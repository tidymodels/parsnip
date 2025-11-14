# Null model

Fit a single mean or largest class model. `null_model()` is the
user-facing function that relies on the underlying computational
function,
[`nullmodel()`](https://parsnip.tidymodels.org/dev/reference/nullmodel.md).

## Usage

``` r
null_model(mode = "classification", engine = "parsnip")
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  values for this model are `"regression"` and `"classification"`.

- engine:

  A single character string specifying what computational engine to use
  for fitting. Possible engines are listed below. The default for this
  model is `"parsnip"`.

## Details

`null_model()` defines a simple, non-informative model. It doesn't have
any main arguments. This function can fit classification and regression
models.

`null_model()` emulates other model building functions, but returns the
simplest model possible given a training set: a single mean for numeric
outcomes and the most prevalent class for factor outcomes. When class
probabilities are requested, the percentage of the training set samples
with the most prevalent class is returned.

## Engine Details

Engines may have pre-set default arguments when executing the model fit
call. For this type of model, the template of the fit calls are below:

### parsnip

    null_model() |>
      set_engine("parsnip") |>
      set_mode("regression") |>
      translate()

    ## Null Model Specification (regression)
    ##
    ## Computational engine: parsnip
    ##
    ## Model fit template:
    ## parsnip::nullmodel(x = missing_arg(), y = missing_arg())

    null_model() |>
      set_engine("parsnip") |>
      set_mode("classification") |>
      translate()

    ## Null Model Specification (classification)
    ##
    ## Computational engine: parsnip
    ##
    ## Model fit template:
    ## parsnip::nullmodel(x = missing_arg(), y = missing_arg())

## See also

[`fit.model_spec()`](https://parsnip.tidymodels.org/dev/reference/fit.md)

## Examples

``` r
null_model(mode = "regression")
#> Null Model Specification (regression)
#> 
#> Computational engine: parsnip 
#> 
```

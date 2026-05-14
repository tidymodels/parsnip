# Ordinal regression

`ordinal_reg()` defines a generalized linear model that predicts an
ordinal outcome. This function can fit classification models.

`Rd parsnip:::make_engine_list("ordinal_reg")`

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
ordinal_reg(
  mode = "classification",
  ordinal_link = NULL,
  odds_link = NULL,
  penalty = NULL,
  mixture = NULL,
  engine = "polr"
)
```

## Arguments

- mode:

  A single character string for the prediction outcome mode. The only
  possible value for this model is "classification".

- ordinal_link:

  The ordinal link function.

- odds_link:

  The odds or probability link function.

- penalty:

  A non-negative number representing the total amount of regularization
  (specific engines only).

- mixture:

  A number between zero and one (inclusive) denoting the proportion of
  L1 regularization (i.e. lasso) in the model.

  - `mixture = 1` specifies a pure lasso model,

  - `mixture = 0` specifies a ridge regression model, and

  - `0 < mixture < 1` specifies an elastic net model, interpolating
    lasso and ridge.

  Available for specific engines only.

- engine:

  A single character string specifying what computational engine to use
  for fitting. Possible engines are listed below. The default for this
  model is `"polr"`.

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
    ordinal_reg(argument = !!value)

Ordinal regression models include cumulative, sequential, and adjacent
structures.

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

`Rd parsnip:::make_seealso_list("ordinal_reg")`

## Examples

``` r
show_engines("ordinal_reg")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: engine <chr>, mode <chr>

ordinal_reg(mode = "classification")
#> ! parsnip could not locate an implementation for `ordinal_reg`
#>   classification model specifications.
#> ℹ The parsnip extension package ordered implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> Ordinal Regression Model Specification (classification)
#> 
#> Computational engine: polr 
#> 
```

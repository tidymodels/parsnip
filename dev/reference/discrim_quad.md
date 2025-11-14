# Quadratic discriminant analysis

`discrim_quad()` defines a model that estimates a multivariate
distribution for the predictors separately for the data in each class
(usually Gaussian with separate covariance matrices). Bayes' theorem is
used to compute the probability of each class, given the predictor
values. This function can fit classification models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`MASS`](https://parsnip.tidymodels.org/dev/reference/details_discrim_quad_MASS.md)`¹²`

- [`sparsediscrim`](https://parsnip.tidymodels.org/dev/reference/details_discrim_quad_sparsediscrim.md)`²`

¹ The default engine. ² Requires a parsnip extension package.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
discrim_quad(
  mode = "classification",
  regularization_method = NULL,
  engine = "MASS"
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "classification".

- regularization_method:

  A character string for the type of regularized estimation. Possible
  values are: "`diagonal`", "`shrink_cov`", and "`shrink_mean`"
  (`sparsediscrim` engine only).

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
    discrim_quad(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`MASS engine details`](https://parsnip.tidymodels.org/dev/reference/details_discrim_quad_MASS.md),
[`sparsediscrim engine details`](https://parsnip.tidymodels.org/dev/reference/details_discrim_quad_sparsediscrim.md)

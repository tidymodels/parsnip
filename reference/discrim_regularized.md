# Regularized discriminant analysis

`discrim_regularized()` defines a model that estimates a multivariate
distribution for the predictors separately for the data in each class.
The structure of the model can be LDA, QDA, or some amalgam of the two.
Bayes' theorem is used to compute the probability of each class, given
the predictor values. This function can fit classification models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`klaR`](https://parsnip.tidymodels.org/reference/details_discrim_regularized_klaR.md)`¹²`

¹ The default engine. ² Requires a parsnip extension package.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
discrim_regularized(
  mode = "classification",
  frac_common_cov = NULL,
  frac_identity = NULL,
  engine = "klaR"
)
```

## Arguments

- mode:

  A single character string for the prediction outcome mode. Possible
  values for this model are "unknown", "regression", or
  "classification".

- frac_common_cov, frac_identity:

  Numeric values between zero and one.

- engine:

  A single character string specifying what computational engine to use
  for fitting.

## Details

There are many ways of regularizing models. For example, one form of
regularization is to penalize model parameters. Similarly, the classic
James–Stein regularization approach shrinks the model structure to a
less complex form.

The model fits a very specific type of regularized model by Friedman
(1989) that uses two types of regularization. One modulates how
class-specific the covariance matrix should be. This allows the model to
balance between LDA and QDA. The second regularization component shrinks
the covariance matrix towards the identity matrix.

For the penalization approach,
[`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.md)
with a `mda` engine can be used. Other regularization methods can be
used with
[`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.md)
and
[`discrim_quad()`](https://parsnip.tidymodels.org/reference/discrim_quad.md)
can used via the `sparsediscrim` engine for those functions.

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
    discrim_regularized(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

Friedman, J (1989). Regularized Discriminant Analysis. *Journal of the
American Statistical Association*, 84, 165-175.

## See also

[`fit()`](https://parsnip.tidymodels.org/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`klaR engine details`](https://parsnip.tidymodels.org/reference/details_discrim_regularized_klaR.md)

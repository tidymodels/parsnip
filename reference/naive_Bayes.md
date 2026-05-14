# Naive Bayes models

`naive_Bayes()` defines a model that uses Bayes' theorem to compute the
probability of each class, given the predictor values. This function can
fit classification models.

`Rd parsnip:::make_engine_list("naive_Bayes")`

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
naive_Bayes(
  mode = "classification",
  smoothness = NULL,
  Laplace = NULL,
  engine = "klaR"
)
```

## Arguments

- mode:

  A single character string for the prediction outcome mode. Possible
  values for this model are "unknown", "regression", or
  "classification".

- smoothness:

  An non-negative number representing the the relative smoothness of the
  class boundary. Smaller examples result in model flexible boundaries
  and larger values generate class boundaries that are less adaptable

- Laplace:

  A non-negative value for the Laplace correction to smoothing
  low-frequency counts.

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
    naive_Bayes(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

`Rd parsnip:::make_seealso_list("naive_Bayes")`

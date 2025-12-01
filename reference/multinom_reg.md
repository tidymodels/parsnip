# Multinomial regression

`multinom_reg()` defines a model that uses linear predictors to predict
multiclass data using the multinomial distribution. This function can
fit classification models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`nnet`](https://parsnip.tidymodels.org/reference/details_multinom_reg_nnet.md)`¹`

- [`brulee`](https://parsnip.tidymodels.org/reference/details_multinom_reg_brulee.md)

- [`glmnet`](https://parsnip.tidymodels.org/reference/details_multinom_reg_glmnet.md)

- [`h2o`](https://parsnip.tidymodels.org/reference/details_multinom_reg_h2o.md)`²`

- [`keras`](https://parsnip.tidymodels.org/reference/details_multinom_reg_keras.md)

- [`spark`](https://parsnip.tidymodels.org/reference/details_multinom_reg_spark.md)

¹ The default engine. ² Requires a parsnip extension package.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
multinom_reg(
  mode = "classification",
  engine = "nnet",
  penalty = NULL,
  mixture = NULL
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "classification".

- engine:

  A single character string specifying what computational engine to use
  for fitting. Possible engines are listed below. The default for this
  model is `"nnet"`.

- penalty:

  A non-negative number representing the total amount of regularization
  (specific engines only). For `keras` models, this corresponds to
  purely L2 regularization (aka weight decay) while the other models can
  be a combination of L1 and L2 (depending on the value of `mixture`).

- mixture:

  A number between zero and one (inclusive) giving the proportion of L1
  regularization (i.e. lasso) in the model.

  - `mixture = 1` specifies a pure lasso model,

  - `mixture = 0` specifies a ridge regression model, and

  - `0 < mixture < 1` specifies an elastic net model, interpolating
    lasso and ridge.

  Available for specific engines only.

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
    multinom_reg(argument = !!value)

This model fits a classification model for multiclass outcomes; for
binary outcomes, see
[`logistic_reg()`](https://parsnip.tidymodels.org/reference/logistic_reg.md).

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`nnet engine details`](https://parsnip.tidymodels.org/reference/details_multinom_reg_nnet.md),
[`brulee engine details`](https://parsnip.tidymodels.org/reference/details_multinom_reg_brulee.md),
[`glmnet engine details`](https://parsnip.tidymodels.org/reference/details_multinom_reg_glmnet.md),
[`h2o engine details`](https://parsnip.tidymodels.org/reference/details_multinom_reg_h2o.md),
[`keras engine details`](https://parsnip.tidymodels.org/reference/details_multinom_reg_keras.md),
[`spark engine details`](https://parsnip.tidymodels.org/reference/details_multinom_reg_spark.md)

## Examples

``` r
show_engines("multinom_reg")
#> # A tibble: 5 × 2
#>   engine mode          
#>   <chr>  <chr>         
#> 1 glmnet classification
#> 2 spark  classification
#> 3 keras  classification
#> 4 nnet   classification
#> 5 brulee classification

multinom_reg()
#> Multinomial Regression Model Specification (classification)
#> 
#> Computational engine: nnet 
#> 
```

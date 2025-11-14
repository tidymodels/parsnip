# Logistic regression

`logistic_reg()` defines a generalized linear model for binary outcomes.
A linear combination of the predictors is used to model the log odds of
an event. This function can fit classification models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`glm`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_glm.md)`¹`

- [`brulee`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_brulee.md)

- [`gee`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_gee.md)`²`

- [`glmer`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_glmer.md)`²`

- [`glmnet`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_glmnet.md)

- [`h2o`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_h2o.md)`²`

- [`keras`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_keras.md)

- [`LiblineaR`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_LiblineaR.md)

- [`spark`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_spark.md)

- [`stan`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_stan.md)

- [`stan_glmer`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_stan_glmer.md)`²`

¹ The default engine. ² Requires a parsnip extension package.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
logistic_reg(
  mode = "classification",
  engine = "glm",
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
  model is `"glm"`.

- penalty:

  A non-negative number representing the total amount of regularization
  (specific engines only). For `keras` models, this corresponds to
  purely L2 regularization (aka weight decay) while the other models can
  be either or a combination of L1 and L2 (depending on the value of
  `mixture`).

- mixture:

  A number between zero and one (inclusive) giving the proportion of L1
  regularization (i.e. lasso) in the model.

  - `mixture = 1` specifies a pure lasso model,

  - `mixture = 0` specifies a ridge regression model, and

  - `0 < mixture < 1` specifies an elastic net model, interpolating
    lasso and ridge.

  Available for specific engines only. For `LiblineaR` models, `mixture`
  must be exactly 1 or 0 only.

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
    logistic_reg(argument = !!value)

This model fits a classification model for binary outcomes; for
multiclass outcomes, see
[`multinom_reg()`](https://parsnip.tidymodels.org/dev/reference/multinom_reg.md).

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`glm engine details`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_glm.md),
[`brulee engine details`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_brulee.md),
[`gee engine details`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_gee.md),
[`glmer engine details`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_glmer.md),
[`glmnet engine details`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_glmnet.md),
[`h2o engine details`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_h2o.md),
[`keras engine details`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_keras.md),
[`LiblineaR engine details`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_LiblineaR.md),
[`spark engine details`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_spark.md),
[`stan engine details`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_stan.md),
[`stan_glmer engine details`](https://parsnip.tidymodels.org/dev/reference/details_logistic_reg_stan_glmer.md)

## Examples

``` r
show_engines("logistic_reg")
#> # A tibble: 7 × 2
#>   engine    mode          
#>   <chr>     <chr>         
#> 1 glm       classification
#> 2 glmnet    classification
#> 3 LiblineaR classification
#> 4 spark     classification
#> 5 keras     classification
#> 6 stan      classification
#> 7 brulee    classification

logistic_reg()
#> Logistic Regression Model Specification (classification)
#> 
#> Computational engine: glm 
#> 
```

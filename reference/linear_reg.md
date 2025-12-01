# Linear regression

`linear_reg()` defines a model that can predict numeric values from
predictors using a linear function. This function can fit regression
models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`lm`](https://parsnip.tidymodels.org/reference/details_linear_reg_lm.md)`¹`

- [`brulee`](https://parsnip.tidymodels.org/reference/details_linear_reg_brulee.md)

- [`gee`](https://parsnip.tidymodels.org/reference/details_linear_reg_gee.md)`²`

- [`glm`](https://parsnip.tidymodels.org/reference/details_linear_reg_glm.md)

- [`glmer`](https://parsnip.tidymodels.org/reference/details_linear_reg_glmer.md)`²`

- [`glmnet`](https://parsnip.tidymodels.org/reference/details_linear_reg_glmnet.md)

- [`gls`](https://parsnip.tidymodels.org/reference/details_linear_reg_gls.md)`²`

- [`h2o`](https://parsnip.tidymodels.org/reference/details_linear_reg_h2o.md)`²`

- [`keras`](https://parsnip.tidymodels.org/reference/details_linear_reg_keras.md)

- [`lme`](https://parsnip.tidymodels.org/reference/details_linear_reg_lme.md)`²`

- [`lmer`](https://parsnip.tidymodels.org/reference/details_linear_reg_lmer.md)`²`

- [`quantreg`](https://parsnip.tidymodels.org/reference/details_linear_reg_quantreg.md)

- [`spark`](https://parsnip.tidymodels.org/reference/details_linear_reg_spark.md)

- [`stan`](https://parsnip.tidymodels.org/reference/details_linear_reg_stan.md)

- [`stan_glmer`](https://parsnip.tidymodels.org/reference/details_linear_reg_stan_glmer.md)`²`

¹ The default engine. ² Requires a parsnip extension package for
regression.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
linear_reg(mode = "regression", engine = "lm", penalty = NULL, mixture = NULL)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression".

- engine:

  A single character string specifying what computational engine to use
  for fitting. Possible engines are listed below. The default for this
  model is `"lm"`.

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
    linear_reg(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`lm engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_lm.md),
[`brulee engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_brulee.md),
[`gee engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_gee.md),
[`glm engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_glm.md),
[`glmer engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_glmer.md),
[`glmnet engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_glmnet.md),
[`gls engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_gls.md),
[`h2o engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_h2o.md),
[`keras engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_keras.md),
[`lme engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_lme.md),
[`lmer engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_lmer.md),
[`quantreg engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_quantreg.md),
[`spark engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_spark.md),
[`stan engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_stan.md),
[`stan_glmer engine details`](https://parsnip.tidymodels.org/reference/details_linear_reg_stan_glmer.md)

## Examples

``` r
show_engines("linear_reg")
#> # A tibble: 8 × 2
#>   engine   mode               
#>   <chr>    <chr>              
#> 1 lm       regression         
#> 2 glm      regression         
#> 3 glmnet   regression         
#> 4 stan     regression         
#> 5 spark    regression         
#> 6 keras    regression         
#> 7 brulee   regression         
#> 8 quantreg quantile regression

linear_reg()
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 
```

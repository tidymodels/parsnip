# Proportional hazards regression

[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
fits a regularized Cox proportional hazards model.

## Details

For this engine, there is a single mode: censored regression

### Tuning Parameters

This model has 2 tuning parameters:

- `penalty`: Amount of Regularization (type: double, default: see below)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 1.0)

The `penalty` parameter has no default and requires a single numeric
value. For more details about this, and the `glmnet` model in general,
see
[glmnet-details](https://parsnip.tidymodels.org/reference/glmnet-details.md).
As for `mixture`:

- `mixture = 1` specifies a pure lasso model,

- `mixture = 0` specifies a ridge regression model, and

- `0 < mixture < 1` specifies an elastic net model, interpolating lasso
  and ridge.

### Translation from parsnip to the original package

The **censored** extension package is required to fit this model.

    library(censored)

    proportional_hazards(penalty = double(1), mixture = double(1)) |>
      set_engine("glmnet") |>
      translate()

    ## Proportional Hazards Model Specification (censored regression)
    ##
    ## Main Arguments:
    ##   penalty = 0
    ##   mixture = double(1)
    ##
    ## Computational engine: glmnet
    ##
    ## Model fit template:
    ## censored::coxnet_train(formula = missing_arg(), data = missing_arg(),
    ##     weights = missing_arg(), alpha = double(1))

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/reference/fit.md), parsnip will
convert factor columns to indicators.

Predictors should have the same scale. One way to achieve this is to
center and scale each so that each predictor has mean zero and a
variance of one. By default,
[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
uses the argument `standardize = TRUE` to center and scale the data.

### Other details

The model does not fit an intercept.

The model formula (which is required) can include *special* terms, such
as [`survival::strata()`](https://rdrr.io/pkg/survival/man/strata.html).
This allows the baseline hazard to differ between groups contained in
the function. (To learn more about using special terms in formulas with
tidymodels, see
[`?model_formula`](https://parsnip.tidymodels.org/reference/model_formula.md).)
The column used inside `strata()` is treated as qualitative no matter
its type. This is different than the syntax offered by the
[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
package (i.e.,
[`glmnet::stratifySurv()`](https://glmnet.stanford.edu/reference/stratifySurv.html))
which is not recommended here.

For example, in this model, the numeric column `rx` is used to estimate
two different baseline hazards for each value of the column:

    library(survival)
    library(censored)
    library(dplyr)
    library(tidyr)

    mod <-
      proportional_hazards(penalty = 0.01) |>
      set_engine("glmnet", nlambda = 5) |>
      fit(Surv(futime, fustat) ~ age + ecog.ps + strata(rx), data = ovarian)

    pred_data <- data.frame(age = c(50, 50), ecog.ps = c(1, 1), rx = c(1, 2))

    # Different survival probabilities for different values of 'rx'
    predict(mod, pred_data, type = "survival", time = 500) |>
      bind_cols(pred_data) |>
      unnest(.pred)

    ## # A tibble: 2 x 5
    ##   .eval_time .pred_survival   age ecog.ps    rx
    ##        <dbl>          <dbl> <dbl>   <dbl> <dbl>
    ## 1        500          0.666    50       1     1
    ## 2        500          0.769    50       1     2

Note that columns used in the `strata()` function *will* also be
estimated in the regular portion of the model (i.e., within the linear
predictor).

Predictions of type `"time"` are predictions of the mean survival time.

### Linear predictor values

Since risk regression and parametric survival models are modeling
different characteristics (e.g. relative hazard versus event time),
their linear predictors will be going in opposite directions.

For example, for parametric models, the linear predictor *increases with
time*. For proportional hazards models the linear predictor *decreases
with time* (since hazard is increasing). As such, the linear predictors
for these two quantities will have opposite signs.

tidymodels does not treat different models differently when computing
performance metrics. To standardize across model types, the default for
proportional hazards models is to have *increasing values with time*. As
a result, the sign of the linear predictor will be the opposite of the
value produced by the
[`predict()`](https://rdrr.io/r/stats/predict.html) method in the engine
package.

This behavior can be changed by using the `increasing` argument when
calling [`predict()`](https://rdrr.io/r/stats/predict.html) on a model
object.

### Case weights

This model can utilize case weights during model fitting. To use them,
see the documentation in
[case_weights](https://parsnip.tidymodels.org/reference/case_weights.md)
and the examples on `tidymodels.org`.

The [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) arguments
have arguments called `case_weights` that expect vectors of case
weights.

### Saving fitted model objects

This model object contains data that are not required to make
predictions. When saving the model for the purpose of prediction, the
size of the saved object might be substantially reduced by using
functions from the [butcher](https://butcher.tidymodels.org) package.

### Prediction types

    parsnip:::get_from_env("proportional_hazards_predict") |>
      dplyr::filter(engine == "glmnet") |>
      dplyr::select(mode, type)

    ## # A tibble: 4 x 2
    ##   mode                type
    ##   <chr>               <chr>
    ## 1 censored regression linear_pred
    ## 2 censored regression survival
    ## 3 censored regression time
    ## 4 censored regression raw

## References

- Simon N, Friedman J, Hastie T, Tibshirani R. 2011. “Regularization
  Paths for Cox’s Proportional Hazards Model via Coordinate Descent.”
  *Journal of Statistical Software*, Articles 39 (5): 1–13. .

- Hastie T, Tibshirani R, Wainwright M. 2015. *Statistical Learning with
  Sparsity*. CRC Press.

- Kuhn M, Johnson K. 2013. *Applied Predictive Modeling*. Springer.

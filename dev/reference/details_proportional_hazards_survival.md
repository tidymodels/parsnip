# Proportional hazards regression

[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) fits
a Cox proportional hazards model.

## Details

For this engine, there is a single mode: censored regression

### Tuning Parameters

This model has no tuning parameters.

### Translation from parsnip to the original package

The **censored** extension package is required to fit this model.

    library(censored)

    proportional_hazards() |>
      set_engine("survival") |>
      set_mode("censored regression") |>
      translate()

    ## Proportional Hazards Model Specification (censored regression)
    ##
    ## Computational engine: survival
    ##
    ## Model fit template:
    ## survival::coxph(formula = missing_arg(), data = missing_arg(),
    ##     weights = missing_arg(), x = TRUE, model = TRUE)

### Other details

The model does not fit an intercept.

The main interface for this model uses the formula method since the
model specification typically involved the use of
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html).

The model formula can include *special* terms, such as
[`survival::strata()`](https://rdrr.io/pkg/survival/man/strata.html).
The allows the baseline hazard to differ between groups contained in the
function. The column used inside
[`strata()`](https://rdrr.io/pkg/survival/man/strata.html) is treated as
qualitative no matter its type. To learn more about using special terms
in formulas with tidymodels, see
[`?model_formula`](https://parsnip.tidymodels.org/dev/reference/model_formula.md).

For example, in this model, the numeric column `rx` is used to estimate
two different baseline hazards for each value of the column:

    library(survival)

    proportional_hazards() |>
      fit(Surv(futime, fustat) ~ age + strata(rx), data = ovarian) |>
      extract_fit_engine() |>
      # Two different hazards for each value of 'rx'
      basehaz()

    ##        hazard time strata
    ## 1  0.02250134   59   rx=1
    ## 2  0.05088586  115   rx=1
    ## 3  0.09467873  156   rx=1
    ## 4  0.14809975  268   rx=1
    ## 5  0.30670509  329   rx=1
    ## 6  0.46962698  431   rx=1
    ## 7  0.46962698  448   rx=1
    ## 8  0.46962698  477   rx=1
    ## 9  1.07680229  638   rx=1
    ## 10 1.07680229  803   rx=1
    ## 11 1.07680229  855   rx=1
    ## 12 1.07680229 1040   rx=1
    ## 13 1.07680229 1106   rx=1
    ## 14 0.05843331  353   rx=2
    ## 15 0.12750063  365   rx=2
    ## 16 0.12750063  377   rx=2
    ## 17 0.12750063  421   rx=2
    ## 18 0.23449656  464   rx=2
    ## 19 0.35593895  475   rx=2
    ## 20 0.50804209  563   rx=2
    ## 21 0.50804209  744   rx=2
    ## 22 0.50804209  769   rx=2
    ## 23 0.50804209  770   rx=2
    ## 24 0.50804209 1129   rx=2
    ## 25 0.50804209 1206   rx=2
    ## 26 0.50804209 1227   rx=2

Note that columns used in the
[`strata()`](https://rdrr.io/pkg/survival/man/strata.html) function will
not be estimated in the regular portion of the model (i.e., within the
linear predictor).

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
[case_weights](https://parsnip.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

The [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) arguments
have arguments called `case_weights` that expect vectors of case
weights.

### Prediction types

    parsnip:::get_from_env("proportional_hazards_predict") |>
      dplyr::filter(engine == "survival") |>
      dplyr::select(mode, type)

    ## # A tibble: 3 x 2
    ##   mode                type
    ##   <chr>               <chr>
    ## 1 censored regression time
    ## 2 censored regression survival
    ## 3 censored regression linear_pred

### References

- Andersen P, Gill R. 1982. Cox’s regression model for counting
  processes, a large sample study. *Annals of Statistics* 10, 1100-1120.

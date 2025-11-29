# Poisson regression via pscl

`pscl::hurdle()` uses maximum likelihood estimation to fit a model for
count data that has separate model terms for predicting the counts and
for predicting the probability of a zero count.

## Details

For this engine, there is a single mode: regression

### Tuning Parameters

This engine has no tuning parameters.

### Translation from parsnip to the underlying model call (regression)

The **poissonreg** extension package is required to fit this model.

    library(poissonreg)

    poisson_reg() |>
      set_engine("hurdle") |>
      translate()

    ## Poisson Regression Model Specification (regression)
    ##
    ## Computational engine: hurdle
    ##
    ## Model fit template:
    ## pscl::hurdle(formula = missing_arg(), data = missing_arg(), weights = missing_arg())

### Preprocessing and special formulas for zero-inflated Poisson models

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md), parsnip
will convert factor columns to indicators.

### Specifying the statistical model details

For this particular model, a special formula is used to specify which
columns affect the counts and which affect the model for the probability
of zero counts. These sets of terms are separated by a bar. For example,
`y ~ x | z`. This type of formula is not used by the base R
infrastructure
(e.g. [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html))

When fitting a parsnip model with this engine directly, the formula
method is required and the formula is just passed through. For example:

    library(tidymodels)
    tidymodels_prefer()

    data("bioChemists", package = "pscl")
    poisson_reg() |>
      set_engine("hurdle") |>
      fit(art ~ fem + mar | ment, data = bioChemists)

    ## parsnip model object
    ##
    ##
    ## Call:
    ## pscl::hurdle(formula = art ~ fem + mar | ment, data = data)
    ##
    ## Count model coefficients (truncated poisson with log link):
    ## (Intercept)     femWomen   marMarried
    ##    0.847598    -0.237351     0.008846
    ##
    ## Zero hurdle model coefficients (binomial with logit link):
    ## (Intercept)         ment
    ##     0.24871      0.08092

However, when using a workflow, the best approach is to avoid using
[`workflows::add_formula()`](https://workflows.tidymodels.org/reference/add_formula.html)
and use
[`workflows::add_variables()`](https://workflows.tidymodels.org/reference/add_variables.html)
in conjunction with a model formula:

    data("bioChemists", package = "pscl")
    spec <-
      poisson_reg() |>
      set_engine("hurdle")

    workflow() |>
      add_variables(outcomes = c(art), predictors = c(fem, mar, ment)) |>
      add_model(spec, formula = art ~ fem + mar | ment) |>
      fit(data = bioChemists) |>
      extract_fit_engine()

    ##
    ## Call:
    ## pscl::hurdle(formula = art ~ fem + mar | ment, data = data)
    ##
    ## Count model coefficients (truncated poisson with log link):
    ## (Intercept)     femWomen   marMarried
    ##    0.847598    -0.237351     0.008846
    ##
    ## Zero hurdle model coefficients (binomial with logit link):
    ## (Intercept)         ment
    ##     0.24871      0.08092

The reason for this is that
[`workflows::add_formula()`](https://workflows.tidymodels.org/reference/add_formula.html)
will try to create the model matrix and either fail or create dummy
variables prematurely.

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

    parsnip:::get_from_env("poisson_reg_predict") |>
      dplyr::filter(engine == "hurdle") |>
      dplyr::select(mode, type)

    ## # A tibble: 2 x 2
    ##   mode       type
    ##   <chr>      <chr>
    ## 1 regression numeric
    ## 2 regression raw

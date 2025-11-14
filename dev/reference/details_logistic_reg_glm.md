# Logistic regression via glm

[`stats::glm()`](https://rdrr.io/r/stats/glm.html) fits a generalized
linear model for binary outcomes. A linear combination of the predictors
is used to model the log odds of an event.

## Details

For this engine, there is a single mode: classification

### Tuning Parameters

This engine has no tuning parameters but you can set the `family`
parameter (and/or `link`) as an engine argument (see below).

### Translation from parsnip to the original package

    logistic_reg() |>
      set_engine("glm") |>
      translate()

    ## Logistic Regression Model Specification (classification)
    ##
    ## Computational engine: glm
    ##
    ## Model fit template:
    ## stats::glm(formula = missing_arg(), data = missing_arg(), weights = missing_arg(),
    ##     family = stats::binomial)

To use a non-default `family` and/or `link`, pass in as an argument to
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md):

    logistic_reg() |>
      set_engine("glm", family = stats::binomial(link = "probit")) |>
      translate()

    ## Logistic Regression Model Specification (classification)
    ##
    ## Engine-Specific Arguments:
    ##   family = stats::binomial(link = "probit")
    ##
    ## Computational engine: glm
    ##
    ## Model fit template:
    ## stats::glm(formula = missing_arg(), data = missing_arg(), weights = missing_arg(),
    ##     family = stats::binomial(link = "probit"))

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md), parsnip
will convert factor columns to indicators.

### Case weights

This model can utilize case weights during model fitting. To use them,
see the documentation in
[case_weights](https://parsnip.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

The [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) arguments
have arguments called `case_weights` that expect vectors of case
weights.

*However*, the documentation in
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) assumes that is
specific type of case weights are being used:“Non-NULL weights can be
used to indicate that different observations have different dispersions
(with the values in weights being inversely proportional to the
dispersions); or equivalently, when the elements of weights are positive
integers `w_i`, that each response `y_i` is the mean of `w_i`
unit-weight observations. For a binomial GLM prior weights are used to
give the number of trials when the response is the proportion of
successes: they would rarely be used for a Poisson GLM.”

### Saving fitted model objects

This model object contains data that are not required to make
predictions. When saving the model for the purpose of prediction, the
size of the saved object might be substantially reduced by using
functions from the [butcher](https://butcher.tidymodels.org) package.

### Examples

The “Fitting and Predicting with parsnip” article contains
[examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#logistic-reg-glm)
for
[`logistic_reg()`](https://parsnip.tidymodels.org/dev/reference/logistic_reg.md)
with the `"glm"` engine.

### References

- Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.

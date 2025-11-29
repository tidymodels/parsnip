# Logistic regression via mixed models

The `"glmer"` engine estimates fixed and random effect regression
parameters using maximum likelihood (or restricted maximum likelihood)
estimation.

## Details

For this engine, there is a single mode: classification

### Tuning Parameters

This model has no tuning parameters.

### Translation from parsnip to the original package

The **multilevelmod** extension package is required to fit this model.

    library(multilevelmod)

    logistic_reg() |>
      set_engine("glmer") |>
      translate()

    ## Logistic Regression Model Specification (classification)
    ##
    ## Computational engine: glmer
    ##
    ## Model fit template:
    ## lme4::glmer(formula = missing_arg(), data = missing_arg(), weights = missing_arg(),
    ##     family = binomial)

### Predicting new samples

This model can use subject-specific coefficient estimates to make
predictions (i.e. partial pooling). For example, this equation shows the
linear predictor (`\eta`) for a random intercept:

    \eta_{i} = (\beta_0 + b_{0i}) + \beta_1x_{i1}

where `i` denotes the `i`th independent experimental unit
(e.g. subject). When the model has seen subject `i`, it can use that
subject’s data to adjust the *population* intercept to be more specific
to that subjects results.

What happens when data are being predicted for a subject that was not
used in the model fit? In that case, this package uses *only* the
population parameter estimates for prediction:

    \hat{\eta}_{i'} = \hat{\beta}_0+ \hat{\beta}x_{i'1}

Depending on what covariates are in the model, this might have the
effect of making the same prediction for all new samples. The population
parameters are the “best estimate” for a subject that was not included
in the model fit.

The tidymodels framework deliberately constrains predictions for new
data to not use the training set or other data (to prevent information
leakage).

### Preprocessing requirements

There are no specific preprocessing needs. However, it is helpful to
keep the clustering/subject identifier column as factor or character
(instead of making them into dummy variables). See the examples in the
next section.

### Other details

The model can accept case weights.

With parsnip, we suggest using the formula method when fitting:

    library(tidymodels)
    data("toenail", package = "HSAUR3")

    logistic_reg() |>
      set_engine("glmer") |>
      fit(outcome ~ treatment * visit + (1 | patientID), data = toenail)

When using tidymodels infrastructure, it may be better to use a
workflow. In this case, you can add the appropriate columns using
`add_variables()` then supply the typical formula when adding the model:

    library(tidymodels)

    glmer_spec <-
      logistic_reg() |>
      set_engine("glmer")

    glmer_wflow <-
      workflow() |>
      # The data are included as-is using:
      add_variables(outcomes = outcome, predictors = c(treatment, visit, patientID)) |>
      add_model(glmer_spec, formula = outcome ~ treatment * visit + (1 | patientID))

    fit(glmer_wflow, data = toenail)

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

    parsnip:::get_from_env("logistic_reg_predict") |>
      dplyr::filter(engine == "glmer") |>
      dplyr::select(mode, type)

    ## # A tibble: 2 x 2
    ##   mode           type
    ##   <chr>          <chr>
    ## 1 classification class
    ## 2 classification prob

### References

- J Pinheiro, and D Bates. 2000. *Mixed-effects models in S and S-PLUS*.
  Springer, New York, NY

- West, K, Band Welch, and A Galecki. 2014. *Linear Mixed Models: A
  Practical Guide Using Statistical Software*. CRC Press.

- Thorson, J, Minto, C. 2015, Mixed effects: a unifying framework for
  statistical modelling in fisheries biology. *ICES Journal of Marine
  Science*, Volume 72, Issue 5, Pages 1245–1256.

- Harrison, XA, Donaldson, L, Correa-Cano, ME, Evans, J, Fisher, DN,
  Goodwin, CED, Robinson, BS, Hodgson, DJ, Inger, R. 2018. *A brief
  introduction to mixed effects modelling and multi-model inference in
  ecology*. PeerJ 6:e4794.

- DeBruine LM, Barr DJ. Understanding Mixed-Effects Models Through Data
  Simulation. 2021. *Advances in Methods and Practices in Psychological
  Science*.

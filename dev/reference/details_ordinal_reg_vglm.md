# Ordinal regression via vector GLMs

`VGAM::vglm()` fits vector generalized linear models, which specialize
to several families of ordinal regression models.

## Details

For this engine, there is a single mode: classification

### Tuning Parameters

This model has 2 tuning parameters:

- `ordinal_link`: Ordinal Link (type: character, default: logit)

- `odds_link`: Odds Link (type: character, default: cumulative)

### Translation from parsnip to the original package

The **ordered** extension package is required to fit this model.

    library(ordered)

    ordinal_reg(ordinal_link = character(0), odds_link = character(0)) %>%
      set_engine("vglm") %>%
      set_mode("classification") %>%
      translate()

    ## Ordinal Regression Model Specification (classification)
    ##
    ## Main Arguments:
    ##   ordinal_link = character(0)
    ##   odds_link = character(0)
    ##
    ## Computational engine: vglm
    ##
    ## Model fit template:
    ## ordered::VGAM_vglm_wrapper(formula = missing_arg(), data = missing_arg(),
    ##     weights = missing_arg(), link = character(0), family = character(0),
    ##     parallel = TRUE)

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

*However*, the documentation in `VGAM::vglm()` notes that matrix of case
weights can be passed so that different classes have different weights.
tidymodels assumes vector of a weights; a matrix cannot be passed in.

Also, the engine documentation notes that: “The values of weights must
be positive; try setting a very small value such as 1.0e-8 to
effectively delete an observation.”

### Prediction types

    parsnip:::get_from_env("ordinal_reg_predict") |>
      dplyr::filter(engine == "vglm") |>
      dplyr::select(mode, type)

    ## # A tibble: 2 x 2
    ##   mode           type
    ##   <chr>          <chr>
    ## 1 classification class
    ## 2 classification prob

### References

- Yee T. 2010. The VGAM Package for Categorical Data Analysis. *Journal
  of Statistical Software* 32(10):1–34. .

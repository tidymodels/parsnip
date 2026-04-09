# Ordinal regression via polr

[`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) fits a
cumulative-link ordinal regression model.

## Details

For this engine, there is a single mode: classification

### Tuning Parameters

This model has no tuning parameters.

### Translation from parsnip to the original package

The **ordered** extension package is required to fit this model.

    library(ordered)

    ordinal_reg() %>%
      set_engine("polr") %>%
      set_mode("classification") %>%
      translate()

    ## Ordinal Regression Model Specification (classification)
    ##
    ## Computational engine: polr
    ##
    ## Model fit template:
    ## MASS::polr(formula = missing_arg(), data = missing_arg(), weights = missing_arg())

### References

- Bürkner P-C, Vuorre M. 2019. Ordinal Regression Models in Psychology:
  A Tutorial. *Advances in Methods and Practices in Psychological
  Science* 2(1):77-101. .

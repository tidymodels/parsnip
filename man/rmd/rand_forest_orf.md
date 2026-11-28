


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 6 tuning parameters:

- `mtry`: # Randomly Selected Predictors (type: integer, default: see below)

- `trees`: # Trees (type: integer, default: see below)

- `min_n`: Minimal Node Size (type: integer, default: see below)

- `sample.fraction`: Subsampling Fraction (type: double, default: see below)

- `honesty`: Honest Splitting? (type: logical, default: TRUE)

- `honesty.fraction`: Honest Sample Fraction (type: double, default: see below)

`mtry` and `min_n` depend on the data dimensions. `sample.fraction`,
`honesty`, and `honesty.fraction` are engine-specific arguments that can be
passed via [set_engine()] and are registered as tunable parameters by
`ordered`.

## Translation from parsnip to the original package

The **ordered** extension package is required to fit this model.


``` r
library(ordered)

rand_forest(
  mtry = integer(1),
  trees = integer(1),
  min_n = integer(1)
) |>
  set_engine(
    "orf",
    sample.fraction = 0.7,
    honesty = TRUE,
    honesty.fraction = 0.4
  ) |>
  set_mode("classification") |>
  translate()
```

```
## Random Forest Model Specification (classification)
## 
## Main Arguments:
##   mtry = integer(1)
##   trees = integer(1)
##   min_n = integer(1)
## 
## Engine-Specific Arguments:
##   sample.fraction = 0.7
##   honesty = TRUE
##   honesty.fraction = 0.4
## 
## Computational engine: orf 
## 
## Model fit template:
## ordered::orf_wrapper(X = missing_arg(), Y = missing_arg(), mtry = min_cols(~integer(1), 
##     x), num.trees = integer(1), min.node.size = min_rows(~integer(1), 
##     x), sample.fraction = 0.7, honesty = TRUE, honesty.fraction = 0.4)
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Case weights


The underlying model implementation does not allow for case weights. 

## Prediction types


``` r
parsnip:::get_from_env("rand_forest_predict") |>
  dplyr::filter(engine == "orf") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 2 x 2
##   mode           type 
##   <chr>          <chr>
## 1 classification class
## 2 classification prob
```

## References

- Lechner M, Okasa G. 2025. Random Forest estimation of the ordered choice
  model. _Empirical Economics_ 68(1):1-106.
  \doi{10.1007/s00181-024-02646-4}.

- Okasa G, Lechner M. 2022. _orf: Ordered Random Forests_. R package version
  0.1.4.

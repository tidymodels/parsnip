


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 2 tuning parameters:

- `predictor_prop`: Proportion of Predictors (type: double, default: see below)

- `num_comp`: # Components (type: integer, default: 2L)


## Translation from parsnip to the underlying model call  (regression)

The **plsmod** extension package is required to fit this model.


``` r
library(plsmod)

pls(num_comp = integer(1), predictor_prop = double(1)) |>
  set_engine("mixOmics") |>
  set_mode("regression") |>
  translate()
```

```
## PLS Model Specification (regression)
## 
## Main Arguments:
##   predictor_prop = double(1)
##   num_comp = integer(1)
## 
## Computational engine: mixOmics 
## 
## Model fit template:
## plsmod::pls_fit(x = missing_arg(), y = missing_arg(), predictor_prop = double(1), 
##     ncomp = integer(1))
```

[plsmod::pls_fit()] is a function that: 

 - Determines the number of predictors in the data.
 - Adjusts `num_comp` if the value is larger than the number of factors.
 - Determines whether sparsity is required based on the value of `predictor_prop`.
 - Sets the `keepX` argument of `mixOmics::spls()` for sparse models. 

## Translation from parsnip to the underlying model call  (classification)

The **plsmod** extension package is required to fit this model.


``` r
library(plsmod)

pls(num_comp = integer(1), predictor_prop = double(1)) |>
  set_engine("mixOmics") |>
  set_mode("classification") |>
  translate()
```

```
## PLS Model Specification (classification)
## 
## Main Arguments:
##   predictor_prop = double(1)
##   num_comp = integer(1)
## 
## Computational engine: mixOmics 
## 
## Model fit template:
## plsmod::pls_fit(x = missing_arg(), y = missing_arg(), predictor_prop = double(1), 
##     ncomp = integer(1))
```

In this case, [plsmod::pls_fit()] has the same role as above but eventually targets `mixOmics::plsda()` or `mixOmics::splsda()`.

## Installing mixOmics

This package is available via the Bioconductor repository and is not accessible via CRAN. You can install using: 


``` r
  if (!require("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  
  remotes::install_bioc("mixOmics")
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Variance calculations are used in these computations so _zero-variance_ predictors (i.e., with a single unique value) should be eliminated before fitting the model. 




Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Case weights


The underlying model implementation does not allow for case weights. 

## References

 - Rohart F and Gautier B and Singh A and Le Cao K-A  (2017). "mixOmics: An R package for 'omics feature selection and multiple data integration." PLoS computational biology, 13(11), e1005752.
 

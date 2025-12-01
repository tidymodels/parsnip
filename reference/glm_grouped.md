# Fit a grouped binomial outcome from a data set with case weights

[`stats::glm()`](https://rdrr.io/r/stats/glm.html) assumes that a
tabular data set with case weights corresponds to "different
observations have different dispersions" (see
[`?glm`](https://rdrr.io/r/stats/glm.html)).

In some cases, the case weights reflect that the same covariate pattern
was observed multiple times (i.e., *frequency weights*). In this case,
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) expects the data to
be formatted as the number of events for each factor level so that the
outcome can be given to the formula as `cbind(events_1, events_2)`.

`glm_grouped()` converts data with integer case weights to the expected
"number of events" format for binomial data.

## Usage

``` r
glm_grouped(formula, data, weights, ...)
```

## Arguments

- formula:

  A formula object with one outcome that is a two-level factors.

- data:

  A data frame with the outcomes and predictors (but not case weights).

- weights:

  An integer vector of weights whose length is the same as the number of
  rows in `data`. If it is a non-integer numeric, it will be converted
  to integer (with a warning).

- ...:

  Options to pass to [`stats::glm()`](https://rdrr.io/r/stats/glm.html).
  If `family` is not set, it will automatically be assigned the basic
  binomial family.

## Value

A object produced by [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

## Examples

``` r
#----------------------------------------------------------------------------
# The same data set formatted three ways

# First with basic case weights that, from ?glm, are used inappropriately.
ucb_weighted <- as.data.frame(UCBAdmissions)
ucb_weighted$Freq <- as.integer(ucb_weighted$Freq)
head(ucb_weighted)
#>      Admit Gender Dept Freq
#> 1 Admitted   Male    A  512
#> 2 Rejected   Male    A  313
#> 3 Admitted Female    A   89
#> 4 Rejected Female    A   19
#> 5 Admitted   Male    B  353
#> 6 Rejected   Male    B  207
nrow(ucb_weighted)
#> [1] 24

# Format when yes/no data are in individual rows (probably still inappropriate)
library(tidyr)
ucb_long <- uncount(ucb_weighted, Freq)
head(ucb_long)
#>      Admit Gender Dept
#> 1 Admitted   Male    A
#> 2 Admitted   Male    A
#> 3 Admitted   Male    A
#> 4 Admitted   Male    A
#> 5 Admitted   Male    A
#> 6 Admitted   Male    A
nrow(ucb_long)
#> [1] 4526

# Format where the outcome is formatted as number of events
ucb_events <-
  ucb_weighted |>
  tidyr::pivot_wider(
    id_cols = c(Gender, Dept),
    names_from = Admit,
    values_from = Freq,
    values_fill = 0L
  )
head(ucb_events)
#> # A tibble: 6 × 4
#>   Gender Dept  Admitted Rejected
#>   <fct>  <fct>    <int>    <int>
#> 1 Male   A          512      313
#> 2 Female A           89       19
#> 3 Male   B          353      207
#> 4 Female B           17        8
#> 5 Male   C          120      205
#> 6 Female C          202      391
nrow(ucb_events)
#> [1] 12

#----------------------------------------------------------------------------
# Different model fits

# Treat data as separate Bernoulli data:
glm(Admit ~ Gender + Dept, data = ucb_long, family = binomial)
#> 
#> Call:  glm(formula = Admit ~ Gender + Dept, family = binomial, data = ucb_long)
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale         DeptB         DeptC         DeptD  
#>     -0.58205      -0.09987       0.04340       1.26260       1.29461  
#>        DeptE         DeptF  
#>      1.73931       3.30648  
#> 
#> Degrees of Freedom: 4525 Total (i.e. Null);  4519 Residual
#> Null Deviance:       6044 
#> Residual Deviance: 5187  AIC: 5201

# Weights produce the same statistics
glm(
  Admit ~ Gender + Dept,
  data = ucb_weighted,
  family = binomial,
  weights = ucb_weighted$Freq
)
#> 
#> Call:  glm(formula = Admit ~ Gender + Dept, family = binomial, data = ucb_weighted, 
#>     weights = ucb_weighted$Freq)
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale         DeptB         DeptC         DeptD  
#>     -0.58205      -0.09987       0.04340       1.26260       1.29461  
#>        DeptE         DeptF  
#>      1.73931       3.30648  
#> 
#> Degrees of Freedom: 23 Total (i.e. Null);  17 Residual
#> Null Deviance:       6044 
#> Residual Deviance: 5187  AIC: 5201

# Data as binomial "x events out of n trials" format. Note that, to get the same
# coefficients, the order of the levels must be reversed.
glm(
  cbind(Rejected, Admitted) ~ Gender + Dept,
  data = ucb_events,
  family = binomial
)
#> 
#> Call:  glm(formula = cbind(Rejected, Admitted) ~ Gender + Dept, family = binomial, 
#>     data = ucb_events)
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale         DeptB         DeptC         DeptD  
#>     -0.58205      -0.09987       0.04340       1.26260       1.29461  
#>        DeptE         DeptF  
#>      1.73931       3.30648  
#> 
#> Degrees of Freedom: 11 Total (i.e. Null);  5 Residual
#> Null Deviance:       877.1 
#> Residual Deviance: 20.2  AIC: 103.1

# The new function that starts with frequency weights and gets the correct place:
glm_grouped(Admit ~ Gender + Dept, data = ucb_weighted, weights = ucb_weighted$Freq)
#> 
#> Call:  glm(formula = formula, family = "binomial", data = data)
#> 
#> Coefficients:
#>  (Intercept)  GenderFemale         DeptB         DeptC         DeptD  
#>     -0.58205      -0.09987       0.04340       1.26260       1.29461  
#>        DeptE         DeptF  
#>      1.73931       3.30648  
#> 
#> Degrees of Freedom: 11 Total (i.e. Null);  5 Residual
#> Null Deviance:       877.1 
#> Residual Deviance: 20.2  AIC: 103.1
```

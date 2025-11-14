# Extract elements of a parsnip model object

These functions extract various elements from a parsnip object. If they
do not exist yet, an error is thrown.

- [`extract_spec_parsnip()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the parsnip [model
  specification](https://parsnip.tidymodels.org/dev/reference/model_spec.md).

- [`extract_fit_engine()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the engine specific fit embedded within a parsnip model fit.
  For example, when using
  [`linear_reg()`](https://parsnip.tidymodels.org/dev/reference/linear_reg.md)
  with the `"lm"` engine, this returns the underlying `lm` object.

- [`extract_parameter_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns a single dials parameter object.

- [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns a set of dials parameter objects.

- [`extract_fit_time()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns a tibble with fit times. The fit times correspond to the time
  for the parsnip engine to fit and do not include other portions of the
  elapsed time in
  [`fit.model_spec()`](https://parsnip.tidymodels.org/dev/reference/fit.md).

## Usage

``` r
# S3 method for class 'model_fit'
extract_spec_parsnip(x, ...)

# S3 method for class 'model_fit'
extract_fit_engine(x, ...)

# S3 method for class 'model_spec'
extract_parameter_set_dials(x, ...)

# S3 method for class 'model_spec'
extract_parameter_dials(x, parameter, ...)

# S3 method for class 'model_fit'
extract_fit_time(x, summarize = TRUE, ...)
```

## Arguments

- x:

  A parsnip `model_fit` object or a parsnip `model_spec` object.

- ...:

  Not currently used.

- parameter:

  A single string for the parameter ID.

- summarize:

  A logical for whether the elapsed fit time should be returned as a
  single row or multiple rows. Doesn't support `FALSE` for parsnip
  models.

## Value

The extracted value from the parsnip object, `x`, as described in the
description section.

## Details

Extracting the underlying engine fit can be helpful for describing the
model (via [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html), etc.) or for
variable importance/explainers.

However, users should not invoke the
[`predict()`](https://rdrr.io/r/stats/predict.html) method on an
extracted model. There may be preprocessing operations that parsnip has
executed on the data prior to giving it to the model. Bypassing these
can lead to errors or silently generating incorrect predictions.

**Good**:

       parsnip_fit |> predict(new_data)

**Bad**:

       parsnip_fit |> extract_fit_engine() |> predict(new_data)

## Examples

``` r
lm_spec <- linear_reg() |> set_engine("lm")
lm_fit <- fit(lm_spec, mpg ~ ., data = mtcars)

lm_spec
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 
extract_spec_parsnip(lm_fit)
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 
#> Model fit template:
#> stats::lm(formula = missing_arg(), data = missing_arg(), weights = missing_arg())

extract_fit_engine(lm_fit)
#> 
#> Call:
#> stats::lm(formula = mpg ~ ., data = data)
#> 
#> Coefficients:
#> (Intercept)          cyl         disp           hp         drat  
#>    12.30337     -0.11144      0.01334     -0.02148      0.78711  
#>          wt         qsec           vs           am         gear  
#>    -3.71530      0.82104      0.31776      2.52023      0.65541  
#>        carb  
#>    -0.19942  
#> 
lm(mpg ~ ., data = mtcars)
#> 
#> Call:
#> lm(formula = mpg ~ ., data = mtcars)
#> 
#> Coefficients:
#> (Intercept)          cyl         disp           hp         drat  
#>    12.30337     -0.11144      0.01334     -0.02148      0.78711  
#>          wt         qsec           vs           am         gear  
#>    -3.71530      0.82104      0.31776      2.52023      0.65541  
#>        carb  
#>    -0.19942  
#> 
```

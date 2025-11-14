# Model Fit Objects

Model fits are trained [model
specifications](https://parsnip.tidymodels.org/dev/reference/model_spec.md)
that are ready to
[predict](https://parsnip.tidymodels.org/dev/reference/predict.model_fit.md)
on new data. Model fits have class `model_fit` and, usually, a subclass
referring to the engine used to fit the model.

## Details

An object with class `"model_fit"` is a container for information about
a model that has been fit to the data.

The main elements of the object are:

- `lvl`: A vector of factor levels when the outcome is a factor. This is
  `NULL` when the outcome is not a factor vector.

- `spec`: A `model_spec` object.

- `fit`: The object produced by the fitting function.

- `preproc`: This contains any data-specific information required to
  process new a sample point for prediction. For example, if the
  underlying model function requires arguments `x` and `y` and the user
  passed a formula to `fit`, the `preproc` object would contain items
  such as the terms object and so on. When no information is required,
  this is `NA`.

As discussed in the documentation for
[`model_spec`](https://parsnip.tidymodels.org/dev/reference/model_spec.md),
the original arguments to the specification are saved as quosures. These
are evaluated for the `model_fit` object prior to fitting. If the
resulting model object prints its call, any user-defined options are
shown in the call preceded by a tilde (see the example below). This is a
result of the use of quosures in the specification.

This class and structure is the basis for how parsnip stores model
objects after seeing the data and applying a model.

## Examples

``` r
# Keep the `x` matrix if the data are not too big.
spec_obj <-
  linear_reg() |>
  set_engine("lm", x = ifelse(.obs() < 500, TRUE, FALSE))
spec_obj
#> Linear Regression Model Specification (regression)
#> 
#> Engine-Specific Arguments:
#>   x = ifelse(.obs() < 500, TRUE, FALSE)
#> 
#> Computational engine: lm 
#> 

fit_obj <- fit(spec_obj, mpg ~ ., data = mtcars)
fit_obj
#> parsnip model object
#> 
#> 
#> Call:
#> stats::lm(formula = mpg ~ ., data = data, x = ~ifelse(.obs() < 
#>     500, TRUE, FALSE))
#> 
#> Coefficients:
#> (Intercept)          cyl         disp           hp         drat  
#>    12.30337     -0.11144      0.01334     -0.02148      0.78711  
#>          wt         qsec           vs           am         gear  
#>    -3.71530      0.82104      0.31776      2.52023      0.65541  
#>        carb  
#>    -0.19942  
#> 

nrow(fit_obj$fit$x)
#> [1] 32
```

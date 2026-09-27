# Polynomial support vector machines

`svm_poly()` defines a support vector machine model. For classification,
the model tries to maximize the width of the margin between classes
using a polynomial class boundary. For regression, the model optimizes a
robust loss function that is only affected by very large model residuals
and uses polynomial functions of the predictors. This function can fit
classification and regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`kernlab`](https://parsnip.tidymodels.org/reference/details_svm_poly_kernlab.md)`¹`

¹ The default engine.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
svm_poly(
  mode = "unknown",
  engine = "kernlab",
  cost = NULL,
  degree = NULL,
  scale_factor = NULL,
  margin = NULL
)
```

## Arguments

- mode:

  A single character string for the prediction outcome mode. Possible
  values for this model are "unknown", "regression", or
  "classification".

- engine:

  A single character string specifying what computational engine to use
  for fitting.

- cost:

  A positive number for the cost of predicting a sample within or on the
  wrong side of the margin

- degree:

  A positive number for polynomial degree.

- scale_factor:

  A positive number for the polynomial scaling factor.

- margin:

  A positive number for the epsilon in the SVM insensitive loss function
  (regression only)

## Details

This function only defines what *type* of model is being fit. Once an
engine is specified, the *method* to fit the model is also defined. See
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md)
for more on setting the engine, including how to set engine arguments.

The model is not trained or fit until the
[`fit()`](https://parsnip.tidymodels.org/reference/fit.md) function is
used with the data.

Each of the arguments in this function other than `mode` and `engine`
are captured as
[quosures](https://rlang.r-lib.org/reference/topic-quosure.html). To
pass values programmatically, use the [injection
operator](https://rlang.r-lib.org/reference/injection-operator.html)
like so:

    value <- 1
    svm_poly(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`kernlab engine details`](https://parsnip.tidymodels.org/reference/details_svm_poly_kernlab.md)

## Examples

``` r
show_engines("svm_poly")
#> # A tibble: 2 × 2
#>   engine  mode          
#>   <chr>   <chr>         
#> 1 kernlab classification
#> 2 kernlab regression    

svm_poly(mode = "classification", degree = 1.2)
#> Polynomial Support Vector Machine Model Specification (classification)
#> 
#> Main Arguments:
#>   degree = 1.2
#> 
#> Computational engine: kernlab 
#> 
```

# Linear support vector machines

`svm_linear()` defines a support vector machine model. For
classification, the model tries to maximize the width of the margin
between classes (using a linear class boundary). For regression, the
model optimizes a robust loss function that is only affected by very
large model residuals and uses a linear fit. This function can fit
classification and regression models.

`Rd parsnip:::make_engine_list("svm_linear")`

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
svm_linear(mode = "unknown", engine = "LiblineaR", cost = NULL, margin = NULL)
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
    svm_linear(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

`Rd parsnip:::make_seealso_list("svm_linear")`

## Examples

``` r
show_engines("svm_linear")
#> # A tibble: 4 × 2
#>   engine    mode          
#>   <chr>     <chr>         
#> 1 LiblineaR classification
#> 2 LiblineaR regression    
#> 3 kernlab   classification
#> 4 kernlab   regression    

svm_linear(mode = "classification")
#> Linear Support Vector Machine Model Specification (classification)
#> 
#> Computational engine: LiblineaR 
#> 
```

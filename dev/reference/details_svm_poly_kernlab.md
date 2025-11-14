# Polynomial support vector machines (SVMs) via kernlab

[`kernlab::ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html) fits a
support vector machine model. For classification, the model tries to
maximize the width of the margin between classes. For regression, the
model optimizes a robust loss function that is only affected by very
large model residuals.

## Details

For this engine, there are multiple modes: classification and regression

### Tuning Parameters

This model has 4 tuning parameters:

- `cost`: Cost (type: double, default: 1.0)

- `degree`: Degree of Interaction (type: integer, default: 1L1)

- `scale_factor`: Scale Factor (type: double, default: 1.0)

- `margin`: Insensitivity Margin (type: double, default: 0.1)

Parsnip changes the default range for `cost` to `c(-10, 5)`.

### Translation from parsnip to the original package (regression)

    svm_poly(
      cost = double(1),
      degree = integer(1),
      scale_factor = double(1),
      margin = double(1)
    ) |>
      set_engine("kernlab") |>
      set_mode("regression") |>
      translate()

    ## Polynomial Support Vector Machine Model Specification (regression)
    ##
    ## Main Arguments:
    ##   cost = double(1)
    ##   degree = integer(1)
    ##   scale_factor = double(1)
    ##   margin = double(1)
    ##
    ## Computational engine: kernlab
    ##
    ## Model fit template:
    ## kernlab::ksvm(x = missing_arg(), data = missing_arg(), C = double(1),
    ##     epsilon = double(1), kernel = "polydot", kpar = list(degree = ~integer(1),
    ##         scale = ~double(1)))

### Translation from parsnip to the original package (classification)

    svm_poly(
      cost = double(1),
      degree = integer(1),
      scale_factor = double(1)
    ) |>
      set_engine("kernlab") |>
      set_mode("classification") |>
      translate()

    ## Polynomial Support Vector Machine Model Specification (classification)
    ##
    ## Main Arguments:
    ##   cost = double(1)
    ##   degree = integer(1)
    ##   scale_factor = double(1)
    ##
    ## Computational engine: kernlab
    ##
    ## Model fit template:
    ## kernlab::ksvm(x = missing_arg(), data = missing_arg(), C = double(1),
    ##     kernel = "polydot", prob.model = TRUE, kpar = list(degree = ~integer(1),
    ##         scale = ~double(1)))

The `margin` parameter does not apply to classification models.

Note that the `"kernlab"` engine does not naturally estimate class
probabilities. To produce them, the decision values of the model are
converted to probabilities using Platt scaling. This method fits an
additional model on top of the SVM model. When fitting the Platt scaling
model, random numbers are used that are not reproducible or controlled
by R’s random number stream.

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md), parsnip
will convert factor columns to indicators.

Predictors should have the same scale. One way to achieve this is to
center and scale each so that each predictor has mean zero and a
variance of one.

### Case weights

The underlying model implementation does not allow for case weights.

### Examples

The “Fitting and Predicting with parsnip” article contains
[examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#svm-poly-kernlab)
for
[`svm_poly()`](https://parsnip.tidymodels.org/dev/reference/svm_poly.md)
with the `"kernlab"` engine.

### Saving fitted model objects

This model object contains data that are not required to make
predictions. When saving the model for the purpose of prediction, the
size of the saved object might be substantially reduced by using
functions from the [butcher](https://butcher.tidymodels.org) package.

### References

- Lin, HT, and R Weng. [“A Note on Platt’s Probabilistic Outputs for
  Support Vector
  Machines”](https://www.csie.ntu.edu.tw/~cjlin/papers/plattprob.pdf)

- Karatzoglou, A, Smola, A, Hornik, K, and A Zeileis. 2004. [“kernlab -
  An S4 Package for Kernel Methods in
  R.”](https://www.jstatsoft.org/article/view/v011i09), *Journal of
  Statistical Software*.

- Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.

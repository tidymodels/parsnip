# Create a ggplot for a model object

This method provides a good visualization method for model results.
Currently, only methods for glmnet models are implemented.

## Usage

``` r
# S3 method for class 'model_fit'
autoplot(object, ...)

# S3 method for class 'glmnet'
autoplot(object, ..., min_penalty = 0, best_penalty = NULL, top_n = 3L)
```

## Arguments

- object:

  A model fit object.

- ...:

  For `autoplot.glmnet()`, options to pass to
  [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).
  Otherwise, this argument is ignored.

- min_penalty:

  A single, non-negative number for the smallest penalty value that
  should be shown in the plot. If left `NULL`, the whole data range is
  used.

- best_penalty:

  A single, non-negative number that will show a vertical line marker.
  If left `NULL`, no line is shown. When this argument is used, the
  ggrepl package is required.

- top_n:

  A non-negative integer for how many model predictors to label. The top
  predictors are ranked by their absolute coefficient value. For
  multinomial or multivariate models, the `top_n` terms are selected
  within class or response, respectively.

## Value

A ggplot object with penalty on the x-axis and coefficients on the
y-axis. For multinomial or multivariate models, the plot is faceted.

## Details

The glmnet package will need to be attached or loaded for its
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
method to work correctly.

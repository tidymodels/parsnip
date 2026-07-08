# Residual Neural Network for Tabular Data

`tabular_resnet()` ... This function can fit classification and
regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`brulee`](https://parsnip.tidymodels.org/dev/reference/details_tabular_resnet_brulee.md)`¹²`

¹ The default engine. ² Requires a parsnip extension package for
classification and regression.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
tabular_resnet(
  mode = "unknown",
  engine = "brulee",
  hidden_units = NULL,
  bottleneck_units = NULL,
  residual_at = NULL,
  penalty = NULL,
  mixture = NULL,
  dropout = NULL,
  epochs = NULL,
  activation = NULL,
  learn_rate = NULL,
  rate_schedule = NULL,
  momentum = NULL,
  batch_size = NULL,
  class_weights = NULL,
  stop_iter = NULL
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

- hidden_units:

  An integer vector for the number of units in the hidden model.

- bottleneck_units:

  The number of embeddings that are produced by batch normalization.

- residual_at:

  An integer vector with the layer number should use a residual
  connection (i.e., skip layer).

- penalty:

  A non-negative numeric value for the amount of weight decay.

- mixture:

  A number between zero and one (inclusive) giving the proportion of L1
  regularization (i.e. lasso) in the model. `mixture = 1` is a pure
  lasso model while `mixture = 0` indicates ridge regression (a.k.a
  weight decay).

- dropout:

  A number between 0 (inclusive) and 1 denoting the proportion of model
  parameters randomly set to zero during model training.

- epochs:

  An integer for the number of training iterations.

- activation:

  A vector character strings denoting the type of relationship between
  the layers. The activation function between the hidden and output
  layers is automatically set to either "linear" or "softmax" depending
  on the type of outcome. Possible values depend on the engine being
  used.

- learn_rate:

  A number for the rate at which the boosting algorithm adapts from
  iteration-to-iteration (specific engines only). This is sometimes
  referred to as the shrinkage parameter.

- rate_schedule:

  A character string for the learning rate schedule.

- momentum:

  A number for the momentum parameter in optimizers that use it.

- batch_size:

  An integer for the number of training instances in each batch.

- class_weights:

  Numeric class weights for imbalanced data (classification only).

- stop_iter:

  The number of iterations without improvement before stopping (specific
  engines only).

## See also

[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`brulee engine details`](https://parsnip.tidymodels.org/dev/reference/details_tabular_resnet_brulee.md)

## Examples

``` r
show_engines("tabular_resnet")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: engine <chr>, mode <chr>

tabular_resnet(mode = "classification", penalty = 0.01)
#> ! parsnip could not locate an implementation for `tabular_resnet`
#>   classification model specifications.
#> ℹ The parsnip extension package tabby implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> tabular resnet Model Specification (classification)
#> 
#> Main Arguments:
#>   penalty = 0.01
#> 
#> Computational engine: brulee 
#> 
```

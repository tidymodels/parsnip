# AutoInt: Automatic Feature Interaction Learning

`tabular_auto_int()` uses an attention mechanism to automatically learn
embedding co-representations for tabular data. This function can fit
classification and regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`brulee`](https://parsnip.tidymodels.org/dev/reference/details_tabular_auto_int_brulee.md)`¹²`

¹ The default engine. ² Requires a parsnip extension package for
classification and regression.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
tabular_auto_int(
  mode = "unknown",
  engine = "brulee",
  epochs = NULL,
  num_embedding = NULL,
  hidden_units = NULL,
  hidden_activations = NULL,
  num_attn_feat = NULL,
  num_attn_heads = NULL,
  num_attn_blocks = NULL,
  activation = NULL,
  dropout = NULL,
  dropout_attn = NULL,
  dropout_embedding = NULL,
  penalty = NULL,
  mixture = NULL,
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

- epochs:

  An integer for the number of training iterations.

- num_embedding:

  An integer for the dimensionality of the embedding space for features.

- hidden_units:

  An integer vector for the number of units in the hidden layers after
  the attention mechanism.

- hidden_activations:

  A character vector denoting the activation functions for the hidden
  layers.

- num_attn_feat:

  An integer for the number of attention features.

- num_attn_heads:

  An integer for the number of attention heads in the multi-head
  attention mechanism.

- num_attn_blocks:

  An integer for the number of sequential attention blocks.

- activation:

  A character vector denoting the type of relationship between the
  different layers. The activation function between the hidden and
  output layers is automatically set to either "linear" or "softmax"
  depending on the type of outcome. Possible values depend on the engine
  being used.

- dropout:

  A number between 0 (inclusive) and 1 denoting the proportion of model
  parameters randomly set to zero during model training.

- dropout_attn:

  A number between 0 (inclusive) and 1 denoting the proportion of
  attention weights set to zero during model training.

- dropout_embedding:

  A number between 0 (inclusive) and 1 denoting the proportion of
  embedding values set to zero during model training.

- penalty:

  A non-negative numeric value for the amount of weight decay.

- mixture:

  A number between zero and one (inclusive) denoting the proportion of
  L1 regularization (i.e. lasso) in the model.

  - `mixture = 1` specifies a pure lasso model,

  - `mixture = 0` specifies a ridge regression model, and

  - `0 < mixture < 1` specifies an elastic net model, interpolating
    lasso and ridge.

  Available for specific engines only.

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
[`brulee engine details`](https://parsnip.tidymodels.org/dev/reference/details_tabular_auto_int_brulee.md)

## Examples

``` r
show_engines("tabular_auto_int")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: engine <chr>, mode <chr>

tabular_auto_int(mode = "classification", num_attn_blocks = 4)
#> ! parsnip could not locate an implementation for `tabular_auto_int`
#>   classification model specifications.
#> ℹ The parsnip extension package tabby implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> tabular auto int Model Specification (classification)
#> 
#> Main Arguments:
#>   num_attn_blocks = 4
#> 
#> Computational engine: brulee 
#> 
```

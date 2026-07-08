# SAINT: Self-Attention and Inter-sample Attention Transformer

`tabular_saint()` uses self-attention and inter-sample attention
mechanisms to learn feature interactions for tabular data. This function
can fit classification and regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`brulee`](https://parsnip.tidymodels.org/dev/reference/details_tabular_saint_brulee.md)`¹²`

¹ The default engine. ² Requires a parsnip extension package for
classification and regression.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
tabular_saint(
  mode = "unknown",
  engine = "brulee",
  epochs = NULL,
  num_embedding = NULL,
  attention_type = NULL,
  num_attn_heads = NULL,
  num_attn_blocks = NULL,
  dropout_attn = NULL,
  dropout_hidden = NULL,
  dropout_last = NULL,
  hidden_units = NULL,
  hidden_activations = NULL,
  target_token = NULL,
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

- attention_type:

  A character string for the type of attention to use. Options are
  `"column"` (SAINT-s), `"row"` (SAINT-i), or `"both"` (full SAINT).

- num_attn_heads:

  An integer for the number of attention heads in the multi-head
  attention mechanism.

- num_attn_blocks:

  An integer for the number of sequential attention blocks.

- dropout_attn:

  A number between 0 (inclusive) and 1 denoting the proportion of
  attention weights set to zero during model training.

- dropout_hidden:

  A number between 0 (inclusive) and 1 denoting the proportion of values
  in the feed-forward layers set to zero during training.

- dropout_last:

  A number between 0 (inclusive) and 1 denoting the proportion of values
  set to zero between the last hidden layer and the output head.

- hidden_units:

  An integer vector for the number of units in the hidden layers after
  the attention mechanism.

- hidden_activations:

  A character vector denoting the activation functions for the hidden
  layers.

- target_token:

  A logical for whether to use a learnable target token (CLS-like
  embedding) to aggregate information for prediction. When `TRUE`, the
  model appends a special target token to the input that attends to all
  features via the attention mechanism.

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

## Details

### Row attention at prediction time

When `attention_type` is `"row"` or `"both"`, SAINT applies inter-sample
(row) attention across the samples in a batch. The brulee engine keeps
this on at prediction time by default, so the prediction for a given row
depends on which other rows are passed to
[`predict()`](https://rdrr.io/r/stats/predict.html) in the same call. To
obtain batch-independent predictions (where a row's prediction does not
change with its neighbors), bypass row attention at predict time with
`set_engine("brulee", row_attention_on_predict = FALSE)`.

## References

Somepalli, G., Goldblum, M., Schwarzschild, A., Bruss, C. B., &
Goldstein, T. (2021). SAINT: Improved Neural Networks for Tabular Data
via Row Attention and Contrastive Pre-Training. arXiv:2106.01342.

## See also

[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`brulee engine details`](https://parsnip.tidymodels.org/dev/reference/details_tabular_saint_brulee.md)

## Examples

``` r
show_engines("tabular_saint")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: engine <chr>, mode <chr>

tabular_saint(mode = "classification", num_attn_blocks = 4)
#> ! parsnip could not locate an implementation for `tabular_saint`
#>   classification model specifications.
#> ℹ The parsnip extension package tabby implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> tabular saint Model Specification (classification)
#> 
#> Main Arguments:
#>   num_attn_blocks = 4
#> 
#> Computational engine: brulee 
#> 
```

# SAINT via brulee

`brulee::brulee_saint()` fits a neural network with self-attention and
inter-sample attention.

## Details

For this engine, there are multiple modes: classification and regression

### Tuning Parameters

This model has 19 tuning parameters:

- `epochs`: \# Epochs (type: integer, default: 100L)

- `num_embedding`: \# Embedding Dimensions (type: integer, default: 32L)

- `attention_type`: Attention Type (type: character, default: ‘both’)

- `num_attn_heads`: \# Attention Heads (type: integer, default: 8L)

- `num_attn_blocks`: \# Attention Blocks (type: integer, default: 6L)

- `dropout_attn`: Attention Dropout Rate (type: double, default: 0.1)

- `dropout_hidden`: Hidden Dropout Rate (type: double, default: 0.1)

- `dropout_last`: Final Layer Dropout Rate (type: double, default: 0.0)

- `hidden_units`: \# Hidden Units (type: integer, default: 5L)

- `hidden_activations`: Activation Function (type: character, default:
  ‘relu’)

- `target_token`: Use Target Token? (type: logical, default: TRUE)

- `penalty`: Amount of Regularization (type: double, default: 0.001)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 0.0)

- `learn_rate`: Learning Rate (type: double, default: 0.0001)

- `rate_schedule`: Learning Rate Scheduler (type: character, default:
  ‘none’)

- `momentum`: Gradient Descent Momentum (type: double, default: 0.0)

- `batch_size`: Batch Size (type: integer, default: NULL)

- `class_weights`: Minority Class Weight (type: double, default: NULL)

- `stop_iter`: \# Iterations Before Stopping (type: integer, default:
  5L)

The use of the L1 penalty (a.k.a. the lasso penalty) does *not* force
parameters to be strictly zero (as it does in packages such as glmnet).
The zeroing out of parameters is a specific feature the optimization
method used in those packages.

When `attention_type` is `"row"` or `"both"`, SAINT applies inter-sample
(row) attention across the samples in a batch. The engine keeps this on
at prediction time by default, so the prediction for a given row depends
on which other rows are passed to
[`predict()`](https://rdrr.io/r/stats/predict.html) in the same call. To
obtain batch-independent predictions, bypass row attention at predict
time with `set_engine("brulee", row_attention_on_predict = FALSE)`.

### Translation from parsnip to the original package (regression)

    tabular_saint(
      epochs = integer(1),
      num_embedding = integer(1),
      attention_type = character(1),
      num_attn_heads = integer(1),
      num_attn_blocks = integer(1),
      dropout_attn = double(1),
      dropout_hidden = double(1),
      dropout_last = double(1),
      hidden_units = integer(1),
      hidden_activations = character(1),
      target_token = logical(1),
      penalty = double(1),
      mixture = double(1),
      learn_rate = double(1),
      rate_schedule = character(1),
      momentum = double(1),
      batch_size = NULL,
      class_weights = NULL,
      stop_iter = integer(1)
    ) |>
      set_engine("brulee") |>
      set_mode("regression") |>
      translate()

    ## tabular saint Model Specification (regression)
    ##
    ## Main Arguments:
    ##   epochs = integer(1)
    ##   num_embedding = integer(1)
    ##   attention_type = character(1)
    ##   num_attn_heads = integer(1)
    ##   num_attn_blocks = integer(1)
    ##   dropout_attn = double(1)
    ##   dropout_hidden = double(1)
    ##   dropout_last = double(1)
    ##   hidden_units = integer(1)
    ##   hidden_activations = character(1)
    ##   target_token = logical(1)
    ##   penalty = double(1)
    ##   mixture = double(1)
    ##   learn_rate = double(1)
    ##   rate_schedule = character(1)
    ##   momentum = double(1)
    ##   stop_iter = integer(1)
    ##
    ## Computational engine: brulee
    ##
    ## Model fit template:
    ## brulee::brulee_saint(x = missing_arg(), y = missing_arg(), epochs = integer(1),
    ##     num_embedding = integer(1), attention_type = character(1),
    ##     num_attn_heads = integer(1), num_attn_blocks = integer(1),
    ##     dropout_attn = double(1), dropout_hidden = double(1), dropout_last = double(1),
    ##     hidden_units = integer(1), hidden_activations = character(1),
    ##     target_token = logical(1), penalty = double(1), mixture = double(1),
    ##     learn_rate = double(1), rate_schedule = character(1), momentum = double(1),
    ##     stop_iter = integer(1))

Note that parsnip automatically sets linear activation in the last
layer.

### Translation from parsnip to the original package (classification)

    tabular_saint(
      epochs = integer(1),
      num_embedding = integer(1),
      attention_type = character(1),
      num_attn_heads = integer(1),
      num_attn_blocks = integer(1),
      dropout_attn = double(1),
      dropout_hidden = double(1),
      dropout_last = double(1),
      hidden_units = integer(1),
      hidden_activations = character(1),
      target_token = logical(1),
      penalty = double(1),
      mixture = double(1),
      learn_rate = double(1),
      rate_schedule = character(1),
      momentum = double(1),
      batch_size = NULL,
      class_weights = NULL,
      stop_iter = integer(1)
    ) |>
      set_engine("brulee") |>
      set_mode("classification") |>
      translate()

    ## tabular saint Model Specification (classification)
    ##
    ## Main Arguments:
    ##   epochs = integer(1)
    ##   num_embedding = integer(1)
    ##   attention_type = character(1)
    ##   num_attn_heads = integer(1)
    ##   num_attn_blocks = integer(1)
    ##   dropout_attn = double(1)
    ##   dropout_hidden = double(1)
    ##   dropout_last = double(1)
    ##   hidden_units = integer(1)
    ##   hidden_activations = character(1)
    ##   target_token = logical(1)
    ##   penalty = double(1)
    ##   mixture = double(1)
    ##   learn_rate = double(1)
    ##   rate_schedule = character(1)
    ##   momentum = double(1)
    ##   stop_iter = integer(1)
    ##
    ## Computational engine: brulee
    ##
    ## Model fit template:
    ## brulee::brulee_saint(x = missing_arg(), y = missing_arg(), epochs = integer(1),
    ##     num_embedding = integer(1), attention_type = character(1),
    ##     num_attn_heads = integer(1), num_attn_blocks = integer(1),
    ##     dropout_attn = double(1), dropout_hidden = double(1), dropout_last = double(1),
    ##     hidden_units = integer(1), hidden_activations = character(1),
    ##     target_token = logical(1), penalty = double(1), mixture = double(1),
    ##     learn_rate = double(1), rate_schedule = character(1), momentum = double(1),
    ##     stop_iter = integer(1))

### Preprocessing requirements

`brulee_saint()` natively handles factor predictors via learned
embeddings. Factor columns are automatically detected and embedded,
while numeric columns use a scaled embedding. There is *no need to
pre-encode factors as indicators*.

Predictors should have the same scale. One way to achieve this is to
center and scale each so that each predictor has mean zero and a
variance of one.

### Case weights

The underlying model implementation does not allow for case weights.

### Prediction types

    parsnip:::get_from_env("tabular_saint_predict") |>
      dplyr::filter(engine == "brulee") |>
      dplyr::select(mode, type)

    ## # A tibble: 3 x 2
    ##   mode           type
    ##   <chr>          <chr>
    ## 1 regression     numeric
    ## 2 classification class
    ## 3 classification prob

### References

- Somepalli, G., Goldblum, M., Schwarzschild, A., Bruss, C. B., &
  Goldstein, T. (2021). SAINT: Improved Neural Networks for Tabular Data
  via Row Attention and Contrastive Pre-Training. *arXiv preprint*
  arXiv:2106.01342.

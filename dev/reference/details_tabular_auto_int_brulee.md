# AutoInt via brulee

`brulee::brulee_auto_int()` fits a neural network.

## Details

For this engine, there are multiple modes: classification and regression

### Tuning Parameters

This model has 19 tuning parameters:

- `epochs`: \# Epochs (type: integer, default: 100L)

- `num_embedding`: \# Embedding Dimensions (type: integer, default: 16L)

- `hidden_units`: \# Hidden Units (type: integer, default: NULL)

- `hidden_activations`: Activation Function (type: character, default:
  NULL)

- `num_attn_feat`: \# Attention Features (type: integer, default: 16L)

- `num_attn_heads`: \# Attention Heads (type: integer, default: 2L)

- `num_attn_blocks`: \# Attention Blocks (type: integer, default: 3L)

- `activation`: Activation Function (type: character, default: ‘relu’)

- `dropout`: Dropout Rate (type: double, default: 0.0)

- `dropout_attn`: Attention Dropout Rate (type: double, default: 0.0)

- `dropout_embedding`: Embedding Dropout Rate (type: double, default:
  0.0)

- `penalty`: Amount of Regularization (type: double, default: 0.001)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 0.0)

- `learn_rate`: Learning Rate (type: double, default: 0.01)

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

### Translation from parsnip to the original package (regression)

    tabular_auto_int(
      epochs = integer(1),
      num_embedding = integer(1),
      hidden_units = NULL,
      hidden_activations = NULL,
      num_attn_feat = integer(1),
      num_attn_heads = integer(1),
      num_attn_blocks = integer(1),
      activation = character(1),
      dropout = double(1),
      dropout_attn = double(1),
      dropout_embedding = double(1),
      penalty = double(1),
      mixture = double(1),
      learn_rate = double(1),
      rate_schedule = tune(),
      momentum = character(1),
      batch_size = NULL,
      class_weights = NULL,
      stop_iter = integer(1)
      ) |>
      set_engine("brulee") |>
      set_mode("regression") |>
      translate()

    ## tabular auto int Model Specification (regression)
    ##
    ## Main Arguments:
    ##   epochs = integer(1)
    ##   num_embedding = integer(1)
    ##   num_attn_feat = integer(1)
    ##   num_attn_heads = integer(1)
    ##   num_attn_blocks = integer(1)
    ##   activation = character(1)
    ##   dropout = double(1)
    ##   dropout_attn = double(1)
    ##   dropout_embedding = double(1)
    ##   penalty = double(1)
    ##   mixture = double(1)
    ##   learn_rate = double(1)
    ##   rate_schedule = tune()
    ##   momentum = character(1)
    ##   stop_iter = integer(1)
    ##
    ## Computational engine: brulee
    ##
    ## Model fit template:
    ## brulee::brulee_auto_int(x = missing_arg(), y = missing_arg(),
    ##     epochs = integer(1), num_embedding = integer(1), num_attn_feat = integer(1),
    ##     num_attn_heads = integer(1), num_attn_blocks = integer(1),
    ##     activation = character(1), dropout = double(1), dropout_attn = double(1),
    ##     dropout_embedding = double(1), penalty = double(1), mixture = double(1),
    ##     learn_rate = double(1), rate_schedule = tune(), momentum = character(1),
    ##     stop_iter = integer(1))

Note that parsnip automatically sets linear activation in the last
layer.

### Translation from parsnip to the original package (classification)

    tabular_auto_int(
      epochs = integer(1),
      num_embedding = integer(1),
      hidden_units = NULL,
      hidden_activations = NULL,
      num_attn_feat = integer(1),
      num_attn_heads = integer(1),
      num_attn_blocks = integer(1),
      activation = character(1),
      dropout = double(1),
      dropout_attn = double(1),
      dropout_embedding = double(1),
      penalty = double(1),
      mixture = double(1),
      learn_rate = double(1),
      rate_schedule = tune(),
      momentum = character(1),
      batch_size = NULL,
      class_weights = NULL,
      stop_iter = integer(1)
    ) |>
      set_engine("brulee") |>
      set_mode("classification") |>
      translate()

    ## tabular auto int Model Specification (classification)
    ##
    ## Main Arguments:
    ##   epochs = integer(1)
    ##   num_embedding = integer(1)
    ##   num_attn_feat = integer(1)
    ##   num_attn_heads = integer(1)
    ##   num_attn_blocks = integer(1)
    ##   activation = character(1)
    ##   dropout = double(1)
    ##   dropout_attn = double(1)
    ##   dropout_embedding = double(1)
    ##   penalty = double(1)
    ##   mixture = double(1)
    ##   learn_rate = double(1)
    ##   rate_schedule = tune()
    ##   momentum = character(1)
    ##   stop_iter = integer(1)
    ##
    ## Computational engine: brulee
    ##
    ## Model fit template:
    ## brulee::brulee_auto_int(x = missing_arg(), y = missing_arg(),
    ##     epochs = integer(1), num_embedding = integer(1), num_attn_feat = integer(1),
    ##     num_attn_heads = integer(1), num_attn_blocks = integer(1),
    ##     activation = character(1), dropout = double(1), dropout_attn = double(1),
    ##     dropout_embedding = double(1), penalty = double(1), mixture = double(1),
    ##     learn_rate = double(1), rate_schedule = tune(), momentum = character(1),
    ##     stop_iter = integer(1))

### Preprocessing requirements

`brulee_auto_int()` natively handles factor predictors via learned
embeddings. Factor columns are automatically detected and embedded,
while numeric columns use a scaled embedding. There is *no need to
pre-encode factors as indicators*.

Predictors should have the same scale. One way to achieve this is to
center and scale each so that each predictor has mean zero and a
variance of one.

### Case weights

The underlying model implementation does not allow for case weights.

### Prediction types

    parsnip:::get_from_env("tabular_auto_int_predict") |>
      dplyr::filter(engine == "brulee") |>
      dplyr::select(mode, type)

    ## # A tibble: 3 x 2
    ##   mode           type
    ##   <chr>          <chr>
    ## 1 regression     numeric
    ## 2 classification class
    ## 3 classification prob

### References

- Song, W., Shi, C., Xiao, Z., Duan, Z., Xu, Y., Zhang, M., & Tang, J.
  (2019). AutoInt: Automatic Feature Interaction Learning via
  Self-Attentive Neural Networks. In *Proceedings of the 28th ACM
  International Conference on Information and Knowledge Management
  (CIKM)*.

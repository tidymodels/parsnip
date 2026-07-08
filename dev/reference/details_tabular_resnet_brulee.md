# Tabular ResNet via brulee

`brulee::brulee_resnet()` fits a residual neural network.

## Details

For this engine, there are multiple modes: classification and regression

### Tuning Parameters

This model has 14 tuning parameters:

- `hidden_units`: \# Hidden Units (type: integer, default: 3L)

- `bottleneck_units`: \# Bottleneck Units (type: integer, default: same
  as hidden_units)

- `residual_at`: Residual Locations (type: integer, default: NULL)

- `penalty`: Amount of Regularization (type: double, default: 0.001)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 0.0)

- `epochs`: \# Epochs (type: integer, default: 100L)

- `dropout`: Dropout Rate (type: double, default: 0.0)

- `learn_rate`: Learning Rate (type: double, default: 0.01)

- `activation`: Activation Function (type: character, default: ‘relu’)

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

    tabular_resnet(
      epochs = integer(1),
      hidden_units = integer(1),
      bottleneck_units = integer(1),
      residual_at = NULL,
      activation = character(1),
      penalty = double(1),
      mixture = double(1),
      dropout = double(1),
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

    ## tabular resnet Model Specification (regression)
    ##
    ## Main Arguments:
    ##   hidden_units = integer(1)
    ##   bottleneck_units = integer(1)
    ##   penalty = double(1)
    ##   mixture = double(1)
    ##   dropout = double(1)
    ##   epochs = integer(1)
    ##   activation = character(1)
    ##   learn_rate = double(1)
    ##   rate_schedule = character(1)
    ##   momentum = double(1)
    ##   stop_iter = integer(1)
    ##
    ## Computational engine: brulee
    ##
    ## Model fit template:
    ## brulee::brulee_resnet(x = missing_arg(), y = missing_arg(), hidden_units = integer(1),
    ##     bottleneck_units = integer(1), penalty = double(1), mixture = double(1),
    ##     dropout = double(1), epochs = integer(1), activation = character(1),
    ##     learn_rate = double(1), rate_schedule = character(1), momentum = double(1),
    ##     stop_iter = integer(1))

Note that parsnip automatically sets linear activation in the last
layer.

### Translation from parsnip to the original package (classification)

    tabular_resnet(
      epochs = integer(1),
      hidden_units = integer(1),
      bottleneck_units = integer(1),
      residual_at = NULL,
      activation = character(1),
      penalty = double(1),
      mixture = double(1),
      dropout = double(1),
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

    ## tabular resnet Model Specification (classification)
    ##
    ## Main Arguments:
    ##   hidden_units = integer(1)
    ##   bottleneck_units = integer(1)
    ##   penalty = double(1)
    ##   mixture = double(1)
    ##   dropout = double(1)
    ##   epochs = integer(1)
    ##   activation = character(1)
    ##   learn_rate = double(1)
    ##   rate_schedule = character(1)
    ##   momentum = double(1)
    ##   stop_iter = integer(1)
    ##
    ## Computational engine: brulee
    ##
    ## Model fit template:
    ## brulee::brulee_resnet(x = missing_arg(), y = missing_arg(), hidden_units = integer(1),
    ##     bottleneck_units = integer(1), penalty = double(1), mixture = double(1),
    ##     dropout = double(1), epochs = integer(1), activation = character(1),
    ##     learn_rate = double(1), rate_schedule = character(1), momentum = double(1),
    ##     stop_iter = integer(1))

### Preprocessing requirements

`brulee_resnet()` requires numeric predictors. Factor or categorical
predictors need to be converted to numeric values (e.g., dummy or
indicator variables) before fitting; parsnip does *not* create indicator
variables for this engine, so use a recipe (or some other method) to
make them numeric.

Predictors should have the same scale. One way to achieve this is to
center and scale each so that each predictor has mean zero and a
variance of one.

### Case weights

The underlying model implementation does not allow for case weights.

### Prediction types

    parsnip:::get_from_env("tabular_resnet_predict") |>
      dplyr::filter(engine == "brulee") |>
      dplyr::select(mode, type)

    ## # A tibble: 3 x 2
    ##   mode           type
    ##   <chr>          <chr>
    ## 1 regression     numeric
    ## 2 classification class
    ## 3 classification prob

### References

- Gorishniy, Y., Rubachev, I., Khrulkov, V., & Babenko, A. (2021).
  Revisiting deep learning models for tabular data. *Advances in Neural
  Information Processing Systems*, 34.

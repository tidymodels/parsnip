# Regularization learning network via brulee

`brulee::brulee_rln()` fits a regularization learning network.

## Details

For this engine, there is a single mode: regression

### Tuning Parameters

This model has 11 tuning parameters:

- `hidden_units`: \# Hidden Units (type: integer, default: 5L)

- `penalty_type`: Penalty Type (type: character, default: ‘L1’)

- `penalty_average`: Penalty Average (type: double, default: 1e-10)

- `step_rate`: Step Rate (type: double, default: 1e6)

- `activation`: Activation Function (type: character, default: ‘relu’)

- `epochs`: \# Epochs (type: integer, default: 100L)

- `learn_rate`: Learning Rate (type: double, default: 0.001)

- `rate_schedule`: Learning Rate Scheduler (type: character, default:
  ‘none’)

- `momentum`: Gradient Descent Momentum (type: double, default: 0.0)

- `batch_size`: Batch Size (type: integer, default: NULL)

- `stop_iter`: \# Iterations Before Stopping (type: integer, default:
  20L)

`penalty_average` and `step_rate` are specified on the natural scale but
are best tuned on the log10 scale.

### Translation from parsnip to the original package (regression)

    tabular_rln(
      hidden_units = integer(1),
      penalty_type = character(1),
      penalty_average = double(1),
      step_rate = double(1),
      activation = character(1),
      epochs = integer(1),
      learn_rate = double(1),
      rate_schedule = character(1),
      momentum = double(1),
      batch_size = NULL,
      stop_iter = integer(1)
    ) |>
      set_engine("brulee") |>
      set_mode("regression") |>
      translate()

    ## tabular rln Model Specification (regression)
    ##
    ## Main Arguments:
    ##   hidden_units = integer(1)
    ##   penalty_type = character(1)
    ##   penalty_average = double(1)
    ##   step_rate = double(1)
    ##   activation = character(1)
    ##   epochs = integer(1)
    ##   learn_rate = double(1)
    ##   rate_schedule = character(1)
    ##   momentum = double(1)
    ##   stop_iter = integer(1)
    ##
    ## Computational engine: brulee
    ##
    ## Model fit template:
    ## brulee::brulee_rln(x = missing_arg(), y = missing_arg(), hidden_units = integer(1),
    ##     penalty_type = character(1), penalty_average = double(1),
    ##     step_rate = double(1), activation = character(1), epochs = integer(1),
    ##     learn_rate = double(1), rate_schedule = character(1), momentum = double(1),
    ##     stop_iter = integer(1))

### Preprocessing requirements

`brulee_rln()` requires numeric predictors. Factor or categorical
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

    parsnip:::get_from_env("tabular_rln_predict") |>
      dplyr::filter(engine == "brulee") |>
      dplyr::select(mode, type)

    ## # A tibble: 1 x 2
    ##   mode       type
    ##   <chr>      <chr>
    ## 1 regression numeric

### References

- Shavitt, I., & Segal, E. (2018). Regularization learning networks:
  Deep learning for tabular datasets. *Advances in Neural Information
  Processing Systems*, 31, 1379-1389.

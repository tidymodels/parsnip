# linear quantile regression via quantreg - multiple quantiles

    Code
      ten_quant_pred <- predict(ten_quant, new_data = sac_test, quantile_levels = (0:
      9) / 9)
    Condition
      Error in `predict()`:
      ! When the mode is "quantile regression", `quantile_levels` are specified by `set_mode()`.


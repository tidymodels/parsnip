# `predict_survival()` errors early when `add_censoring_weights = TRUE` but `new_data` has no Surv column

    Code
      predict_survival(fake_fit, new_data = no_surv_data, eval_time = 1,
        add_censoring_weights = TRUE)
    Condition
      Error in `predict_survival()`:
      ! `add_censoring_weights = TRUE` requires a <Surv> column in `new_data`.


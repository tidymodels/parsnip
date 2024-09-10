# flexsurv execution

    Code
      res <- fit_xy(surv_basic, x = lung[, "age", drop = FALSE], y = lung$time,
      control = ctrl)
    Condition
      Error in `fit_xy()`:
      ! Survival models must use the formula interface.


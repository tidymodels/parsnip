# kknn execution

    Code
      fit_xy(hpc_basic, control = ctrl, x = hpc[, num_pred], y = hpc$input_fields)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a `factor`, not a `numeric`.

---

    Code
      fit(hpc_basic, hpc_bad_form, data = hpc, control = ctrl)
    Condition
      Error:
      ! object 'term' not found

# argument checks for data dimensions

    Code
      f_fit <- spec %>% fit(body_mass_g ~ ., data = penguins)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 328 samples will be used.

---

    Code
      xy_fit <- spec %>% fit_xy(x = penguins[, -6], y = penguins$body_mass_g)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 328 samples will be used.


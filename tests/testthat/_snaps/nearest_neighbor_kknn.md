# argument checks for data dimensions

    Code
      f_fit <- fit(spec, body_mass_g ~ ., data = penguins)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 328 samples will be used.

---

    Code
      xy_fit <- fit_xy(spec, x = penguins[, -6], y = penguins$body_mass_g)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 328 samples will be used.


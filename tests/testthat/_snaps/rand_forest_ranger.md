# argument checks for data dimensions

    Code
      f_fit <- spec %>% fit(body_mass_g ~ ., data = penguins)
    Warning <rlang_warning>
      1000 columns were requested but there were 6 predictors in the data. 6 will be used.
      1000 samples were requested but there were 333 rows in the data. 333 will be used.

---

    Code
      xy_fit <- spec %>% fit_xy(x = penguins[, -6], y = penguins$body_mass_g)
    Warning <rlang_warning>
      1000 columns were requested but there were 6 predictors in the data. 6 will be used.
      1000 samples were requested but there were 333 rows in the data. 333 will be used.


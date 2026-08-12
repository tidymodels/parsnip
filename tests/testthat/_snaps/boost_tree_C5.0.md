# C5.0 execution

    Code
      res <- fit(lc_basic, funded_amnt ~ term, data = lending_club, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a <factor>, not an integer vector.

# submodel prediction

    Code
      multi_predict(class_fit, newdata = wa_churn[1:4, vars], trees = 4, type = "prob")
    Condition
      Error in `multi_predict()`:
      ! Please use `new_data` instead of `newdata`.

# argument checks for data dimensions

    Code
      f_fit <- fit(spec, species ~ ., data = penguins)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 will be used.

---

    Code
      xy_fit <- fit_xy(spec, x = penguins[, -1], y = penguins$species)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 will be used.


# C5.0 execution

    Code
      res <- fit(lc_basic, funded_amnt ~ term, data = lending_club, engine = "C5.0",
      control = ctrl)
    Condition
      Error in `glue()`:
      ! Expecting '}'

# submodel prediction

    Code
      multi_predict(class_fit, newdata = wa_churn[1:4, vars], trees = 4, type = "prob")
    Condition
      Error in `multi_predict()`:
      ! Please use `new_data` instead of `newdata`.

# argument checks for data dimensions

    Code
      f_fit <- spec %>% fit(species ~ ., data = penguins)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 will be used.

---

    Code
      xy_fit <- spec %>% fit_xy(x = penguins[, -1], y = penguins$species)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 will be used.


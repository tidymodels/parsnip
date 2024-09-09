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


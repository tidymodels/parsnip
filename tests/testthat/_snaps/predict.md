# prepare_data() turns novel levels into missing values

    Code
      res <- prepare_data(lm_fit, data.frame(a = c("new", "x"), b = 1))
    Condition
      Warning:
      Novel level found in column "a": "new".
      i The level has been removed, and values have been coerced to <NA>.


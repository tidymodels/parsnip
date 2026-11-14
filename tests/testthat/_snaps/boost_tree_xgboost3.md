# xgboost3 converts novel levels to missing values

    Code
      res <- predict(xy_fit, data.frame(a = c("new", "c"), b = 0))
    Condition
      Warning:
      Novel level found in column "a": "new".
      i The level has been removed, and values have been coerced to <NA>.

# xgboost3 rejects the exact tree method with factors

    Code
      fit(spec, y ~ a + b, data = dat)
    Condition
      Error in `xgb_train()`:
      ! `tree_method = "exact"` cannot be used with factor predictors.
      i Use "hist" or "approx", or convert the factors to indicator columns (e.g. with the "xgboost" engine).


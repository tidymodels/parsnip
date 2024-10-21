# xgboost execution, classification

    Code
      res <- parsnip::fit(hpc_xgboost, class ~ novar, data = hpc, control = ctrl)
    Condition
      Error:
      ! object 'novar' not found

# submodel prediction

    Code
      multi_predict(class_fit, newdata = wa_churn[1:4, vars], trees = 5, type = "prob")
    Condition
      Error in `multi_predict()`:
      ! Please use `new_data` instead of `newdata`.

# validation sets

    Code
      reg_fit <- boost_tree(trees = 20, mode = "regression") %>% set_engine("xgboost",
        validation = 3) %>% fit(mpg ~ ., data = mtcars[-(1:4), ])
    Condition
      Error in `parsnip::xgb_train()`:
      ! `validation` should be on [0, 1).

# early stopping

    Code
      reg_fit <- boost_tree(trees = 20, stop_iter = 30, mode = "regression") %>%
        set_engine("xgboost", validation = 0.1) %>% fit(mpg ~ ., data = mtcars[-(1:4),
      ])
    Condition
      Warning:
      `early_stop` was reduced to 19.

---

    Code
      reg_fit <- boost_tree(trees = 20, stop_iter = 0, mode = "regression") %>%
        set_engine("xgboost", validation = 0.1) %>% fit(mpg ~ ., data = mtcars[-(1:4),
      ])
    Condition
      Error in `parsnip::xgb_train()`:
      ! `early_stop` should be on [2, 20).

# xgboost data conversion

    Code
      from_df <- parsnip:::as_xgb_data(mtcar_x, mtcars_y, event_level = "second")
    Condition
      Warning:
      `event_level` can only be set for binary outcomes.

# argument checks for data dimensions

    Code
      f_fit <- spec %>% fit(species ~ ., data = penguins, control = ctrl)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 will be used.

---

    Code
      xy_fit <- spec %>% fit_xy(x = penguins_dummy, y = penguins$species, control = ctrl)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 will be used.

# count/proportion parameters

    Code
      boost_tree(mtry = 0.9, trees = 4) %>% set_engine("xgboost") %>% set_mode(
        "regression") %>% fit(mpg ~ ., data = mtcars)
    Condition
      Error in `xgb_train()`:
      ! The option `counts = TRUE` was used but `colsample_bynode` was given as 0.9.
      i Please use a value >= 1 or use `counts = FALSE`.

# interface to param arguments

    ! Please supply elements of the `params` list argument as main arguments to `set_engine()` rather than as part of `params`.
    i See `?details_boost_tree_xgboost` for more information.

---

    ! Please supply elements of the `params` list argument as main arguments to `set_engine()` rather than as part of `params`.
    i See `?details_boost_tree_xgboost` for more information.

---

    ! The argument `watchlist` is guarded by parsnip and will not be passed to `xgb.train()`.

---

    ! The arguments `watchlist` and `data` are guarded by parsnip and will not be passed to `xgb.train()`.

---

    ! Please supply elements of the `params` list argument as main arguments to `set_engine()` rather than as part of `params`.
    i See `?details_boost_tree_xgboost` for more information.


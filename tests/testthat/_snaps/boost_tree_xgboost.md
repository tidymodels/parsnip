# submodel prediction

    Code
      multi_predict(class_fit, newdata = wa_churn[1:4, vars], trees = 5, type = "prob")
    Condition
      Error in `multi_predict()`:
      ! Please use `new_data` instead of `newdata`.

# xgboost data conversion

    Code
      from_df <- parsnip:::as_xgb_data(mtcar_x, mtcars_y, event_level = "second")
    Condition
      Warning:
      `event_level` can only be set for binary outcomes.

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


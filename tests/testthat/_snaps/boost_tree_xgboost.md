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
      reg_fit <- fit(set_engine(boost_tree(trees = 20, mode = "regression"),
      "xgboost", validation = 3), mpg ~ ., data = mtcars[-(1:4), ])
    Condition
      Error in `parsnip::xgb_train()`:
      ! `validation` should be on [0, 1).

# early stopping

    Code
      reg_fit <- fit(set_engine(boost_tree(trees = 20, stop_iter = 30, mode = "regression"),
      "xgboost", validation = 0.1), mpg ~ ., data = mtcars[-(1:4), ])
    Condition
      Warning:
      `early_stop` was reduced to 19.

---

    Code
      reg_fit <- fit(set_engine(boost_tree(trees = 20, stop_iter = 0, mode = "regression"),
      "xgboost", validation = 0.1), mpg ~ ., data = mtcars[-(1:4), ])
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
      f_fit <- fit(spec, species ~ ., data = penguins, control = ctrl)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 will be used.

---

    Code
      xy_fit <- fit_xy(spec, x = penguins_dummy, y = penguins$species, control = ctrl)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 will be used.

# count/proportion parameters

    Code
      fit(set_mode(set_engine(boost_tree(mtry = 0.9, trees = 4), "xgboost"),
      "regression"), mpg ~ ., data = mtcars)
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

# xgboost execution, quantile regression

    Code
      spec_1
    Output
      Boosted Tree Model Specification (quantile regression)
      
      Main Arguments:
        trees = 50
      
      Engine-Specific Arguments:
        validation = 0.1
      
      Computational engine: xgboost 
      
    Message
      Quantile levels: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, and 0.9.

---

    Code
      print(qnt_fit_1)
    Output
      parsnip model object
      
      ##### xgb.Booster
      call:
        xgboost::xgb.train(params = list(eta = 0.3, max_depth = 6, gamma = 0, 
          colsample_bytree = 1, colsample_bynode = 1, min_child_weight = 1, 
          subsample = 1, objective = "reg:quantileerror", nthread = 1, 
          quantile_alpha = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 
          0.9)), data = x$data, nrounds = 50, evals = x$watchlist, 
          verbose = 0)
      # of features: 20 
      # of rounds:  50 
      callbacks:
         evaluation_log 
      evaluation_log:
        iter validation_quantile
       <num>               <num>
           1            7.009181
           2            6.548682
         ---                 ---
          49            5.314723
          50            5.304116

---

    Code
      spec_2
    Output
      Boosted Tree Model Specification (quantile regression)
      
      Main Arguments:
        trees = 50
        stop_iter = 2
      
      Engine-Specific Arguments:
        validation = 0.1
      
      Computational engine: xgboost 
      
    Message
      Quantile levels: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, and 0.9.

---

    Code
      print(qnt_fit_2)
    Output
      parsnip model object
      
      ##### xgb.Booster
      call:
        xgboost::xgb.train(params = list(eta = 0.3, max_depth = 6, gamma = 0, 
          colsample_bytree = 1, colsample_bynode = 1, min_child_weight = 1, 
          subsample = 1, objective = "reg:quantileerror", nthread = 1, 
          quantile_alpha = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 
          0.9)), data = x$data, nrounds = 50, evals = x$watchlist, 
          verbose = 0, early_stopping_rounds = 2)
      # of features: 20 
      # of rounds:  42 
      xgb.attributes:
         best_iteration, best_score 
      callbacks:
         early_stop, evaluation_log 
      evaluation_log:
        iter validation_quantile
       <num>               <num>
           1            7.009181
           2            6.548682
         ---                 ---
          41            5.339619
          42            5.342320


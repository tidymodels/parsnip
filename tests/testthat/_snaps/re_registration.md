# re-registration of fit information

    Code
      set_fit(model = "bart", eng = "dbarts", mode = "regression", value = list(
        interface = "data.frame", data = c(x = "x.train", y = "y.train"), protect = c(
          "x", "y"), func = c(pkg = "dbarts", fun = "bart"), defaults = list(verbose = TRUE,
          keeptrees = TRUE, keepcall = FALSE)))
    Condition
      Error in `set_fit()`:
      ! The combination of engine `dbarts` and mode `regression` already has fit data for model `bart` and the new information being registered is different.

# re-registration of encoding information

    Code
      set_encoding(model = "bart", eng = "dbarts", mode = "regression", options = list(
        predictor_indicators = "none", compute_intercept = TRUE, remove_intercept = FALSE,
        allow_sparse_x = FALSE))
    Condition
      Error in `set_encoding()`:
      ! The combination of engine `dbarts` and mode `regression` already has encoding data for model `bart` and the new information being registered is different.

# re-registration of prediction information

    Code
      set_pred(model = "bart", eng = "dbarts", mode = "regression", type = "numeric",
        value = list(pre = NULL, post = NULL, func = c(pkg = "parsnip", fun = "dbart_predict_calc"),
        args = list(obj = quote(object), new_data = quote(new_data), type = "tuba")))
    Condition
      Error in `set_pred()`:
      ! The combination of engine `dbarts` and mode `regression` and prediction type "numeric" already has predict data for model `bart` and the new information being registered is different.


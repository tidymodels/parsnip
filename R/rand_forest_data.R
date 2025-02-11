# wrappers for ranger
ranger_class_pred <-
  function(results, object)  {
    if (results$treetype == "Probability estimation") {
      res <- colnames(results$predictions)[apply(results$predictions, 1, which.max)]
    } else {
      res <- results$predictions
    }
    res
  }

ranger_num_confint <- function(object, new_data, ...) {
  hf_lvl <- (1 - object$spec$method$pred$conf_int$extras$level)/2
  const <- qnorm(hf_lvl, lower.tail = FALSE)

  res <-
    tibble(
      .pred = predict(object$fit, data = new_data, type = "response", ...)$predictions
    )
  std_error <- predict(object$fit, data = new_data, type = "se", ...)$se
  res$.pred_lower <- res$.pred - const * std_error
  res$.pred_upper <- res$.pred + const * std_error
  res$.pred <- NULL

  if (object$spec$method$pred$conf_int$extras$std_error)
    res$.std_error <- std_error
  res
}
ranger_class_confint <- function(object, new_data, ...) {
  hf_lvl <- (1 - object$spec$method$pred$conf_int$extras$level)/2
  const <- qnorm(hf_lvl, lower.tail = FALSE)

  pred <- predict(object$fit, data = new_data, type = "response", ...)$predictions
  pred <- as_tibble(pred)

  std_error <- predict(object$fit, data = new_data, type = "se", ...)$se
  colnames(std_error) <- colnames(pred)
  std_error <- as_tibble(std_error)
  names(std_error) <- paste0(".std_error_", names(std_error))

  lowers <- pred - const * std_error
  names(lowers) <- paste0(".pred_lower_", names(lowers))
  uppers <- pred + const * std_error
  names(uppers) <- paste0(".pred_upper_", names(uppers))

  res <- cbind(lowers, uppers)
  res[res < 0] <- 0
  res[res > 1] <- 1
  res <- as_tibble(res)
  lvl <- rep(object$fit$forest$levels, each = 2)
  col_names <- paste0(c(".pred_lower_", ".pred_upper_"), lvl)
  res <- res[, col_names]

  if (object$spec$method$pred$conf_int$extras$std_error)
    res <- bind_cols(res, std_error)

  res
}

ranger_confint <- function(object, new_data, ...) {
  if (object$fit$forest$treetype == "Regression") {
    res <- ranger_num_confint(object, new_data, ...)
  } else {
    if (object$fit$forest$treetype == "Probability estimation") {
      res <- ranger_class_confint(object, new_data, ...)
    } else {
      cli::cli_abort(
        "Cannot compute confidence intervals for a ranger forest
         of type {.val {object$fit$forest$treetype}}.",
        call = rlang::call2("predict")
      )
    }
  }
  res
}

# ------------------------------------------------------------------------------

set_new_model("rand_forest")

set_model_mode("rand_forest", "classification")
set_model_mode("rand_forest", "regression")
set_model_mode("rand_forest", "censored regression")

# ------------------------------------------------------------------------------
# ranger components

set_model_engine("rand_forest", "classification", "ranger")
set_model_engine("rand_forest", "regression", "ranger")
set_dependency("rand_forest", "ranger", "ranger", mode = "classification")
set_dependency("rand_forest", "ranger", "ranger", mode = "regression")

set_model_arg(
  model = "rand_forest",
  eng = "ranger",
  parsnip = "mtry",
  original = "mtry",
  func = list(pkg = "dials", fun = "mtry"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rand_forest",
  eng = "ranger",
  parsnip = "trees",
  original = "num.trees",
  func = list(pkg = "dials", fun = "trees"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rand_forest",
  eng = "ranger",
  parsnip = "min_n",
  original = "min.node.size",
  func = list(pkg = "dials", fun = "min_n"),
  has_submodel = FALSE
)

set_fit(
  model = "rand_forest",
  eng = "ranger",
  mode = "classification",
  value = list(
    interface = "data.frame",
    data = c(x = "x", y = "y", weights = "case.weights"),
    protect = c("x", "y", "weights"),
    func = c(pkg = "ranger", fun = "ranger"),
    defaults =
      list(
        num.threads = 1,
        verbose = FALSE,
        seed = expr(sample.int(10 ^ 5, 1))
      )
  )
)

set_encoding(
  model = "rand_forest",
  eng = "ranger",
  mode = "classification",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = TRUE
  )
)

set_fit(
  model = "rand_forest",
  eng = "ranger",
  mode = "regression",
  value = list(
    interface = "data.frame",
    data = c(x = "x", y = "y", weights = "case.weights"),
    protect = c("x", "y", "weights"),
    func = c(pkg = "ranger", fun = "ranger"),
    defaults =
      list(
        num.threads = 1,
        verbose = FALSE,
        seed = expr(sample.int(10 ^ 5, 1))
      )
  )
)

set_encoding(
  model = "rand_forest",
  eng = "ranger",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = TRUE
  )
)

set_pred(
  model = "rand_forest",
  eng = "ranger",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = ranger_class_pred,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        data = quote(new_data),
        type = "response",
        seed = expr(sample.int(10 ^ 5, 1)),
        verbose = FALSE
      )
  )
)

set_pred(
  model = "rand_forest",
  eng = "ranger",
  mode = "classification",
  type = "prob",
  value = list(
    pre = function(x, object) {
      if (object$fit$forest$treetype != "Probability estimation")
        cli::cli_abort(
          c(
            "`ranger` model does not appear to use class probabilities.",
            "i" = "Was the model fit with `probability = TRUE`?"
          ),
          call = call2("predict")
        )
      x
    },
    post = function(x, object) {
      x <- x$prediction
      as_tibble(x)
    },
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        data = quote(new_data),
        seed = expr(sample.int(10 ^ 5, 1)),
        verbose = FALSE
      )
  )
)

set_pred(
  model = "rand_forest",
  eng = "ranger",
  mode = "classification",
  type = "conf_int",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "ranger_confint"),
    args =
      list(
        object = quote(object),
        new_data = quote(new_data),
        seed = expr(sample.int(10^5, 1))
      )
  )
)

set_pred(
  model = "rand_forest",
  eng = "ranger",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        data = quote(new_data),
        seed = expr(sample.int(10 ^ 5, 1))
      )
  )
)

set_pred(
  model = "rand_forest",
  eng = "ranger",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = function(results, object)
      results$predictions,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        data = quote(new_data),
        type = "response",
        seed = expr(sample.int(10 ^ 5, 1)),
        verbose = FALSE
      )
  )
)


set_pred(
  model = "rand_forest",
  eng = "ranger",
  mode = "regression",
  type = "conf_int",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "ranger_confint"),
    args =
      list(
        object = quote(object),
        new_data = quote(new_data),
        seed = expr(sample.int(10^5, 1))
      )
  )
)
set_pred(
  model = "rand_forest",
  eng = "ranger",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        data = quote(new_data),
        seed = expr(sample.int(10 ^ 5, 1))
      )
  )
)

# ------------------------------------------------------------------------------
# randomForest components

set_model_engine("rand_forest", "classification", "randomForest")
set_model_engine("rand_forest", "regression",     "randomForest")
set_dependency("rand_forest", "randomForest", "randomForest", mode = "regression")
set_dependency("rand_forest", "randomForest", "randomForest", mode = "classification")

set_model_arg(
  model = "rand_forest",
  eng = "randomForest",
  parsnip = "mtry",
  original = "mtry",
  func = list(pkg = "dials", fun = "mtry"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rand_forest",
  eng = "randomForest",
  parsnip = "trees",
  original = "ntree",
  func = list(pkg = "dials", fun = "trees"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rand_forest",
  eng = "randomForest",
  parsnip = "min_n",
  original = "nodesize",
  func = list(pkg = "dials", fun = "min_n"),
  has_submodel = FALSE
)

set_fit(
  model = "rand_forest",
  eng = "randomForest",
  mode = "classification",
  value = list(
    interface = "data.frame",
    protect = c("x", "y"),
    func = c(pkg = "randomForest", fun = "randomForest"),
    defaults =
      list()
  )
)

set_encoding(
  model = "rand_forest",
  eng = "randomForest",
  mode = "classification",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_fit(
  model = "rand_forest",
  eng = "randomForest",
  mode = "regression",
  value = list(
    interface = "data.frame",
    protect = c("x", "y"),
    func = c(pkg = "randomForest", fun = "randomForest"),
    defaults =
      list()
  )
)

set_encoding(
  model = "rand_forest",
  eng = "randomForest",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "rand_forest",
  eng = "randomForest",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(object = quote(object$fit),
           newdata = quote(new_data))
  )
)

set_pred(
  model = "rand_forest",
  eng = "randomForest",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(object = quote(object$fit),
           newdata = quote(new_data))
  )
)

set_pred(
  model = "rand_forest",
  eng = "randomForest",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)


set_pred(
  model = "rand_forest",
  eng = "randomForest",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      as_tibble(as.data.frame(x))
    },
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "prob"
      )
  )
)

set_pred(
  model = "rand_forest",
  eng = "randomForest",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(object = quote(object$fit),
           newdata = quote(new_data))
  )
)

# ------------------------------------------------------------------------------
# spark components

set_model_engine("rand_forest", "classification", "spark")
set_model_engine("rand_forest", "regression", "spark")
set_dependency("rand_forest", "spark", "sparklyr")

set_model_arg(
  model = "rand_forest",
  eng = "spark",
  parsnip = "mtry",
  original = "feature_subset_strategy",
  func = list(pkg = "dials", fun = "mtry"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rand_forest",
  eng = "spark",
  parsnip = "trees",
  original = "num_trees",
  func = list(pkg = "dials", fun = "trees"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rand_forest",
  eng = "spark",
  parsnip = "min_n",
  original = "min_instances_per_node",
  func = list(pkg = "dials", fun = "min_n"),
  has_submodel = FALSE
)

set_fit(
  model = "rand_forest",
  eng = "spark",
  mode = "classification",
  value = list(
    interface = "formula",
    data = c(formula = "formula", data = "x"),
    protect = c("x", "formula", "type"),
    func = c(pkg = "sparklyr", fun = "ml_random_forest"),
    defaults = list(seed = expr(sample.int(10 ^ 5, 1)))
  )
)

set_encoding(
  model = "rand_forest",
  eng = "spark",
  mode = "classification",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_fit(
  model = "rand_forest",
  eng = "spark",
  mode = "regression",
  value = list(
    interface = "formula",
    data = c(formula = "formula", data = "x"),
    protect = c("x", "formula", "type"),
    func = c(pkg = "sparklyr", fun = "ml_random_forest"),
    defaults = list(seed = expr(sample.int(10 ^ 5, 1)))
  )
)

set_encoding(
  model = "rand_forest",
  eng = "spark",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "rand_forest",
  eng = "spark",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = format_spark_num,
    func = c(pkg = "sparklyr", fun = "ml_predict"),
    args =
      list(x = quote(object$fit),
           dataset = quote(new_data))
  )
)

set_pred(
  model = "rand_forest",
  eng = "spark",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = format_spark_class,
    func = c(pkg = "sparklyr", fun = "ml_predict"),
    args =
      list(x = quote(object$fit),
           dataset = quote(new_data))
  )
)

set_pred(
  model = "rand_forest",
  eng = "spark",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = format_spark_probs,
    func = c(pkg = "sparklyr", fun = "ml_predict"),
    args =
      list(x = quote(object$fit),
           dataset = quote(new_data))
  )
)

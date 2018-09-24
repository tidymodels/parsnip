
rand_forest_arg_key <- data.frame(
  randomForest = c("mtry", "ntree", "nodesize"),
  ranger = c("mtry", "num.trees", "min.node.size"),
  spark =
    c("feature_subset_strategy", "num_trees", "min_instances_per_node"),
  stringsAsFactors = FALSE,
  row.names =  c("mtry", "trees", "min_n")
)

rand_forest_modes <- c("classification", "regression", "unknown")

rand_forest_engines <- data.frame(
  ranger =       c(TRUE, TRUE, FALSE),
  randomForest = c(TRUE, TRUE, FALSE),
  spark =        c(TRUE, TRUE, FALSE),
  row.names =  c("classification", "regression", "unknown")
)

# ------------------------------------------------------------------------------

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

#' @importFrom stats qnorm
ranger_num_confint <- function(object, new_data, ...) {
  hf_lvl <- (1 - object$spec$method$confint$extras$level)/2
  const <- qnorm(hf_lvl, lower.tail = FALSE)

  res <-
    tibble(
      .pred = predict(object$fit, data = new_data, type = "response", ...)$predictions
    )
  std_error <- predict(object$fit, data = new_data, type = "se", ...)$se
  res$.pred_lower <- res$.pred - const * std_error
  res$.pred_upper <- res$.pred + const * std_error
  res$.pred <- NULL

  if(object$spec$method$confint$extras$std_error)
    res$.std_error <- std_error
  res
}
ranger_class_confint <- function(object, new_data, ...) {
  hf_lvl <- (1 - object$spec$method$confint$extras$level)/2
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

  if(object$spec$method$confint$extras$std_error)
    res <- bind_cols(res, std_error)

  res
}

ranger_confint <- function(object, new_data, ...) {
  if(object$fit$forest$treetype == "Regression") {
    res <- ranger_num_confint(object, new_data, ...)
  } else {
    if(object$fit$forest$treetype == "Probability estimation") {
      res <- ranger_class_confint(object, new_data, ...)
    } else {
      stop ("Cannot compute confidence intervals for a ranger forest ",
            "of type ", object$fit$forest$treetype, ".", call. = FALSE)
    }
  }
  res
}

# ------------------------------------------------------------------------------


rand_forest_ranger_data <-
  list(
    libs = "ranger",
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "case.weights"),
      func = c(pkg = "ranger", fun = "ranger"),
      defaults =
        list(
          num.threads = 1,
          verbose = FALSE,
          seed = expr(sample.int(10^5, 1))
        )
    ),
    pred = list(
      pre = NULL,
      post = function(results, object) results$predictions,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          data = quote(new_data),
          type = "response",
          seed = expr(sample.int(10^5, 1)),
          verbose = FALSE
        )
    ),
    classes = list(
      pre = NULL,
      post = ranger_class_pred,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          data = quote(new_data),
          type = "response",
          seed = expr(sample.int(10^5, 1)),
          verbose = FALSE
        )
    ),
    prob = list(
      pre = function(x, object) {
        if (object$fit$forest$treetype != "Probability estimation")
          stop("`ranger` model does not appear to use class probabilities. Was ",
               "the model fit with `probability = TRUE`?",
               call. = FALSE)
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
          seed = expr(sample.int(10^5, 1)),
          verbose = FALSE
        )
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          data = quote(new_data),
          seed = expr(sample.int(10^5, 1))
        )
    ),
    confint = list(
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

rand_forest_randomForest_data <-
  list(
    libs = "randomForest",
    fit = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "randomForest", fun = "randomForest"),
      defaults =
        list()
    ),
    pred = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    ),
    classes = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    ),
    prob = list(
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
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )


rand_forest_spark_data <-
  list(
    libs = "sparklyr",
    fit = list(
      interface = "formula",
      protect = c("x", "formula", "type"),
      func = c(pkg = "sparklyr", fun = "ml_random_forest"),
      defaults =
        list(
          seed = expr(sample.int(10^5, 1))
        )
    ),
    pred = list(
      pre = NULL,
      post = format_spark_num,
      func = c(pkg = "sparklyr", fun = "ml_predict"),
      args =
        list(
          x = quote(object$fit),
          dataset = quote(new_data)
        )
    ),
    classes = list(
      pre = NULL,
      post = format_spark_class,
      func = c(pkg = "sparklyr", fun = "ml_predict"),
      args =
        list(
          x = quote(object$fit),
          dataset = quote(new_data)
        )
    ),
    prob = list(
      pre = NULL,
      post = format_spark_probs,
      func = c(pkg = "sparklyr", fun = "ml_predict"),
      args =
        list(
          x = quote(object$fit),
          dataset = quote(new_data)
        )
    )
  )

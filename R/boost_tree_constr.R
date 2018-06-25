
boost_tree_arg_key <- data.frame(
  xgboost = c("max_depth", "nrounds", "eta", "colsample_bytree", "min_child_weight", "gamma", "subsample"),
  stringsAsFactors = FALSE,
  row.names =  c("tree_depth", "trees", "learn_rate", "mtry", "min_n", "loss_reduction", "sample_size")
)

boost_tree_modes <- c("classification", "regression", "unknown")

boost_tree_engines <- data.frame(
  xgboost =       rep(TRUE, 3),
  row.names =  c("classification", "regression", "unknown")
)

###################################################################

#' @export
xgb_train <- function(
  x, y,
  max_depth = 6, nrounds = 100, eta  = 0.3, colsample_bytree = 1,
  min_child_weight = 1, gamma = 0, subsample = 1, ...) {

  num_class <- if (length(levels(y)) > 2) length(levels(y)) else NULL

  if (is.numeric(y)) {
    loss <- "reg:linear"
  } else {
    lvl <- levels(y)
    y <- as.numeric(y) - 1
    if (length(lvl) == 2) {
      loss <- "binary:logistic"
    } else {
      loss <- "multi:softprob"
    }
  }

  if (is.data.frame(x))
    x <- as.matrix(x) # maybe use model.matrix here?

  n <- nrow(x)
  p <- ncol(x)

  if (!inherits(x, "xgb.DMatrix"))
    x <- xgboost::xgb.DMatrix(x, label = y, missing = NA)
  else
    xgboost::setinfo(x, "label", y)

  # translate `subsample` and `colsample_bytree` to be on (0, 1] if not
  if(subsample > 1)
    subsample <- subsample/n
  if(subsample > 1)
    subsample <- 1

  if(colsample_bytree > 1)
    colsample_bytree <- colsample_bytree/p
  if(colsample_bytree > 1)
    colsample_bytree <- 1

  arg_list <- list(
    eta = eta,
    max_depth = max_depth,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    min_child_weight = min_child_weight,
    subsample = subsample
  )

  # eval if contains expressions?

  main_args <- list(
    data = quote(x),
    params = arg_list,
    nrounds = nrounds,
    objective = loss
  )
  if (!is.null(num_class))
    main_args$num_class <- num_class

  call <- make_call(fun = "xgb.train", ns = "xgboost", main_args)

  # override or add some other args
  others <- list(...)
  others <-
    others[!(names(others) %in% c("data", "weights", "nrounds", "num_class", names(arg_list)))]
  if (length(others) > 0)
    for (i in names(others))
      call[[i]] <- others[[i]]

  eval_tidy(call, env = current_env())
}

#' @export
xgb_pred <- function(object, newdata, ...) {
  if (!inherits(newdata, "xgb.DMatrix")) {
    newdata <- as.matrix(newdata)
    newdata <- xgboost::xgb.DMatrix(data = newdata, missing = NA)
  }

  res <- predict(object, newdata, ...)

  x = switch(
    object$params$objective,
    "reg:linear" =, "reg:logistic" =, "binary:logistic" = res,
    "binary:logitraw" = binomial()$linkinv(res),
    "multi:softprob" = matrix(res, ncol = object$params$num_class, byrow = TRUE),
    res
  )
  x
}


###################################################################


boost_tree_xgboost_fit <-
  list(
    libs = "xgboost",
    fit = list(
      interface = "matrix",
      protect = c("data", "weight"),
      func = c(pkg = "parsnip", fun = "xgb_train"),
      alternates =
        list(
          nthread = 1,
          verbose = 0
        )
    ),
    pred = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "xgb_pred"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(newdata)
        )
    ),
    classes = list(
      pre = NULL,
      post = function(x, object) {
        if (is.vector(x)) {
          x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
        } else {
          x <- object$lvl[apply(x, 1, which.max)]
        }
        x
      },
      func = c(pkg = "parsnip", fun = "xgb_pred"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(newdata)
        )
    ),
    prob = list(
      pre = NULL,
      post = function(x, object) {
        if (is.vector(x)) {
          x <- tibble(v1 = 1 - x, v2 = x)
        } else {
          x <- as_tibble(x)
        }
        colnames(x) <- object$lvl
        x
      },
      func = c(pkg = "parsnip", fun = "xgb_pred"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(newdata)
        )
    )
  )

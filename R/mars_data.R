
set_new_model("mars")

set_model_mode("mars", "classification")
set_model_mode("mars", "regression")

# ------------------------------------------------------------------------------

set_model_engine("mars", "classification", "earth")
set_model_engine("mars", "regression", "earth")
set_dependency("mars", "earth", "earth")

set_model_arg(
  mod = "mars",
  eng = "earth",
  val = "num_terms",
  original = "nprune",
  func = list(pkg = "dials", fun = "num_terms"),
  submodels = FALSE
)
set_model_arg(
  mod = "mars",
  eng = "earth",
  val = "prod_degree",
  original = "degree",
  func = list(pkg = "dials", fun = "prod_degree"),
  submodels = FALSE
)
set_model_arg(
  mod = "mars",
  eng = "earth",
  val = "prune_method",
  original = "pmethod",
  func = list(pkg = "dials", fun = "prune_method"),
  submodels = FALSE
)

set_fit(
  mod = "mars",
  eng = "earth",
  mode = "regression",
  value = list(
    interface = "data.frame",
    protect = c("x", "y", "weights"),
    func = c(pkg = "earth", fun = "earth"),
    defaults = list(keepxy = TRUE)
  )
)

set_fit(
  mod = "mars",
  eng = "earth",
  mode = "classification",
  value = list(
    interface = "data.frame",
    protect = c("x", "y", "weights"),
    func = c(pkg = "earth", fun = "earth"),
    defaults = list(keepxy = TRUE)
  )
)

set_pred(
  mod = "mars",
  eng = "earth",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = maybe_multivariate,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
  )
)

set_pred(
  mod = "mars",
  eng = "earth",
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
  mod = "mars",
  eng = "earth",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = function(x, object) {
      x <- ifelse(x[, 1] >= 0.5, object$lvl[2], object$lvl[1])
      x
    },
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
  )
)

set_pred(
  mod = "mars",
  eng = "earth",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      x <- x[, 1]
      x <- tibble(v1 = 1 - x, v2 = x)
      colnames(x) <- object$lvl
      x
    },
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
  )
)

set_pred(
  mod = "mars",
  eng = "earth",
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

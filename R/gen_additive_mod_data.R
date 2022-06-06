
set_new_model("gen_additive_mod")
set_model_mode("gen_additive_mod", "classification")
set_model_mode("gen_additive_mod", "regression")

# ------------------------------------------------------------------------------
#### REGRESION ----
set_model_engine(model = "gen_additive_mod", mode = "regression", eng = "mgcv")
set_dependency(model = "gen_additive_mod", eng = "mgcv", pkg = "mgcv", mode = "regression")

#Args

# TODO make dials PR
set_model_arg(
  model        = "gen_additive_mod",
  eng          = "mgcv",
  parsnip      = "select_features",
  original     = "select",
  func         = list(pkg = "dials", fun = "select_features"),
  has_submodel = FALSE
)

set_model_arg(
  model        = "gen_additive_mod",
  eng          = "mgcv",
  parsnip      = "adjust_deg_free",
  original     = "gamma",
  func         = list(pkg = "dials", fun = "adjust_deg_free"),
  has_submodel = FALSE
)

set_encoding(
  model = "gen_additive_mod",
  eng   = "mgcv",
  mode  = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept    = FALSE,
    remove_intercept     = FALSE,
    allow_sparse_x       = FALSE
  )
)

set_fit(
  model = "gen_additive_mod",
  eng = "mgcv",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "mgcv", fun = "gam"),
    defaults = list()
  )
)

set_pred(
  model  = "gen_additive_mod",
  eng    = "mgcv",
  mode   = "regression",
  type   = "numeric",
  value  = list(
    pre  = NULL,
    post = function(x, object) as.numeric(x),
    func = c(fun = "predict"),
    args = list(
      object = rlang::expr(object$fit),
      newdata = rlang::expr(new_data),
      type = "response"
    )
  )
)

set_pred(
  model  = "gen_additive_mod",
  eng    = "mgcv",
  mode   = "regression",
  type   = "conf_int",
  value  = list(
    pre  = NULL,
    post = linear_lp_to_conf_int,
    func = c(fun = "predict"),
    args = list(
      object = rlang::expr(object$fit),
      newdata = rlang::expr(new_data),
      type = "link",
      se.fit = TRUE
    )
  )
)

set_pred(
  model  = "gen_additive_mod",
  eng    = "mgcv",
  mode   = "regression",
  type   = "raw",
  value  = list(
    pre  = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(
      object = rlang::expr(object$fit),
      newdata = rlang::expr(new_data)
    )
  )
)

# ------------------------------------------------------------------------------
#### CLASSIFICATION
set_model_engine(model = "gen_additive_mod", mode = "classification", eng = "mgcv")
set_dependency(model = "gen_additive_mod", eng = "mgcv", pkg = "mgcv", mode = "classification")

set_encoding(
  model = "gen_additive_mod",
  eng   = "mgcv",
  mode  = "classification",
  options = list(
    predictor_indicators = "none",
    compute_intercept    = FALSE,
    remove_intercept     = FALSE,
    allow_sparse_x       = FALSE
  )
)

set_fit(
  model = "gen_additive_mod",
  eng = "mgcv",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "mgcv", fun = "gam"),
    defaults = list(
      family = quote(stats::binomial(link = "logit"))
    )
  )
)

set_pred(
  model  = "gen_additive_mod",
  eng    = "mgcv",
  mode   = "classification",
  type   = "class",
  value  = list(
    pre  = NULL,
    post = function(x, object) {
      if (is.array(x)) {
        x <- as.vector(x)
      }
      x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
      unname(x)
    },
    func = c(fun = "predict"),
    args = list(
      object = rlang::expr(object$fit),
      newdata = rlang::expr(new_data),
      type = "response"
    )
  )
)

set_pred(
  model  = "gen_additive_mod",
  eng    = "mgcv",
  mode   = "classification",
  type   = "prob",
  value  = list(
    pre = NULL,
    post = function(x, object) {
      if (is.array(x)) {
        x <- as.vector(x)
      }
      x <- tibble(v1 = 1 - x, v2 = x)
      colnames(x) <- object$lvl
      x
    },
    func = c(fun = "predict"),
    args = list(
      object = rlang::expr(object$fit),
      newdata = rlang::expr(new_data),
      type = "response"
    )
  )
)

set_pred(
  model  = "gen_additive_mod",
  eng    = "mgcv",
  mode   = "classification",
  type   = "raw",
  value  = list(
    pre  = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(
      object = rlang::expr(object$fit),
      newdata = rlang::expr(new_data)
    )
  )
)


set_pred(
  model = "gen_additive_mod",
  eng = "mgcv",
  mode = "classification",
  type = "conf_int",
  value = list(
    pre = NULL,
    post = logistic_lp_to_conf_int,
    func = c(fun = "predict"),
    args =
      list(
        object = rlang::expr(object$fit),
        newdata = rlang::expr(new_data),
        type = "link",
        se.fit = TRUE
      )
  )
)


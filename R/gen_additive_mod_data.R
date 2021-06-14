
set_new_model("gen_additive_mod")

# ------------------------------------------------------------------------------
#### REGRESION ----
set_model_engine(model = "gen_additive_mod", mode = "regression", eng = "mgcv")
set_dependency(model = "gen_additive_mod", eng = "mgcv", pkg = "mgcv")

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
    protect = c("formula", "data"),
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
    post = function(results, object) {
      hf_lvl <- (1 - object$spec$method$pred$conf_int$extras$level)/2
      const <-
        qt(hf_lvl, df = object$fit$df.residual, lower.tail = FALSE)
      trans <- object$fit$family$linkinv
      res <-
        tibble(
          .pred_lower = trans(results$fit - const * results$se.fit),
          .pred_upper = trans(results$fit + const * results$se.fit)
        )
      # In case of inverse or other links
      if (any(res$.pred_upper < res$.pred_lower)) {
        nms <- names(res)
        res <- res[, 2:1]
        names(res) <- nms
      }

      if (object$spec$method$pred$conf_int$extras$std_error) {
        res$.std_error <- results$se.fit
      }
      res
    },
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
set_dependency(model = "gen_additive_mod", eng = "mgcv", pkg = "mgcv")


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
    protect = c("formula", "data"),
    func = c(pkg = "mgcv", fun = "gam"),
    defaults = list(
      family = stats::binomial(link = "logit")
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
    post = function(results, object) {
      hf_lvl <- (1 - object$spec$method$pred$conf_int$extras$level)/2
      const <-
        qt(hf_lvl, df = object$fit$df.residual, lower.tail = FALSE)
      trans <- object$fit$family$linkinv
      res_2 <-
        tibble(
          lo = trans(results$fit - const * results$se.fit),
          hi = trans(results$fit + const * results$se.fit)
        )
      res_1 <- res_2
      res_1$lo <- 1 - res_2$hi
      res_1$hi <- 1 - res_2$lo
      lo_nms <- paste0(".pred_lower_", object$lvl)
      hi_nms <- paste0(".pred_upper_", object$lvl)
      colnames(res_1) <- c(lo_nms[1], hi_nms[1])
      colnames(res_2) <- c(lo_nms[2], hi_nms[2])
      res <- bind_cols(res_1, res_2)

      if (object$spec$method$pred$conf_int$extras$std_error) {
        res$.std_error <- results$se.fit
      }
      res
    },
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



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
      # TODO fix this; see the logistic regression code
      res <-tibble::tibble(.pred_lower = results$fit - 2*results$se.fit,
                           .pred_upper = results$fit + 2*results$se.fit)
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
    post = function(results, object) {

      tbl <-tibble::as_tibble(results)

      if (ncol(tbl) == 1) {
        res <- prob_to_class_2(tbl, object) %>%
          tibble::as_tibble() %>%
          stats::setNames("values") %>%
          dplyr::mutate(values = as.factor(values))
      } else{
        res <- tbl %>%
          apply(., 1, function(x)
            which(max(x) == x)[1]) - 1 %>% #modify in the future for something more elegant when gets the formula ok
          tibble::as_tibble()
      }

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
    pre  = NULL,
    post = function(results, object) {
      res <- tibble::as_tibble(results)
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




set_new_model("bart")

set_model_mode("bart", "classification")
set_model_mode("bart", "regression")

# ------------------------------------------------------------------------------

set_model_engine("bart", "classification", "dbarts")
set_model_engine("bart", "regression", "dbarts")
set_dependency("bart", "dbarts", "dbarts")

set_model_arg(
  model = "bart",
  eng = "dbarts",
  parsnip = "trees",
  original = "ntree",
  func = list(pkg = "dials", fun = "trees", range = c(50, 500)),
  has_submodel = FALSE
)
set_model_arg(
  model = "bart",
  eng = "dbarts",
  parsnip = "prior_terminal_node_coef",
  original = "base",
  func = list(pkg = "dials", fun = "prior_terminal_node_coef"),
  has_submodel = FALSE
)
set_model_arg(
  model = "bart",
  eng = "dbarts",
  parsnip = "prior_terminal_node_expo",
  original = "power",
  func = list(pkg = "dials", fun = "prior_terminal_node_expo"),
  has_submodel = FALSE
)
set_model_arg(
  model = "bart",
  eng = "dbarts",
  parsnip = "prior_outcome_range",
  original = "k",
  func = list(pkg = "dials", fun = "prior_outcome_range"),
  has_submodel = FALSE
)
set_fit(
  model = "bart",
  eng = "dbarts",
  mode = "regression",
  value = list(
    interface = "data.frame",
    data = c(x = "x.train", y = "y.train"),
    protect = c("x", "y"),
    func = c(pkg = "dbarts", fun = "bart"),
    defaults = list(verbose = FALSE, keeptrees = TRUE, keepcall = FALSE)
  )
)

set_encoding(
  model = "bart",
  eng = "dbarts",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_fit(
  model = "bart",
  eng = "dbarts",
  mode = "classification",
  value = list(
    interface = "data.frame",
    data = c(x = "x.train", y = "y.train"),
    protect = c("x", "y"),
    func = c(pkg = "dbarts", fun = "bart"),
    defaults = list(verbose = FALSE, keeptrees = TRUE, keepcall = FALSE)
  )
)

set_encoding(
  model = "bart",
  eng = "dbarts",
  mode = "classification",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "bart",
  eng = "dbarts",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = "parsnip", fun = "dbart_predict_calc"),
    args =
      list(
        obj = quote(object),
        new_data =  quote(new_data),
        type = "numeric"
      )
  )
)

set_pred(
  model = "bart",
  eng = "dbarts",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = "parsnip", fun = "dbart_predict_calc"),
    args =
      list(obj = quote(object),
           new_data =  quote(new_data))
  )
)


set_pred(
  model = "bart",
  eng = "dbarts",
  mode = "regression",
  type = "conf_int",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = "parsnip", fun = "dbart_predict_calc"),
    args =
      list(
        obj = expr(object),
        new_data = expr(new_data),
        type = "conf_int",
        level = expr(level),
        std_err = expr(std_error)
      )
  )
)
set_pred(
  model = "bart",
  eng = "dbarts",
  mode = "regression",
  type = "pred_int",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = "parsnip", fun = "dbart_predict_calc"),
    args =
      list(
        obj = expr(object),
        new_data = expr(new_data),
        type = "pred_int",
        level = expr(level),
        std_err = expr(std_error)
      )
  )
)


set_pred(
  model = "bart",
  eng = "dbarts",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = "parsnip", fun = "dbart_predict_calc"),
    args =
      list(
        obj = quote(object),
        new_data =  quote(new_data),
        type = "class"
      )
  )
)

set_pred(
  model = "bart",
  eng = "dbarts",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = "parsnip", fun = "dbart_predict_calc"),
    args =
      list(
        obj = quote(object),
        new_data =  quote(new_data),
        type = "prob"
      )
  )
)


set_pred(
  model = "bart",
  eng = "dbarts",
  mode = "classification",
  type = "conf_int",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = "parsnip", fun = "dbart_predict_calc"),
    args =
      list(
        obj = expr(object),
        new_data = expr(new_data),
        type = "conf_int",
        level = expr(level),
        std_err = expr(std_error)
      )
  )
)
set_pred(
  model = "bart",
  eng = "dbarts",
  mode = "classification",
  type = "pred_int",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = "parsnip", fun = "dbart_predict_calc"),
    args =
      list(
        obj = expr(object),
        new_data = expr(new_data),
        type = "pred_int",
        level = expr(level),
        std_err = expr(std_error)
      )
  )
)

set_pred(
  model = "bart",
  eng = "dbarts",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = "parsnip", fun = "dbart_predict_calc"),
    args =
      list(
        obj = quote(object),
        new_data =  quote(new_data)
      )
  )
)

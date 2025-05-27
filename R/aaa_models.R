# Initialize model environments

all_modes <- c("classification", "regression", "censored regression", "quantile regression")

# ------------------------------------------------------------------------------

## Rules about model-related information

### Definitions:

# - the model is the model type (e.g. "rand_forest", "linear_reg", etc)
# - the model's mode is the species of model such as "classification" or "regression"
# - the engines are within a model and mode and describe the method/implementation
#   of the model in question. These are often R package names.

### The package dependencies are model- and engine-specific. They are used across modes

### The `fit` information is a list of data that is needed to fit the model. This
### information is specific to an engine and mode.

### The `predict` information is also list of data that is needed to make some sort
### of prediction on the model object. The possible types are contained in `pred_types`
### and this information is specific to the engine, mode, and type (although there
### are no types across different modes).

# ------------------------------------------------------------------------------

parsnip <- rlang::new_environment()
parsnip$models <- NULL
parsnip$modes <- c(all_modes, "unknown")

# ------------------------------------------------------------------------------

pred_types <-
  c("raw", "numeric", "class", "prob", "conf_int", "pred_int", "quantile",
    "time", "survival", "linear_pred", "hazard")

# ------------------------------------------------------------------------------

#' Working with the parsnip model environment
#'
#' These functions read and write to the environment where the package stores
#'  information about model specifications.
#'
#' @param items A character string of objects in the model environment.
#' @param ... Named values that will be assigned to the model environment.
#' @param name A single character value for a new symbol in the model environment.
#' @param value A single value for a new value in the model environment.
#' @keywords internal
#' @references "How to build a parsnip model"
#'  \url{https://www.tidymodels.org/learn/develop/models/}
#' @examplesIf !parsnip:::is_cran_check()
#' # Access the model data:
#' current_code <- get_model_env()
#' ls(envir = current_code)
#'
#' @keywords internal
#' @export
get_model_env <- function() {
  current <- utils::getFromNamespace("parsnip", ns = "parsnip")
  current
}

#' @rdname get_model_env
#' @keywords internal
#' @export
get_from_env <- function(items) {
  check_character(items)
  mod_env <- get_model_env()
  rlang::env_get(mod_env, items, default = NULL)
}

#' @rdname get_model_env
#' @keywords internal
#' @export
set_in_env <- function(...) {
  mod_env <- get_model_env()
  rlang::env_bind(mod_env, ...)
}

#' @rdname get_model_env
#' @keywords internal
#' @export
set_env_val <- function(name, value) {
  check_string(name, allow_empty = FALSE)
  mod_env <- get_model_env()
  x <- list(value)
  names(x) <- name
  rlang::env_bind(mod_env, !!!x)
}

# ------------------------------------------------------------------------------

error_set_object <- function(object, func) {
  msg <-
    "`{func}()` expected a model specification to be supplied to the \\
     `object` argument, but received a(n) `{class(object)[1]}` object."

  if (inherits(object, "function") &&
      isTRUE(environment(object)$.packageName == "parsnip")) {
    msg <- c(
      msg,
      "i" = "Did you mistakenly pass `model_function` rather than `model_function()`?"
    )
  }

  cli::cli_abort(msg, call = call2(func))
}

check_eng_val <- function(eng, call = caller_env()) {
  check_string(eng, allow_empty = FALSE, call = call)
  invisible(NULL)
}

check_model_exists <- function(model, call = caller_env()) {
  check_string(model, allow_empty = FALSE, call = call)

  current <- get_model_env()

  if (!any(current$models == model)) {
    cli::cli_abort(
      "Model {.val {model}} has not been registered.",
      call = call
    )
  }

  invisible(NULL)
}

check_model_doesnt_exist <- function(model, call = caller_env()) {
  check_string(model, allow_empty = FALSE, call = call)

  current <- get_model_env()

  if (any(current$models == model)) {
    cli::cli_abort(
      "Model {.val {model}} already exists.",
      call = call
    )
  }

  invisible(NULL)
}

check_mode_val <- function(mode, call = caller_env()) {
  check_string(mode, allow_empty = FALSE, call = call)
  invisible(NULL)
}


stop_incompatible_mode <- function(spec_modes, eng = NULL, cls = NULL, call) {
  if (is.null(eng) & is.null(cls)) {
    msg <- "Available modes are:"
  }
  if (!is.null(eng) & is.null(cls)) {
    msg <- "Available modes for engine {eng} are:"
  }
  if (is.null(eng) & !is.null(cls)) {
    msg <- "Available modes for model type {cls} are:"
  }
  if (!is.null(eng) & !is.null(cls)) {
    msg <- "Available modes for model type {cls} with engine {eng} are:"
  }

  cli::cli_abort(paste(msg, "{.val {spec_modes}}."), call = call)
}

stop_incompatible_engine <- function(spec_engs, mode, call) {
  cli::cli_abort(
    "Available engines for mode {.val {mode}} are: {.val {spec_engs}}.",
    call = call
  )
}

stop_missing_engine <- function(cls, call) {
  info <-
    get_from_env(cls) |>
    dplyr::group_by(mode) |>
    dplyr::summarize(msg = paste0(unique(mode), " {",
                                  paste0(unique(engine), collapse = ", "),
                                  "}"),
                     .groups = "drop")
  if (nrow(info) == 0) {
    cli::cli_abort("No known engines for {.fn {cls}}.", call = call)
  }
  cli::cli_abort(
    "Missing engine. Possible mode/engine combinations are: {info$msg}.",
    call = call
  )
}

check_mode_for_new_engine <- function(cls, eng, mode, call = caller_env()) {
  model_modes <- get_from_env(paste0(cls, "_modes"))
  if (!(mode %in% model_modes)) {
    cli::cli_abort(
      "{.val {mode}} is not a known mode for model {.fn {cls}}.",
      call = call
    )
  }
  invisible(NULL)
}


# check if class and mode and engine are compatible
check_spec_mode_engine_val <- function(cls, eng, mode, call = caller_env()) {

  all_modes <- get_from_env(paste0(cls, "_modes"))
  if (!(mode %in% all_modes)) {
    cli::cli_abort(
      "{.val {mode}} is not a known mode for model {.fn {cls}}.",
      call = call
    )
  }

  model_info <- rlang::env_get(get_model_env(), cls)

  # Initially, check if the specification is well-defined in the current model
  # parsnip model environment. If so, return early.
  # If not, troubleshoot more precisely and raise a relevant error.
  model_env_match <-
    vctrs::vec_slice(model_info, model_info$engine == eng & model_info$mode == mode)

  if (vctrs::vec_size(model_env_match) == 1) {
    return(invisible(NULL))
  }

  # Cases where the model definition is in parsnip but all of the engines
  # are contained in a different package
  model_info_parsnip_only <-
    dplyr::inner_join(
      model_info_table |> dplyr::filter(is.na(pkg)) |> dplyr::select(-pkg),
      model_info |> dplyr::mutate(model = cls),
      by = c("model", "engine", "mode")
    )

  if (nrow(model_info_parsnip_only) == 0) {
    check_mode_with_no_engine(cls, mode, call = call)
    return(invisible(NULL))
  }

  # ------------------------------------------------------------------------------
  # First check engine against any mode for the given model class

  spec_engs <- model_info$engine
  # engine is allowed to be NULL
  if (!is.null(eng) && !(eng %in% spec_engs)) {
    cli::cli_abort(
      c(
        x = "Engine {.val {eng}} is not supported for {.fn {cls}}",
        i = "See {.code show_engines({.val {cls}})}."
      ),
      call = call
    )
  }

  # ----------------------------------------------------------------------------
  # Check modes based on model and engine

  spec_modes <- model_info$mode
  if (!is.null(eng)) {
    spec_modes <- spec_modes[model_info$engine == eng]
  }
  spec_modes <- unique(c("unknown", spec_modes))

  if (is.null(mode) || length(mode) > 1) {
    stop_incompatible_mode(spec_modes, eng, call = call)
  } else if (!(mode %in% spec_modes)) {
    stop_incompatible_mode(spec_modes, eng, call = call)
  }

  # ----------------------------------------------------------------------------
  # Check engine based on model and model

  # How check for compatibility with the chosen mode (if any)
  if (!is.null(mode) && mode != "unknown") {
    spec_engs <- spec_engs[model_info$mode == mode]
  }
  spec_engs <- unique(spec_engs)
  if (!is.null(eng) && !(eng %in% spec_engs)) {
    stop_incompatible_engine(spec_engs, mode, call = call)
  }

  invisible(NULL)
}

check_mode_with_no_engine <- function(cls, mode, call) {
  spec_modes <- get_from_env(paste0(cls, "_modes"))
  if (!(mode %in% spec_modes)) {
    stop_incompatible_mode(spec_modes, cls = cls, call = call)
  }
}

check_func_val <- function(func, call = caller_env()) {
  msg <- "{.arg func} should be a named vector with element {.field fun} and \\
          the optional elements {.field pkg}, {.field range}, {.field trans}, \\
          and {.field values}. {.field func} and {.field pkg} should both be \\
          single character strings."

  if (rlang::is_missing(func) || !is.vector(func)) {
    cli::cli_abort(msg, call = call)
  }

  nms <- sort(names(func))

  if (all(is.null(nms)))  {
    cli::cli_abort(msg, call = call)
  }

  if (length(func) == 1) {
    if (isTRUE(any(nms != "fun"))) {
      cli::cli_abort(msg, call = call)
    }
  } else {
    # check for extra names:
    allow_nms <- c("fun", "pkg", "range", "trans", "values")
    nm_check <- nms %in% c("fun", "pkg", "range", "trans", "values")
    not_allowed <- nms[!(nms %in% allow_nms)]
    if (length(not_allowed) > 0) {
      cli::cli_abort(msg, call = call)
    }
  }

  check_string(
    func[["fun"]],
    allow_empty = FALSE,
    arg = I("The `fun` element of `func`"),
    call = call
  )
  if (any(nms == "pkg")) {
    check_string(
      func[["pkg"]],
      allow_null = TRUE,
      arg = I("The `pkg` element of `func`"),
      call = call
    )
  }

  invisible(NULL)
}

check_fit_info <- function(fit_obj, call = caller_env()) {
  if (is.null(fit_obj)) {
    cli::cli_abort("The {.arg fit_obj} argument cannot be NULL.", call = call)
  }

  # check required data elements
  exp_nms <- c("defaults", "func", "interface", "protect")
  has_req_nms <- exp_nms %in% names(fit_obj)

  if (!all(has_req_nms)) {
    cli::cli_abort(
      "The {.arg value} argument should have elements: {.field {exp_nms}}.",
      call = call
    )
  }

  # check optional data elements
  opt_nms <- c("data")
  other_nms <- setdiff(exp_nms, names(fit_obj))
  has_opt_nms <- other_nms %in% opt_nms
  if (any(!has_opt_nms)) {
    cli::cli_abort(
      "The {.arg value} argument can only have optional elements: \\
      {.field {exp_nms}}.",
      call = call
    )
  }
  if (any(other_nms == "data")) {
    data_nms <- names(fit_obj$data)
    if (length(data_nms == 0) || any(data_nms == "")) {
      cli::cli_abort(
        "All elements of the {.field data} argument vector must be named.",
        call = call
      )
    }
  }

  check_interface_val(fit_obj$interface)
  check_func_val(fit_obj$func)

  if (!is.list(fit_obj$defaults)) {
    cli::cli_abort("The {.field defaults} element should be a list.", call = call)
  }

  invisible(NULL)
}

check_pred_info <- function(pred_obj, type, call = caller_env()) {
  if (all(type != pred_types)) {
    cli::cli_abort(
      "The prediction type should be one of: {.val {pred_types}}.",
      call = call
    )
  }

  exp_nms <- c("args", "func", "post", "pre")
  if (!isTRUE(all.equal(sort(names(pred_obj)), exp_nms))) {
    cli::cli_abort(
      "The {.field predict} module should have elements: {.val {exp_nms}}.",
      call = call
    )
  }

  check_function(
    pred_obj$pre,
    allow_null = TRUE,
    arg = I("The `pre` element of `pred_obj`"),
    call = call
  )

  check_function(
    pred_obj$post,
    allow_null = TRUE,
    arg = I("The `post` element of `pred_obj`"),
    call = call
  )

  check_func_val(pred_obj$func)

  if (!is.list(pred_obj$args)) {
    cli::cli_abort(
      "The {.field args} element should be a list.",
      call = call
    )
  }

  invisible(NULL)
}

spec_has_pred_type <- function(object, type) {
  possible_preds <- names(object$spec$method$pred)
  any(possible_preds == type)
}

check_spec_pred_type <- function(object, type, call = caller_env()) {
  if (!spec_has_pred_type(object, type)) {
    possible_preds <- names(object$spec$method$pred)
    cli::cli_abort(
      "No {.val {type}} prediction method available for this model. \\
      {.arg type} should be one of: {.val {possible_preds}}.",
      call = call
    )
  }
  invisible(NULL)
}

check_interface_val <- function(x, call = call) {
  exp_interf <- c("data.frame", "formula", "matrix")
  if (length(x) != 1 || !(x %in% exp_interf)) {
    cli::cli_abort(
      "The {.field interface} element should have a single of: {exp_interf}.",
      call = call
    )
  }
  invisible(NULL)
}


# ------------------------------------------------------------------------------

#' Tools to Register Models
#'
#' These functions are similar to constructors and can be used to validate
#'  that there are no conflicts with the underlying model structures used by the
#'  package.
#'
#' @param model A single character string for the model type (e.g.
#'  `"rand_forest"`, etc).
#' @param mode A single character string for the model mode (e.g. "regression").
#' @param eng A single character string for the model engine.
#' @param has_submodel A single logical for whether the argument
#'  can make predictions on multiple submodels at once.
#' @param func A named character vector that describes how to call
#'  a function. `func` should have elements `pkg` and `fun`. The
#'  former is optional but is recommended and the latter is
#'  required. For example, `c(pkg = "stats", fun = "lm")` would be
#'  used to invoke the usual linear regression function. In some
#'  cases, it is helpful to use `c(fun = "predict")` when using a
#'  package's `predict` method.
#' @param type A single character value for the type of prediction. Possible
#'  values are: `class`, `conf_int`, `numeric`, `pred_int`, `prob`, `quantile`,
#'   and `raw`.
#' @param pkg An options character string for a package name.
#' @param parsnip A single character string for the "harmonized" argument name
#'  that parsnip exposes.
#' @param original A single character string for the argument name that
#'  underlying model function uses.
#' @param value A list that conforms to the `fit_obj` or `pred_obj` description
#'  below, depending on context.
#' @param pre,post Optional functions for pre- and post-processing of prediction
#'  results.
#' @param options A list of options for engine-specific preprocessing encodings.
#'  See Details below.
#' @param ... Optional arguments that should be passed into the `args` slot for
#'  prediction objects.
#' @keywords internal
#' @details These functions are available for users to add their
#'  own models or engines (in a package or otherwise) so that they can
#'  be accessed using parsnip. This is more thoroughly documented
#'  on the package web site (see references below).
#'
#' In short, `parsnip` stores an environment object that contains
#'  all of the information and code about how models are used (e.g.
#'  fitting, predicting, etc). These functions can be used to add
#'  models to that environment as well as helper functions that can
#'  be used to makes sure that the model data is in the right
#'  format.
#'
#' `check_model_exists()` checks the model value and ensures that the model has
#'  already been registered. `check_model_doesnt_exist()` checks the model value
#'  and also checks to see if it is novel in the environment.
#'
#'  The options for engine-specific encodings dictate how the predictors should be
#'  handled. These options ensure that the data
#'  that `parsnip` gives to the underlying model allows for a model fit that is
#'  as similar as possible to what it would have produced directly.
#'
#'  For example, if `fit()` is used to fit a model that does not have
#'  a formula interface, typically some predictor preprocessing must
#'  be conducted. `glmnet` is a good example of this.
#'
#'   There are four options that can be used for the encodings:
#'
#'  `predictor_indicators` describes whether and how to create indicator/dummy
#'  variables from factor predictors. There are three options: `"none"` (do not
#'  expand factor predictors), `"traditional"` (apply the standard
#'  `model.matrix()` encodings), and `"one_hot"` (create the complete set
#'  including the baseline level for all factors). This encoding only affects
#'  cases when [fit.model_spec()] is used and the underlying model has an x/y
#'  interface.
#'
#' Another option is `compute_intercept`; this controls whether `model.matrix()`
#'  should include the intercept in its formula. This affects more than the
#'  inclusion of an intercept column. With an intercept, `model.matrix()`
#'  computes dummy variables for all but one factor levels. Without an
#'  intercept, `model.matrix()` computes a full set of indicators for the
#'  _first_ factor variable, but an incomplete set for the remainder.
#'
#'  Next, the option `remove_intercept` will remove the intercept column
#'  _after_ `model.matrix()` is finished. This can be useful if the model
#'  function (e.g. `lm()`) automatically generates an intercept.
#'
#' Finally, `allow_sparse_x` specifies whether the model function can natively
#'  accommodate a sparse matrix representation for predictors during fitting
#'  and tuning.
#'
#'
#' @references "How to build a parsnip model"
#'  \url{https://www.tidymodels.org/learn/develop/models/}
#' @examplesIf !parsnip:::is_cran_check()
#' # set_new_model("shallow_learning_model")
#'
#' # Show the information about a model:
#' show_model_info("rand_forest")
#' @keywords internal
#' @export
set_new_model <- function(model) {
  check_model_doesnt_exist(model)

  current <- get_model_env()

  set_env_val("models", unique(c(current$models, model)))
  set_env_val(model, tibble::new_tibble(list(engine = character(0), mode = character(0))))
  set_env_val(
    paste0(model, "_pkgs"),
    tibble::new_tibble(list(engine = character(0), pkg = list(), mode = character(0)))
  )
  set_env_val(paste0(model, "_modes"), "unknown")
  set_env_val(
    paste0(model, "_args"),
    tibble::new_tibble(list(
      engine = character(0),
      parsnip = character(0),
      original = character(0),
      func = list(),
      has_submodel = logical(0)
    ))
  )
  set_env_val(
    paste0(model, "_fit"),
    tibble::new_tibble(list(
      engine = character(0),
      mode = character(0),
      value = list()
    ))
  )
  set_env_val(
    paste0(model, "_predict"),
    tibble::new_tibble(list(
      engine = character(0),
      mode = character(0),
      type = character(0),
      value = list()
    ))
  )

  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
set_model_mode <- function(model, mode) {
  check_model_exists(model)
  check_mode_val(mode)

  current <- get_model_env()

  if (!any(current$modes == mode)) {
    current$modes <- unique(c(current$modes, mode))
  }

  set_env_val(
    paste0(model, "_modes"),
    unique(c(get_from_env(paste0(model, "_modes")), mode))
  )
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
set_model_engine <- function(model, mode, eng) {
  check_model_exists(model)
  check_mode_val(mode)
  check_eng_val(eng)
  check_mode_for_new_engine(model, eng, mode)

  new_eng <- tibble::new_tibble(list(engine = eng, mode = mode), nrow = 1)
  old_eng <- get_from_env(model)

  engs <-
    old_eng |>
    dplyr::bind_rows(new_eng) |>
    dplyr::distinct()

  set_env_val(model, engs)
  set_model_mode(model, mode)
  invisible(NULL)
}


# ------------------------------------------------------------------------------
#' @rdname set_new_model
#' @keywords internal
#' @export
set_model_arg <- function(model, eng, parsnip, original, func, has_submodel) {
  check_model_exists(model)
  check_eng_val(eng)
  check_string(parsnip, allow_empty = FALSE)
  check_string(original, allow_empty = FALSE)
  check_func_val(func)
  check_bool(has_submodel)

  old_args <- get_from_env(paste0(model, "_args"))

  new_arg <-
    tibble::new_tibble(list(
      engine = eng,
      parsnip = parsnip,
      original = original,
      func = list(func),
      has_submodel = has_submodel
    ), nrow = 1)

  updated <- try(dplyr::bind_rows(old_args, new_arg), silent = TRUE)
  if (inherits(updated, "try-error")) {
    cli::cli_abort(
      "An error occurred when adding the new argument.",
      parent = updated
    )
  }

  updated <- vctrs::vec_unique(updated)
  set_env_val(paste0(model, "_args"), updated)

  invisible(NULL)
}


# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
set_dependency <- function(model, eng, pkg = "parsnip", mode = NULL) {
  check_model_exists(model)
  check_eng_val(eng)
  check_string(pkg, allow_empty = FALSE)

  model_info <- get_from_env(model)
  pkg_info <- get_from_env(paste0(model, "_pkgs"))

  # ----------------------------------------------------------------------------
  # Check engine

  has_engine <-
    model_info |>
    dplyr::distinct(engine) |>
    dplyr::filter(engine == eng) |>
    nrow()
  if (has_engine != 1) {
    cli::cli_abort(
      "The engine {.val {eng}} has not been registered for model \\
      {.val {model}}."
    )
  }

  # ----------------------------------------------------------------------------
  # check mode; if missing assign all modes
  all_modes <- unique(model_info$mode[model_info$engine == eng])
  if (is.null(mode)) {
    # For backward compatibility
    mode <- all_modes
  } else {
    if (length(mode) > 1) {
      cli::cli_abort("{.arg mode} should be a single character value or NULL.")
    }
    if (!any(mode == all_modes)) {
      cli::cli_abort(
        "mode {.val {mode}} is not a valid mode for {.val {model}}."
      )
    }
  }

  # ----------------------------------------------------------------------------

  new_pkgs <- tibble(engine = eng, pkg = list(pkg), mode = mode)

  # Add the new entry to the existing list for this engine (if any) and
  # keep unique results
  eng_pkgs <-
    pkg_info |>
    dplyr::filter(engine == eng) |>
    dplyr::bind_rows(new_pkgs) |>
    # Take unique combinations in case packages have alread been registered
    dplyr::distinct() |>
    # In case there are existing results (in a list column pkg), aggregate the
    # list results and re-list their unique values.
    dplyr::group_by(mode, engine) |>
    dplyr::summarize(pkg = list(unique(unlist(pkg))), .groups = "drop") |>
    dplyr::select(engine, pkg, mode)

  pkg_info <-
    pkg_info |>
    dplyr::filter(engine != eng) |>
    dplyr::bind_rows(eng_pkgs) |>
    dplyr::arrange(engine, mode)

  set_env_val(paste0(model, "_pkgs"), pkg_info)

  invisible(NULL)
}

#' @rdname set_new_model
#' @keywords internal
#' @export
get_dependency <- function(model) {
  check_model_exists(model)
  pkg_name <- paste0(model, "_pkgs")
  if (!any(pkg_name != rlang::env_names(get_model_env()))) {
    cli::cli_abort("{.val {model}} does not have a dependency list in parsnip.")
  }
  rlang::env_get(get_model_env(), pkg_name)
}


# ------------------------------------------------------------------------------

# This will be used to see if the same information is being registered for the
# same model/mode/engine (and prediction type). If it already exists and the
# new information is different, fail with a message. See issue #653
is_discordant_info <- function(model, mode, eng, candidate,
                            pred_type = NULL, component = "fit",
                            call = caller_env()) {
  current <- get_from_env(paste0(model, "_", component))

  # For older versions of parsnip before set_encoding()
  new_encoding <- is.null(current) & component == "encoding"

  if (new_encoding) {
    return(TRUE)
  } else {
    current <-  dplyr::filter(current, engine == eng & mode == !!mode)
  }

  if (component == "predict" & !is.null(pred_type)) {

    current <- dplyr::filter(current, type == pred_type)
    p_type <- "and prediction type {.val {pred_type}} "
  } else {
    p_type <- ""
  }

  if (nrow(current) == 0) {
    return(TRUE)
  }

  same_info <- isTRUE(all.equal(current, candidate, check.environment = FALSE))

  if (!same_info) {
    cli::cli_abort(
      paste0(
        "The combination of engine {.var {eng}} and mode {.var {mode}} ",
        p_type,
        "already has {component} data for model {.var {model}}
         and the new information being registered is different."
      ),
      call = call
    )
  }

  FALSE
}

# Also check for general registration

check_unregistered <- function(model, mode, eng, call = caller_env()) {
  model_info <- get_from_env(model)
  has_engine <-
    model_info |>
    dplyr::filter(engine == eng & mode == !!mode) |>
    nrow()
  if (has_engine != 1) {
    cli::cli_abort(
      "The combination of engine {.var {eng}} and mode {.var {mode}} has not \\
      been registered for model {.var {model}}.",
      call = call
    )
  }
  invisible(NULL)
}



#' @rdname set_new_model
#' @keywords internal
#' @export
set_fit <- function(model, mode, eng, value) {
  check_model_exists(model)
  check_eng_val(eng)
  check_spec_mode_engine_val(model, eng, mode)
  check_fit_info(value)
  check_unregistered(model, mode, eng)

  new_fit <-
    tibble::new_tibble(list(
      engine = eng,
      mode = mode,
      value = list(value)
    ), nrow = 1)

  if (!is_discordant_info(model, mode, eng, new_fit)) {
    return(invisible(NULL))
  }

  old_fits <- get_from_env(paste0(model, "_fit"))
  updated <- try(dplyr::bind_rows(old_fits, new_fit), silent = TRUE)
  if (inherits(updated, "try-error")) {
    cli::cli_abort(
      "An error occurred when adding the new fit module.",
      parent = updated
    )
  }

  set_env_val(
    paste0(model, "_fit"),
    updated
  )

  invisible(NULL)
}

#' @rdname set_new_model
#' @keywords internal
#' @export
get_fit <- function(model) {
  check_model_exists(model)
  fit_name <- paste0(model, "_fit")
  if (!any(fit_name != rlang::env_names(get_model_env()))) {
    cli::cli_abort("{.arg {model}} does not have a {.fn fit} method in parsnip.")
  }
  rlang::env_get(get_model_env(), fit_name)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
set_pred <- function(model, mode, eng, type, value) {
  check_model_exists(model)
  check_eng_val(eng)
  check_spec_mode_engine_val(model, eng, mode)
  check_pred_info(value, type)
  check_unregistered(model, mode, eng)

  model_info <- get_from_env(model)

  new_pred <-
    tibble::new_tibble(
      list(engine = eng, mode = mode, type = type, value = list(value)),
      nrow = 1
    )

  pred_check <- is_discordant_info(model, mode, eng, new_pred, pred_type = type, component = "predict")
  if (!pred_check) {
    return(invisible(NULL))
  }

  old_pred <- get_from_env(paste0(model, "_predict"))
  updated <- try(dplyr::bind_rows(old_pred, new_pred), silent = TRUE)
  if (inherits(updated, "try-error")) {
    cli::cli_abort(
      "An error occurred when adding the new fit module.",
      parent = updated
    )
  }

  set_env_val(paste0(model, "_predict"), updated)

  invisible(NULL)
}

#' @rdname set_new_model
#' @keywords internal
#' @export
get_pred_type <- function(model, type) {
  check_model_exists(model)
  pred_name <- paste0(model, "_predict")
  if (!any(pred_name != rlang::env_names(get_model_env()))) {
    cli::cli_abort(
      "{.arg {model}} does not have any {.arg pred} methods in parsnip."
    )
  }
  all_preds <- rlang::env_get(get_model_env(), pred_name)
  if (!any(all_preds$type == type)) {
    cli::cli_abort("{.arg {model}} does not have any prediction methods in parsnip.")
  }
  dplyr::filter(all_preds, type == !!type)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
show_model_info <- function(model) {
  check_model_exists(model)

  cat("Information for `", model, "`\n", sep = "")

  cat(
    " modes:",
    paste0(get_from_env(paste0(model, "_modes")), collapse = ", "),
    "\n\n"
  )

  engines <- get_from_env(model)
  if (nrow(engines) > 0) {
    cat(" engines: \n")

    weight_info <-
      purrr::map(
        model,
        \(x) get_from_env(paste0(x, "_fit")) |> mutate(model = x)
      ) |>
      purrr::list_rbind() |>
      dplyr::mutate(protect = map(value, ~ .x$protect)) |>
      dplyr::select(-value) |>
      dplyr::mutate(
        has_wts = purrr::map_lgl(protect, \(x) any(grepl("^weight", x))),
        has_wts = ifelse(has_wts, cli::symbol$sup_1, "")
      ) |>
      dplyr::select(engine, mode, has_wts)

      engine_weight_info <- engines |>
        dplyr::left_join(weight_info, by = c("engine", "mode")) |>
      dplyr::mutate(
        engine = paste0(engine, has_wts),
        mode = format(paste0(mode, ": "))
      ) |>
      dplyr::group_by(mode) |>
      dplyr::summarize(
        engine = paste0(sort(engine), collapse = ", ")
      ) |>
      dplyr::mutate(
        lab = paste0("   ", mode, engine, "\n")
      ) |>
      dplyr::ungroup() |>
      dplyr::pull(lab)

    cat(engine_weight_info, sep = "")

    if (!all(weight_info$has_wts == "")) {
      cat("\n", cli::symbol$sup_1, "The model can use case weights.", sep = "")
    }

    cat("\n\n")
  } else {
    cat(" no registered engines.\n\n")
  }

  args <- get_from_env(paste0(model, "_args"))
  if (nrow(args) > 0) {
    cat(" arguments: \n")
    args |>
      dplyr::select(engine, parsnip, original) |>
      dplyr::distinct() |>
      dplyr::mutate(
        engine = format(paste0("   ", engine, ": ")),
        parsnip = paste0("      ", format(parsnip), " --> ", original, "\n")
      ) |>
      dplyr::group_by(engine) |>
      dplyr::mutate(
        engine2 = ifelse(dplyr::row_number() == 1, engine, ""),
        parsnip = ifelse(dplyr::row_number() == 1, paste0("\n", parsnip), parsnip),
        lab = paste0(engine2, parsnip)
      ) |>
      dplyr::ungroup() |>
      dplyr::pull(lab) |>
      cat(sep = "")
    cat("\n")
  } else {
    cat(" no registered arguments.\n\n")
  }

  fits <- get_from_env(paste0(model, "_fit"))
  if (nrow(fits) > 0) {
    cat(" fit modules:\n")
    fits |>
      dplyr::select(-value) |>
      mutate(engine = paste0("  ", engine)) |>
      as.data.frame() |>
      print(row.names = FALSE)
    cat("\n")
  } else {
    cat(" no registered fit modules.\n\n")
  }

  preds <- get_from_env(paste0(model, "_predict"))
  if (nrow(preds) > 0) {
    cat(" prediction modules:\n")
    preds |>
      dplyr::group_by(mode, engine) |>
      dplyr::summarize(methods = paste0(sort(type), collapse = ", ")) |>
      dplyr::ungroup() |>
      mutate(mode = paste0("  ", mode)) |>
      as.data.frame() |>
      print(row.names = FALSE)
    cat("\n")
  } else {
    cat(" no registered prediction modules.\n\n")
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
pred_value_template <-  function(pre = NULL, post = NULL, func, ...) {
  if (rlang::is_missing(func)) {
    cli::cli_abort(
      "Please supply a value to {.arg func}. See {.help [{.fun set_pred}](parsnip::set_pred)}."
    )
  }
  list(pre = pre, post = post, func = func, args = list(...))
}

# ------------------------------------------------------------------------------

check_encodings <- function(x, call = caller_env()) {
  if (!is.list(x)) {
    cli::cli_abort("{.arg values} should be a list.", call = call)
  }
  req_args <- list(predictor_indicators = rlang::na_chr,
                   compute_intercept = rlang::na_lgl,
                   remove_intercept = rlang::na_lgl,
                   allow_sparse_x = rlang::na_lgl)

  missing_args <- setdiff(names(req_args), names(x))
  if (length(missing_args) > 0) {
    cli::cli_abort(
      "The values passed to {.fn set_encoding} are missing arguments: \\
      {.field {missing_args}}.",
      call = call
    )
  }
  extra_args <- setdiff(names(x), names(req_args))
  if (length(extra_args) > 0) {
    cli::cli_abort(
      "The values passed to {.fn set_encoding} had extra arguments: \\
      {.arg {extra_args}}.",
      call = call
    )
  }
  invisible(x)
}

#' @export
#' @rdname set_new_model
#' @keywords internal
set_encoding <- function(model, mode, eng, options) {
  check_model_exists(model)
  check_eng_val(eng)
  check_mode_val(mode)
  check_encodings(options)

  keys   <- tibble::new_tibble(list(model = model, engine = eng, mode = mode), nrow = 1)
  options <- tibble::as_tibble(options)
  new_values <- dplyr::bind_cols(keys, options)

  enc_check <- is_discordant_info(model, mode, eng, new_values, component = "encoding")
  if (!enc_check) {
    return(invisible(NULL))
  }

  # Allow for older versions before set_encoding() was created
  nm <- paste0(model, "_encoding")
  current <- get_from_env(nm)

  db_values <- dplyr::bind_rows(current, new_values)
  set_env_val(nm, db_values)

  invisible(NULL)
}


#' @rdname set_new_model
#' @keywords internal
#' @export
get_encoding <- function(model) {
  check_model_exists(model)
  nm <- paste0(model, "_encoding")
  res <- try(get_from_env(nm), silent = TRUE)
  if (inherits(res, "try-error")) {
    # for objects made before encodings were specified in parsnip
    res <-
      get_from_env(model) |>
      dplyr::mutate(
        model = model,
        predictor_indicators = "traditional",
        compute_intercept = TRUE,
        remove_intercept = TRUE,
        allow_sparse_x = FALSE
      ) |>
      dplyr::select(model, engine, mode, predictor_indicators,
                    compute_intercept, remove_intercept)
  }
  res
}

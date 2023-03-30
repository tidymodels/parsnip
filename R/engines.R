
specific_model <- function(x) {
  cls <- class(x)
  cls[cls != "model_spec"]
}

possible_engines <- function(object, ...) {
  m_env <- get_model_env()
  engs <- rlang::env_get(m_env, specific_model(object))
  unique(engs$engine)
}

# ------------------------------------------------------------------------------

shhhh <- function(x)
  suppressPackageStartupMessages(requireNamespace(x, quietly = TRUE))

is_installed <- function(pkg) {
  res <- try(shhhh(pkg), silent = TRUE)
  res
}

check_installs <- function(x) {
  if (length(x$method$libs) > 0) {
    is_inst <- map_lgl(x$method$libs, is_installed)
    if (any(!is_inst)) {
      missing_pkg <- x$method$libs[!is_inst]
      missing_pkg <- paste0(missing_pkg, collapse = ", ")
      rlang::abort(
        glue::glue(
          "This engine requires some package installs: ",
          glue::glue_collapse(glue::glue("'{missing_pkg}'"), sep = ", ")
        )
      )
    }
  }
}

load_libs <- function(x, quiet, attach = FALSE) {
  for (pkg in x$method$libs) {
    if (!attach) {
      suppressPackageStartupMessages(requireNamespace(pkg, quietly = quiet))
    } else {
      library(pkg, character.only = TRUE)
    }
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' Declare a computational engine and specific arguments
#'
#' `set_engine()` is used to specify which package or system will be used
#'  to fit the model, along with any arguments specific to that software.
#'
#' @details
#' In parsnip,
#'
#' - the model **type** differentiates basic modeling approaches, such as random
#' forests, logistic regression, linear support vector machines, etc.,
#' - the **mode** denotes in what kind of modeling context it will be used
#' (most commonly, classification or regression), and
#' - the computational **engine** indicates how the model is fit, such as with
#' a specific R package implementation or even methods outside of R like Keras
#' or Stan.
#'
#' Use [show_engines()] to get a list of possible engines for the model of
#' interest.
#'
#' Modeling functions in parsnip separate model arguments into two categories:
#'
#' - _Main arguments_ are more commonly used and tend to be available across
#' engines. These names are standardized to work with different engines in a
#' consistent way, so you can use the \pkg{parsnip} main argument `trees`,
#' instead of the heterogeneous arguments for this parameter from \pkg{ranger}
#' and  \pkg{randomForest} packages (`num.trees` and `ntree`, respectively). Set
#' these in your model type function, like `rand_forest(trees = 2000)`.
#' - _Engine arguments_ are either specific to a particular engine or used
#' more rarely; there is no change for these argument names from the underlying
#' engine. The `...` argument of `set_engine()` allows any engine-specific
#' argument to be passed directly to the engine fitting function, like
#' `set_engine("ranger", importance = "permutation")`.
#'
#'
#' @param object A model specification.
#' @param engine A character string for the software that should
#'  be used to fit the model. This is highly dependent on the type
#'  of model (e.g. linear regression, random forest, etc.).
#' @param ... Any optional arguments associated with the chosen computational
#'  engine. These are captured as quosures and can be tuned with `tune()`.
#' @return An updated model specification.
#' @examples
#' # First, set main arguments using the standardized names
#' logistic_reg(penalty = 0.01, mixture = 1/3) %>%
#'   # Now specify how you want to fit the model with another argument
#'   set_engine("glmnet", nlambda = 10) %>%
#'   translate()
#'
#' # Many models have possible engine-specific arguments
#' decision_tree(tree_depth = 5) %>%
#'   set_engine("rpart", parms = list(prior = c(.65,.35))) %>%
#'   set_mode("classification") %>%
#'   translate()
#'
#' @export
set_engine <- function(object, engine, ...) {
  UseMethod("set_engine")
}

#' @export
set_engine.model_spec <- function(object, engine, ...) {
  mod_type <- class(object)[1]

  if (rlang::is_missing(engine)) {
    stop_missing_engine(mod_type, call = caller_env(0))
  }
  object$engine <- engine

  # determine if the model specification could feasibly match any entry
  # in the union of the parsnip model environment and model_info_table.
  # if not, trigger an error based on the (possibly inferred) model spec slots.
  if (!spec_is_possible(spec = object,
                        engine = object$engine, user_specified_engine = TRUE)) {
    check_spec_mode_engine_val(mod_type, object$engine, object$mode)
  }

  if (object$engine == "liquidSVM") {
    lifecycle::deprecate_warn(
      "0.1.6",
      "set_engine(engine = 'cannot be liquidSVM')",
      details = "The liquidSVM package is no longer available on CRAN.")
  }

  new_model_spec(
    cls = mod_type,
    args = object$args,
    eng_args = enquos(...),
    mode = object$mode,
    user_specified_mode = object$user_specified_mode,
    method = NULL,
    engine = object$engine,
    user_specified_engine = TRUE
  )
}

#' @export
set_engine.default <- function(object, engine, ...) {
  error_set_object(object, func = "set_engine")

  invisible(FALSE)
}

#' Display currently available engines for a model
#'
#' The possible engines for a model can depend on what packages are loaded.
#' Some \pkg{parsnip} extension add engines to existing models. For example,
#' the \pkg{poissonreg} package adds additional engines for the [poisson_reg()]
#' model and these are not available unless \pkg{poissonreg} is loaded.
#' @param x The name of a `parsnip` model (e.g., "linear_reg", "mars", etc.)
#' @return A tibble.
#'
#' @examples
#' show_engines("linear_reg")
#' @export
show_engines <- function(x) {
  if (!is.character(x) || length(x) > 1) {
    rlang::abort("`show_engines()` takes a single character string as input.")
  }
  res <- try(get_from_env(x), silent = TRUE)
  if (inherits(res, "try-error") | is.null(res)) {
    rlang::abort(
      paste0("No results found for model function '", x, "'.")
    )
  }
  res
}

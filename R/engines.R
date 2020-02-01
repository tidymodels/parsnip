
specific_model <- function(x) {
  cls <- class(x)
  cls[cls != "model_spec"]
}

possible_engines <- function(object, ...) {
  m_env <- get_model_env()
  engs <- rlang::env_get(m_env, specific_model(object))
  unique(engs$engine)
}

check_engine <- function(object) {
  avail_eng <- possible_engines(object)
  if (is.null(object$engine)) {
    object$engine <- avail_eng[1]
    rlang::warn(glue::glue("`engine` was NULL and updated to be `{object$engine}`"))
  }
  if (!(object$engine %in% avail_eng)) {
    rlang::abort(
      glue::glue(
        "Engine '{object$engine}' is not available. Please use one of: ",
        glue::glue_collapse(glue::glue("'{avail_eng}'"), sep = ", ")
      )
    )
  }
  object
}

# ------------------------------------------------------------------------------

shhhh <- function(x)
  suppressPackageStartupMessages(requireNamespace(x, quietly = TRUE))

is_installed <- function(pkg) {
  res <- try(shhhh(pkg), silent = TRUE)
  res
}

#' @importFrom purrr map_lgl
check_installs <- function(x) {
  if (length(x$method$libs) > 0) {
    is_inst <- map_lgl(x$method$libs, is_installed)
    if (any(!is_inst)) {
      rlang::abort(
        glue::glue(
          "This engine requires some package installs: ",
          glue::glue_collapse(glue::glue("'{x}'"), sep = ", ")
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
#' @param object A model specification.
#' @param engine A character string for the software that should
#'  be used to fit the model. This is highly dependent on the type
#'  of model (e.g. linear regression, random forest, etc.).
#' @param ... Any optional arguments associated with the chosen computational
#'  engine. These are captured as quosures and can be `varying()`.
#' @return An updated model specification.
#' @examples
#' # First, set general arguments using the standardized names
#' mod <-
#'   logistic_reg(mixture = 1/3) %>%
#'   # now say how you want to fit the model and another other options
#'   set_engine("glmnet", nlambda = 10)
#' translate(mod, engine = "glmnet")
#' @export
set_engine <- function(object, engine, ...) {
  if (!inherits(object, "model_spec")) {
    rlang::abort("`object` should have class 'model_spec'.")
  }
  if (!is.character(engine) | length(engine) != 1)
    rlang::abort("`engine` should be a single character value.")

  object$engine <- engine
  object <- check_engine(object)

  new_model_spec(
    cls = class(object)[1],
    args = object$args,
    eng_args = enquos(...),
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

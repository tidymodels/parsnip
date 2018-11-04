
get_model_info <-  function (x, engine)  {
  cls <- specific_model(x)
  nm <- paste(cls, engine, "data", sep = "_")
  res <- try(get(nm), silent = TRUE)
  if (inherits(res, "try-error"))
    stop("Can't find model object ", nm)
  res
}

specific_model <- function(x) {
  cls <- class(x)
  cls[cls != "model_spec"]
}


possible_engines <- function(object, ...) {
  cls <- specific_model(object)
  key_df <- get(paste(cls, "engines", sep = "_"))
  colnames(key_df[object$mode, , drop = FALSE])
}

check_engine <- function(object) {
  avail_eng <- possible_engines(object)
  if (is.null(object$engine)) {
    object$engine <- avail_eng[1]
    warning("`engine` was NULL and updated to be '",
            object$engine, "'", call. = FALSE)
  }
  if (!(object$engine %in% avail_eng)) {
    stop(
      "engine '",object$engine,
      "' is not availble. Please use ",  "one of: ",
      paste0("'", avail_eng, "'", collapse = ", "),
      call. = FALSE
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
      stop(
        "This engine requires some package installs: ",
        paste0("'", x$method$libs[!is_inst], "'", collapse = ", "),
        call. = FALSE
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
#' `set_engine` is used to specify which package or system will be used
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
    stop("`object` should have class 'model_spec'.", call. = FALSE)
  }
  if (!is.character(engine) | length(engine) != 1)
    stop("`engine` should be a single character value.", call. = FALSE)

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

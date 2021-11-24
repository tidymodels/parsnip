# nocov start

.onLoad <- function(libname, pkgname) {
  s3_register("generics::tidy", "model_fit")
  s3_register("generics::tidy", "nullmodel")
  s3_register("generics::tidy", "_elnet")
  s3_register("generics::tidy", "_lognet")
  s3_register("generics::tidy", "_multnet")
  s3_register("generics::tidy", "_fishnet")
  s3_register("generics::glance", "model_fit")
  s3_register("generics::augment", "model_fit")
  s3_register("generics::required_pkgs", "model_fit")
  s3_register("generics::required_pkgs", "model_spec")

  # - If tune isn't installed, register the method (`packageVersion()` will error here)
  # - If tune >= 0.1.6.9001 is installed, register the method
  should_register_tune_args_method <- tryCatch(
    expr = utils::packageVersion("tune") >= "0.1.6.9001",
    error = function(cnd) TRUE
  )

  if (should_register_tune_args_method) {
    # `tune_args.model_spec()` moved from tune to parsnip
    vctrs::s3_register("generics::tune_args", "model_spec", tune_args_model_spec)
  }

  # - If tune isn't installed, register the method (`packageVersion()` will error here)
  # - If tune >= 0.1.6.9002 is installed, register the method
  should_register_tunable_method <- tryCatch(
    expr = utils::packageVersion("tune") >= "0.1.6.9002",
    error = function(cnd) TRUE
  )

  if (should_register_tunable_method) {
    # `tunable.model_spec()` and friends moved from tune to parsnip
    vctrs::s3_register("generics::tunable", "model_spec", tunable_model_spec)
    vctrs::s3_register("generics::tunable", "linear_reg", tunable_linear_reg)
    vctrs::s3_register("generics::tunable", "logistic_reg", tunable_logistic_reg)
    vctrs::s3_register("generics::tunable", "multinomial_reg", tunable_multinomial_reg)
    vctrs::s3_register("generics::tunable", "boost_tree", tunable_boost_tree)
    vctrs::s3_register("generics::tunable", "rand_forest", tunable_rand_forest)
    vctrs::s3_register("generics::tunable", "mars", tunable_mars)
    vctrs::s3_register("generics::tunable", "decision_tree", tunable_decision_tree)
    vctrs::s3_register("generics::tunable", "svm_poly", tunable_svm_poly)
  }

}


# vctrs:::s3_register()
s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  caller <- parent.frame()

  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }

  method_fn <- get_method(method)
  stopifnot(is.function(method_fn))

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns <- asNamespace(package)

      # Refresh the method, it might have been updated by `devtools::load_all()`
      method_fn <- get_method(method)

      registerS3method(generic, class, method_fn, envir = ns)
    }
  )

  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }

  envir <- asNamespace(package)

  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }

  invisible()
}

# nocov end



#' ## nocov start
#'
#' data_obj <- ls(pattern = "_data$")
#' data_obj <- data_obj[data_obj != "prepare_data"]
#'
#' data_names <-
#'   map_dfr(
#'     data_obj,
#'     function(x)  {
#'       module <- names(get(x))
#'       if (length(module) > 1) {
#'         module <- table(module)
#'         module <- as_tibble(module)
#'         module$object <- x
#'         module
#'       } else
#'         module <- NULL
#'       module
#'     }
#'   )
#'
#' if(any(data_names$n > 1)) {
#'   print(data_names[data_names$n > 1,])
#'   rlang::abort("Some models have duplicate module names.")
#' }
#' rm(data_names)
#'
#' # ------------------------------------------------------------------------------
#'
#' engine_objects <- ls(pattern = "_engines$")
#' engine_objects <- engine_objects[engine_objects != "possible_engines"]
#'
#' get_engine_info <- function(x) {
#'   y <- x
#'   y <- get(y)
#'   z <- stack(y)
#'   z$mode <- rownames(y)
#'   z$model <- gsub("_engines$", "", x)
#'   z$object <- x
#'   z <- z[z$values,]
#'   z <- z[z$mode != "unknown",]
#'   z$values <- NULL
#'   names(z)[1] <- "engine"
#'   z$engine <- as.character(z$engine)
#'   z
#' }
#'
#' engine_info <-
#'   purrr::map_df(
#'   parsnip:::engine_objects,
#'   get_engine_info
#' )
#'
#' rm(engine_objects)
#'
#' ## nocov end

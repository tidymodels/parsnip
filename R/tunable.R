# Unit tests are in extratests
# nocov start

#' @export
tunable.model_spec <- function(x, ...) {
  mod_env <- get_model_env()

  if (is.null(x$engine)) {
    stop("Please declare an engine first using `set_engine()`.", call. = FALSE)
  }

  arg_name <- paste0(mod_type(x), "_args")
  if (!(any(arg_name == names(mod_env)))) {
    stop(
      "The `parsnip` model database doesn't know about the arguments for ",
      "model `",
      mod_type(x),
      "`. Was it registered?",
      sep = "",
      call. = FALSE
    )
  }

  arg_vals <- mod_env[[arg_name]]
  arg_vals <- arg_vals[arg_vals$engine == x$engine, c("parsnip", "func")]
  names(arg_vals)[names(arg_vals) == "parsnip"] <- "name"
  names(arg_vals)[names(arg_vals) == "func"] <- "call_info"

  extra_args <- c(names(x$args), names(x$eng_args))
  extra_args <- extra_args[!extra_args %in% arg_vals$name]

  extra_args_tbl <-
    tibble::new_tibble(
      list(
        name = extra_args,
        call_info = vector("list", vctrs::vec_size(extra_args))
      ),
      nrow = vctrs::vec_size(extra_args)
    )

  res <- vctrs::vec_rbind(arg_vals, extra_args_tbl)

  res$source <- "model_spec"
  res$component <- mod_type(x)
  res$component_id <- "main"
  res$component_id[!res$name %in% names(x$args)] <- "engine"

  if (nrow(res) > 0) {
    has_info <- purrr::map_lgl(res$call_info, is.null)
    rm_list <- !(has_info & (res$component_id == "main"))

    res <- res[rm_list, ]
  }

  res[, c("name", "call_info", "source", "component", "component_id")]
}

mod_type <- function(.mod) class(.mod)[class(.mod) != "model_spec"][1]

# nocov end

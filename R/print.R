#' @export
print.model_spec <- function(x, ...) {
  print_model_spec(x, ...)

  invisible(x)
}

#' @keywords internal
#' @rdname add_on_exports
#' @export
print_model_spec <- function(x, cls = class(x)[1], desc = get_model_desc(cls), ...) {
  if (!spec_is_loaded(spec = structure(x, class = cls))) {
    prompt_missing_implementation(spec = structure(x, class = cls), prompt = cli::cli_inform)
  }

  mode <- switch(x$mode, unknown = "unknown mode", x$mode)

  cat(desc, " Model Specification (", mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (is_printable_spec(x)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  if (x$mode == "quantile regression") {
    cli::cli_inform("Quantile levels: {x$quantile_levels}.")
  }

  invisible(x)
}

get_model_desc <- function(cls) {
  res <- model_descs$desc[model_descs$cls == cls]

  if (length(res) == 0) {
    res <- gsub("\\_|\\.", " ", cls)
  }

  res
}

model_descs <- tibble::tribble(
  ~cls,                   ~desc,
  "auto_ml",              "Automatic Machine Learning",
  "bag_mars",             "Bagged MARS",
  "bag_mlp",              "Bagged Neural Network",
  "bag_tree",             "Bagged Decision Tree",
  "bart",                 "BART",
  "boost_tree",           "Boosted Tree",
  "C5_rules",             "C5.0",
  "cubist_rules",         "Cubist",
  "decision_tree",        "Decision Tree",
  "discrim_flexible",     "Flexible Discriminant",
  "discrim_linear",       "Linear Discriminant",
  "discrim_quad",         "Quadratic Discriminant",
  "discrim_regularized",  "Regularized Discriminant",
  "gen_additive_mod",     "GAM",
  "linear_reg",           "Linear Regression",
  "logistic_reg",         "Logistic Regression",
  "mars",                 "MARS",
  "mlp",                  "Single Layer Neural Network",
  "multinom_reg",         "Multinomial Regression",
  "naive_Bayes",          "Naive Bayes",
  "nearest_neighbor",     "K-Nearest Neighbor",
  "null_model",           "Null",
  "pls",                  "PLS",
  "poisson_reg",          "Poisson Regression",
  "proportional_hazards", "Proportional Hazards",
  "rand_forest",          "Random Forest",
  "rule_fit",             "RuleFit",
  "surv_reg",             "Parametric Survival Regression",
  "survival_reg",         "Parametric Survival Regression",
  "svm_linear",           "Linear Support Vector Machine",
  "svm_poly",             "Polynomial Support Vector Machine",
  "svm_rbf",              "Radial Basis Function Support Vector Machine"
)

#' Print helper for model objects
#'
#' A common format function that prints information about the model object (e.g.
#' arguments, calls, packages, etc).
#'
#' @param x A model object.
#' @param ... Not currently used.
#' @keywords internal
#' @export
model_printer <- function(x, ...) {
  non_null_args <- x$args[!vapply(x$args, null_value, lgl(1))]
  if (length(non_null_args) > 0) {
    cat("Main Arguments:\n")
    non_null_args <- map(non_null_args, convert_arg)
    cat(print_arg_list(non_null_args), "\n", sep = "")
  }
  if (length(x$eng_args) > 0) {
    cat("Engine-Specific Arguments:\n")
    x$eng_args <- map(x$eng_args, convert_arg)
    cat(print_arg_list(x$eng_args), "\n", sep = "")
  }
  if (!is.null(x$engine)) {
    cat("Computational engine:", x$engine, "\n\n")
    if (!is.null(x$method$fit_call)) {
      cat("Fit function:\n")
      print(x$method$fit_call)
      if (length(x$method$libs) > 0) {
        if (length(x$method$libs) > 1)
          cat("\nRequired packages:\n")
        else
          cat("\nRequired package: ")
        cat(paste0(x$method$libs, collapse = ", "), "\n")
      }
    }
  }
}

print_arg_list <- function(x, ...) {
  atomic <- vapply(x, is.atomic, logical(1))
  x2 <- x
  x2[!atomic] <-  lapply(x2[!atomic], deparserizer, ...)
  res <- paste0("  ", names(x2), " = ", x2, collaspe = "\n")
  cat(res, sep = "")
}

deparserizer <- function(x, limit = options()$width - 10) {
  x <- deparse(x, width.cutoff = limit)
  x <- gsub("^    ", "", x)
  x <- paste0(x, collapse = "")
  if (nchar(x) > limit)
    x <- paste0(substring(x, first = 1, last = limit - 7), "<snip>")
  x
}

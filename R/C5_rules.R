#' C5.0 rule-based classification models
#'
#' @description
#' `C5_rules()` defines a model that derives feature rules from a tree for
#' prediction. A single tree or boosted ensemble can be used.
#'
#' There are different ways to fit this model. The method of estimation is
#' chosen by setting the model _engine_.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("C5_rules", pkg = "rules")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param trees A non-negative integer (no greater than 100 for the number
#'  of members of the ensemble.
#' @param min_n An integer greater than one zero and nine for the minimum number
#'  of data points in a node that are required for the node to be split further.
#' @details C5.0 is a classification model that is an extension of the C4.5
#'  model of Quinlan (1993). It has tree- and rule-based versions that also
#'  include boosting capabilities. `C5_rules()` enables the version of the model
#'  that uses a series of rules (see the examples below). To make a set of
#'  rules, an initial C5.0 tree is created and flattened into rules. The rules
#'  are pruned, simplified, and ordered. Rule sets are created within each
#'  iteration of boosting.
#'
#' @references Quinlan R (1993). _C4.5: Programs for Machine Learning_. Morgan
#' Kaufmann Publishers.
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso [C50::C5.0()], [C50::C5.0Control()],
#' \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("C5_rules", "rules")}
#'
#' @examples
#' show_engines("C5_rules")
#'
#' C5_rules()
#' @export
#' @importFrom purrr map_lgl
C5_rules <-
  function(mode = "classification",
           trees = NULL,
           min_n = NULL,
           engine = "C5.0") {

    args <- list(
      trees = enquo(trees),
      min_n = enquo(min_n)
    )

    new_model_spec(
      "C5_rules",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.C5_rules <- function(x, ...) {
  cat("C5.0 Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}


# ------------------------------------------------------------------------------

#' Updating a model specification
#'
#' @param object A `C5_rules` model specification.
#' @param parameters	A 1-row tibble or named list with _main_ parameters to update.
#' If the individual arguments are used, these will supersede the values in
#' parameters. Also, using engine arguments in this object will result in an
#' error.
#' @param fresh	A logical for whether the arguments should be modified in-place
#' or replaced wholesale.
#' @param ...	Not used for `update()`.
#' @examples
#'
#' # ------------------------------------------------------------------------------
#'
#' model <- C5_rules(trees = 10, min_n = 2)
#' model
#' update(model, trees = 1)
#' update(model, trees = 1, fresh = TRUE)
#' @method update C5_rules
#' @name rules_update
#' @inheritParams C5_rules
#' @export
update.C5_rules <-
  function(object,
           parameters = NULL,
           trees = NULL, min_n = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }
    args <- list(
      trees = enquo(trees),
      min_n = enquo(min_n)
    )

    args <- update_main_parameters(args, parameters)

    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }

    new_model_spec(
      "C5_rules",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

# make work in different places

check_args.C5_rules <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$trees)) {
    if (length(args$trees) > 1) {
      rlang::abort("Only a single value of `trees` is used.")
    }
    msg <- "The number of trees should be >= 1 and <= 100. Truncating the value."
    if (args$trees > 100) {
      object$args$trees <-
        rlang::new_quosure(100L, env = rlang::empty_env())
      rlang::warn(msg)
    }
    if (args$trees < 1) {
      object$args$trees <-
        rlang::new_quosure(1L, env = rlang::empty_env())
      rlang::warn(msg)
    }

  }
  if (is.numeric(args$min_n)) {
    if (length(args$min_n) > 1) {
      rlang::abort("Only a single `min_n`` value is used.")
    }
  }
  invisible(object)
}

#' @rdname multi_predict
#' @export
#' @param trees An numeric vector of `trees` between one and 100.
#' @param object An object of class `model_fit`
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or `NULL`. Possible values
#'  are class" and "prob".
#' @param ... Not currently used.
#' @return A tibble with one row for each row of `new_data`. Multiple
#' predictions are contained in a list column called `.pred`. That column has
#' the standard `parsnip` prediction column names as well as the column with
#' the tuning parameter values.
#' @details
#' For C5.0 rule-based models, the model fit may contain less boosting
#' iterations than the number requested. Printing the object will show how many
#' were used due to early stopping. This can be change using an option in
#' [C50::C5.0Control()]. Beware that the number of iterations requested
multi_predict._C5_rules <-
  function(object, new_data, type = NULL, trees = NULL, ...) {
    if (any(names(enquos(...)) == "newdata")) {
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
    }
    trees_used <- object$fit$trials["Actual"]
    trees <- ifelse(trees > trees_used, trees_used, trees)
    trees <- sort(unique(trees))

    if (is.null(type)) {
      type <- "class"
    }

    new_data <- prepare_data(object, new_data)
    # preprocess data
    if (!is.null(object$spec$method$pred$numeric$pre)) {
      new_data <- object$spec$method$pred$numeric$pre(new_data, object)
    }

    res <- c5_pred(object, new_data, trials = trees, type = type, ...)

    res$.row_number <- rep(1:nrow(new_data), length(trees))
    res <-
      res %>%
      dplyr::group_by(.row_number) %>%
      tidyr::nest() %>%
      dplyr::ungroup() %>%
      dplyr::select(-.row_number) %>%
      setNames(".pred")
    res
  }

# ------------------------------------------------------------------------------


#' @export
#' @keywords internal
#' @rdname tunable-parsnip
tunable.C5_rules <- function(x, ...) {
  tibble::tibble(
    name = c('trees'),
    call_info = list(
      list(pkg = "dials", fun = "trees", range = c(1L, 100L))
    ),
    source = "model_spec",
    component = class(x)[class(x) != "model_spec"][1],
    component_id =  "main"
  )
}

# ------------------------------------------------------------------------------

# set_new_model("C5_rules")
# set_model_mode("C5_rules", "classification")


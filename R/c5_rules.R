# The file is named c5_rules.R even though the function is C5_rules(). The case
# change makes sure that it is sourced prior to aaa_models.R

#' C5.0 rule-based classification models
#'
#' @description
#' `C5_rules()` defines a model that derives feature rules from a tree for
#' prediction. A single tree or boosted ensemble can be used. This function can
#' fit classification models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("C5_rules")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams nearest_neighbor
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param trees A non-negative integer (no greater than 100) for the number
#'  of members of the ensemble.
#' @param min_n An integer greater between zero and nine for the minimum number
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
#'
#' @templateVar modeltype C5_rules
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso [C50::C5.0()], [C50::C5.0Control()],
#' \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("C5_rules")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("C5_rules")
#'
#' C5_rules()
#' @export
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
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
  }

# ------------------------------------------------------------------------------

#' Updating a model specification
#'
#' @param object A `C5_rules` model specification.
#' @inheritParams update.boost_tree
#' @examplesIf !parsnip:::is_cran_check()
#'
#' # ------------------------------------------------------------------------------
#'
#' model <- C5_rules(trees = 10, min_n = 2)
#' model
#' update(model, trees = 1)
#' update(model, trees = 1, fresh = TRUE)
#' @method update C5_rules
#' @rdname parsnip_update
#' @inheritParams C5_rules
#' @export
update.C5_rules <-
  function(object,
           parameters = NULL,
           trees = NULL, min_n = NULL,
           fresh = FALSE, ...) {

    args <- list(
      trees = enquo(trees),
      min_n = enquo(min_n)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "C5_rules",
      ...
    )
  }

# ------------------------------------------------------------------------------

# make work in different places

#' @export
check_args.C5_rules <- function(object, call = rlang::caller_env()) {

  args <- lapply(object$args, rlang::eval_tidy)

  check_number_whole(args$min_n, allow_null = TRUE, call = call, arg = "min_n")
  check_number_whole(args$tree, allow_null = TRUE, call = call, arg = "tree")

  msg <- "The number of trees should be {.code >= 1} and {.code <= 100}"
  if (!(is.null(args$trees)) && args$trees > 100) {
    object$args$trees <- rlang::new_quosure(100L, env = rlang::empty_env())
    cli::cli_warn(c(msg, "Truncating to 100."))
  }
  if (!(is.null(args$trees)) && args$trees < 1) {
    object$args$trees <- rlang::new_quosure(1L, env = rlang::empty_env())
    cli::cli_warn(c(msg, "Truncating to 1."))
  }

  invisible(object)
}

# ------------------------------------------------------------------------------

set_new_model("C5_rules")
set_model_mode("C5_rules", "classification")


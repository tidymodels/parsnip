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
#' @inheritParams boost_tree
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
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso [C50::C5.0()], [C50::C5.0Control()],
#' \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("C5_rules")}
#'
#' @examples
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
      trees = enexpr(trees),
      min_n = enexpr(min_n)
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
#' @inheritParams update.boost_tree
#' @examples
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
      trees = enexpr(trees),
      min_n = enexpr(min_n)
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

# ------------------------------------------------------------------------------

set_new_model("C5_rules")
set_model_mode("C5_rules", "classification")


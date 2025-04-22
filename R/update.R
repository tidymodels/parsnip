#' Update a model specification
#'
#' @description
#' If parameters of a model specification need to be modified, `update()` can
#' be used in lieu of recreating the object from scratch.
#'
#' @inheritParams boost_tree
#' @inheritParams decision_tree
#' @inheritParams linear_reg
#' @inheritParams logistic_reg
#' @inheritParams mars
#' @inheritParams mlp
#' @inheritParams multinom_reg
#' @inheritParams nearest_neighbor
#' @inheritParams proportional_hazards
#' @inheritParams rand_forest
#' @inheritParams surv_reg
#' @inheritParams svm_linear
#' @inheritParams svm_poly
#' @inheritParams svm_rbf
#' @param object A [model specification][model_spec].
#' @param parameters A 1-row tibble or named list with _main_
#'  parameters to update. Use **either** `parameters` **or** the main arguments
#'  directly when updating. If the main arguments are used,
#'  these will supersede the values in `parameters`. Also, using
#'  engine arguments in this object will result in an error.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place or replaced wholesale.
#' @return An updated model specification.
#' @name parsnip_update
#' @examplesIf !parsnip:::is_cran_check()
#' model <- boost_tree(mtry = 10, min_n = 3)
#' model
#' update(model, mtry = 1)
#' update(model, mtry = 1, fresh = TRUE)
#'
#' param_values <- tibble::tibble(mtry = 10, tree_depth = 5)
#'
#' model |> update(param_values)
#' model |> update(param_values, mtry = 3)
#'
#' param_values$verbose <- 0
#' # Fails due to engine argument
#' # model |> update(param_values)
#'
#' model <- linear_reg(penalty = 10, mixture = 0.1)
#' model
#' update(model, penalty = 1)
#' update(model, penalty = 1, fresh = TRUE)
#'
NULL

# Helper function for update methods to lightly wrap:
# * object, parameters, fresh, and ... are synonymous with their docs in update.*
# * args_enquo_list is a named list passing each model-specific parameter as a quosure
# * cls is synonymous with the first argument to `new_model_spec`
#' @export
#' @keywords internal
#' @rdname add_on_exports
update_spec <- function(object, parameters, args_enquo_list, fresh, cls, ...,
                        call = caller_env()) {
  check_bool(fresh, call = call)
  eng_args <- update_engine_parameters(object$eng_args, fresh, ...)

  if (!is.null(parameters)) {
    parameters <- check_final_param(parameters, call = call)
  }

  args <- update_main_parameters(args_enquo_list, parameters, call = call)

  if (fresh) {
    object$args <- args
    object$eng_args <- eng_args
  } else {
    null_args <- map_lgl(args, null_value)
    if (any(null_args))
      args <- args[!null_args]
    if (length(args) > 0)
      object$args[names(args)] <- args
    if (length(eng_args) > 0)
      object$eng_args[names(eng_args)] <- eng_args
  }

  new_model_spec(
    cls,
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    user_specified_mode = object$user_specified_mode,
    method = NULL,
    engine = object$engine,
    user_specified_engine = object$user_specified_engine
  )
}

#' Update a model specification
#'
#' @description
#' If parameters of a model specification need to be modified, `update()` can
#' be used in lieu of recreating the object from scratch.
#'
#' @param object A model specification.
#' @param parameters A 1-row tibble or named list with _main_
#'  parameters to update. If the individual arguments are used,
#'  these will supersede the values in `parameters`. Also, using
#'  engine arguments in this object will result in an error.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @name parsnip_update
#' @examples
#' model <- boost_tree(mtry = 10, min_n = 3)
#' model
#' update(model, mtry = 1)
#' update(model, mtry = 1, fresh = TRUE)
#'
#' param_values <- tibble::tibble(mtry = 10, tree_depth = 5)
#'
#' model %>% update(param_values)
#' model %>% update(param_values, mtry = 3)
#'
#' param_values$verbose <- 0
#' # Fails due to engine argument
#' # model %>% update(param_values)
#'
#' model <- linear_reg(penalty = 10, mixture = 0.1)
#' model
#' update(model, penalty = 1)
#' update(model, penalty = 1, fresh = TRUE)
#'
NULL

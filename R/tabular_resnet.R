#' Residual Neural Network for Tabular Data
#'
#' @description
#' `tabular_resnet()` ... This function can fit classification and
#' regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("tabular_resnet")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams mlp
#' @inheritParams linear_reg
#' @inheritParams boost_tree
#' @param hidden_units An integer vector for the number of units in the hidden
#' model.
#' @param bottleneck_units The number of embeddings that are produced by batch
#' normalization.
#' @param residual_at An integer vector with the layer number should use a
#' residual connection (i.e., skip layer).
#' @param penalty A non-negative numeric value for the amount of weight
#'  decay.
#' @param mixture A number between zero and one (inclusive) giving the
#'  proportion of L1 regularization (i.e. lasso) in the model. `mixture = 1`
#'  is a pure lasso model while `mixture = 0` indicates ridge regression
#'  (a.k.a weight decay).
#' @param dropout A number between 0 (inclusive) and 1 denoting the proportion
#'  of model parameters randomly set to zero during model training.
#' @param epochs An integer for the number of training iterations.
#' @param activation A vector character strings denoting the type of relationship
#'  between the layers. The activation
#'  function between the hidden and output layers is automatically set to either
#'  "linear" or "softmax" depending on the type of outcome. Possible values
#'  depend on the engine being used.
#' @param rate_schedule A character string for the learning rate schedule.
#' @param momentum A number for the momentum parameter in optimizers that use it.
#' @param batch_size An integer for the number of training instances in each
#'  batch.
#' @param class_weights Numeric class weights for imbalanced data
#'  (classification only).
#'
#' @templateVar modeltype tabular_resnet
# @template spec-details
#'
# @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("tabular_resnet")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("tabular_resnet")
#'
#' tabular_resnet(mode = "classification", penalty = 0.01)
#' @export

tabular_resnet <-
  function(
    mode = "unknown",
    engine = "brulee",
    hidden_units = NULL,
    bottleneck_units = NULL,
    residual_at = NULL,
    penalty = NULL,
    mixture = NULL,
    dropout = NULL,
    epochs = NULL,
    activation = NULL,
    learn_rate = NULL,
    rate_schedule = NULL,
    momentum = NULL,
    batch_size = NULL,
    class_weights = NULL,
    stop_iter = NULL
  ) {
    args <- list(
      hidden_units = enquo(hidden_units),
      bottleneck_units = enquo(bottleneck_units),
      residual_at = enquo(residual_at),
      penalty = enquo(penalty),
      mixture = enquo(mixture),
      dropout = enquo(dropout),
      epochs = enquo(epochs),
      activation = enquo(activation),
      learn_rate = enquo(learn_rate),
      rate_schedule = enquo(rate_schedule),
      momentum = enquo(momentum),
      batch_size = enquo(batch_size),
      class_weights = enquo(class_weights),
      stop_iter = enquo(stop_iter)
    )

    new_model_spec(
      "tabular_resnet",
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
#' @method update tabular_resnet
#' @rdname parsnip_update
#' @inheritParams tabular_resnet
#' @param object A [model specification][model_spec].
#' @param parameters A 1-row tibble or named list with _main_
#'  parameters to update. Use **either** `parameters` **or** the main arguments
#'  directly when updating. If the main arguments are used,
#'  these will supersede the values in `parameters`. Also, using
#'  engine arguments in this object will result in an error.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place or replaced wholesale.
#' @export
update.tabular_resnet <-
  function(
    object,
    parameters = NULL,
    hidden_units = NULL,
    bottleneck_units = NULL,
    residual_at = NULL,
    penalty = NULL,
    mixture = NULL,
    dropout = NULL,
    epochs = NULL,
    activation = NULL,
    learn_rate = NULL,
    rate_schedule = NULL,
    momentum = NULL,
    batch_size = NULL,
    class_weights = NULL,
    stop_iter = NULL,
    fresh = FALSE,
    ...
  ) {
    args <- list(
      hidden_units = enquo(hidden_units),
      bottleneck_units = enquo(bottleneck_units),
      residual_at = enquo(residual_at),
      penalty = enquo(penalty),
      mixture = enquo(mixture),
      dropout = enquo(dropout),
      epochs = enquo(epochs),
      activation = enquo(activation),
      learn_rate = enquo(learn_rate),
      rate_schedule = enquo(rate_schedule),
      momentum = enquo(momentum),
      batch_size = enquo(batch_size),
      class_weights = enquo(class_weights),
      stop_iter = enquo(stop_iter)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "tabular_resnet",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.tabular_resnet <- function(object, call = rlang::caller_env()) {
  args <- lapply(object$args, rlang::eval_tidy)

  check_number_decimal(
    args$penalty,
    min = 0,
    allow_null = TRUE,
    call = call,
    arg = "penalty"
  )
  check_number_decimal(
    args$mixture,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "mixture"
  )
  check_number_decimal(
    args$dropout,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "dropout"
  )
  check_number_whole(
    args$stop_iter,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "stop_iter"
  )

  if (
    is.numeric(args$penalty) &&
      is.numeric(args$dropout) &&
      args$dropout > 0 &&
      args$penalty > 0
  ) {
    cli::cli_abort(
      "Both weight decay and dropout should not be specified.",
      call = call
    )
  }

  invisible(object)
}

## -----------------------------------------------------------------------------

set_new_model("tabular_resnet")
set_model_mode("tabular_resnet", mode = "classification")
set_model_mode("tabular_resnet", mode = "regression")

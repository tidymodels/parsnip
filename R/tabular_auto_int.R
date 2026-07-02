#' AutoInt: Automatic Feature Interaction Learning
#'
#' @description
#' `tabular_auto_int()` uses an attention mechanism to automatically learn
#' embedding co-representations for tabular data. This function can fit
#' classification and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("tabular_auto_int")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams mlp
#' @inheritParams linear_reg
#' @inheritParams boost_tree
#' @param hidden_units An integer vector for the number of units in the hidden
#'  layers after the attention mechanism.
#' @param hidden_activations A character vector denoting the activation functions
#'  for the hidden layers.
#' @param num_embedding An integer for the dimensionality of the embedding space
#'  for features.
#' @param num_attn_feat An integer for the number of attention features.
#' @param num_attn_heads An integer for the number of attention heads in the
#'  multi-head attention mechanism.
#' @param num_attn_blocks An integer for the number of sequential attention
#'  blocks.
#' @param dropout_attn A number between 0 (inclusive) and 1 denoting the
#'  proportion of attention weights set to zero during model training.
#' @param dropout_embedding A number between 0 (inclusive) and 1 denoting the
#'  proportion of embedding values set to zero during model training.
#' @param rate_schedule A character string for the learning rate schedule.
#' @param momentum A number for the momentum parameter in optimizers that use it.
#' @param batch_size An integer for the number of training instances in each
#'  batch.
#' @param class_weights Numeric class weights for imbalanced data
#'  (classification only).
#'
#' @templateVar modeltype tabular_auto_int
# @template spec-details
#'
# @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("tabular_auto_int")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("tabular_auto_int")
#'
#' tabular_auto_int(mode = "classification", num_attn_blocks = 4)
#' @export

tabular_auto_int <-
  function(
    mode = "unknown",
    engine = "brulee",
    epochs = NULL,
    num_embedding = NULL,
    hidden_units = NULL,
    hidden_activations = NULL,
    num_attn_feat = NULL,
    num_attn_heads = NULL,
    num_attn_blocks = NULL,
    activation = NULL,
    dropout = NULL,
    dropout_attn = NULL,
    dropout_embedding = NULL,
    penalty = NULL,
    mixture = NULL,
    learn_rate = NULL,
    rate_schedule = NULL,
    momentum = NULL,
    batch_size = NULL,
    class_weights = NULL,
    stop_iter = NULL
  ) {
    args <- list(
      epochs = enquo(epochs),
      num_embedding = enquo(num_embedding),
      hidden_units = enquo(hidden_units),
      hidden_activations = enquo(hidden_activations),
      num_attn_feat = enquo(num_attn_feat),
      num_attn_heads = enquo(num_attn_heads),
      num_attn_blocks = enquo(num_attn_blocks),
      activation = enquo(activation),
      dropout = enquo(dropout),
      dropout_attn = enquo(dropout_attn),
      dropout_embedding = enquo(dropout_embedding),
      penalty = enquo(penalty),
      mixture = enquo(mixture),
      learn_rate = enquo(learn_rate),
      rate_schedule = enquo(rate_schedule),
      momentum = enquo(momentum),
      batch_size = enquo(batch_size),
      class_weights = enquo(class_weights),
      stop_iter = enquo(stop_iter)
    )

    parsnip::new_model_spec(
      "tabular_auto_int",
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

#' @method update tabular_auto_int
#' @rdname parsnip_update
#' @inheritParams tabular_auto_int
#' @export
update.tabular_auto_int <-
  function(
    object,
    parameters = NULL,
    epochs = NULL,
    num_embedding = NULL,
    hidden_units = NULL,
    hidden_activations = NULL,
    num_attn_feat = NULL,
    num_attn_heads = NULL,
    num_attn_blocks = NULL,
    activation = NULL,
    dropout = NULL,
    dropout_attn = NULL,
    dropout_embedding = NULL,
    penalty = NULL,
    mixture = NULL,
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
      epochs = enquo(epochs),
      num_embedding = enquo(num_embedding),
      hidden_units = enquo(hidden_units),
      hidden_activations = enquo(hidden_activations),
      num_attn_feat = enquo(num_attn_feat),
      num_attn_heads = enquo(num_attn_heads),
      num_attn_blocks = enquo(num_attn_blocks),
      activation = enquo(activation),
      dropout = enquo(dropout),
      dropout_attn = enquo(dropout_attn),
      dropout_embedding = enquo(dropout_embedding),
      penalty = enquo(penalty),
      mixture = enquo(mixture),
      learn_rate = enquo(learn_rate),
      rate_schedule = enquo(rate_schedule),
      momentum = enquo(momentum),
      batch_size = enquo(batch_size),
      class_weights = enquo(class_weights),
      stop_iter = enquo(stop_iter)
    )

    parsnip::update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "tabular_auto_int",
      ...
    )
  }


#' @export
check_args.tabular_auto_int <- function(object, call = rlang::caller_env()) {
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
  check_number_decimal(
    args$dropout_attn,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "dropout_attn"
  )
  check_number_decimal(
    args$dropout_embedding,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "dropout_embedding"
  )
  check_number_whole(
    args$epochs,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "epochs"
  )
  check_number_whole(
    args$num_attn_feat,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "num_attn_feat"
  )
  check_number_whole(
    args$num_attn_heads,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "num_attn_heads"
  )
  check_number_whole(
    args$num_attn_blocks,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "num_attn_blocks"
  )
  check_number_whole(
    args$num_embedding,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "num_embedding"
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

# ------------------------------------------------------------------------------

set_new_model("tabular_auto_int")
set_model_mode("tabular_auto_int", mode = "classification")
set_model_mode("tabular_auto_int", mode = "regression")

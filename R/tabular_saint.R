#' SAINT: Self-Attention and Inter-sample Attention Transformer
#'
#' @description
#' `tabular_saint()` uses self-attention and inter-sample attention mechanisms
#' to learn feature interactions for tabular data. This function can fit
#' classification and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("tabular_saint")}
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
#' @param attention_type A character string for the type of attention to use.
#'  Options are `"column"` (SAINT-s), `"row"` (SAINT-i), or `"both"` (full
#'  SAINT).
#' @param num_attn_heads An integer for the number of attention heads in the
#'  multi-head attention mechanism.
#' @param num_attn_blocks An integer for the number of sequential attention
#'  blocks.
#' @param dropout_attn A number between 0 (inclusive) and 1 denoting the
#'  proportion of attention weights set to zero during model training.
#' @param dropout_hidden A number between 0 (inclusive) and 1 denoting the
#'  proportion of values in the feed-forward layers set to zero during training.
#' @param dropout_last A number between 0 (inclusive) and 1 denoting the
#'  proportion of values set to zero between the last hidden layer and the
#'  output head.
#' @param rate_schedule A character string for the learning rate schedule.
#' @param momentum A number for the momentum parameter in optimizers that use it.
#' @param batch_size An integer for the number of training instances in each
#'  batch.
#' @param target_token A logical for whether to use a learnable target token
#'  (CLS-like embedding) to aggregate information for prediction. When `TRUE`,
#'  the model appends a special target token to the input that attends to all
#'  features via the attention mechanism.
#' @param class_weights Numeric class weights for imbalanced data
#'  (classification only).
#'
#' @templateVar modeltype tabular_saint
#'
#' @details
#' ## Row attention at prediction time
#'
#' When `attention_type` is `"row"` or `"both"`, SAINT applies inter-sample
#' (row) attention across the samples in a batch. The \pkg{brulee} engine keeps
#' this on at prediction time by default, so the prediction for a given row
#' depends on which other rows are passed to [predict()] in the same call. To
#' obtain batch-independent predictions (where a row's prediction does not
#' change with its neighbors), bypass row attention at predict time with
#' `set_engine("brulee", row_attention_on_predict = FALSE)`.
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("tabular_saint")}
#'
#' @references
#' Somepalli, G., Goldblum, M., Schwarzschild, A., Bruss, C. B., & Goldstein,
#' T. (2021). SAINT: Improved Neural Networks for Tabular Data via Row
#' Attention and Contrastive Pre-Training. arXiv:2106.01342.
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("tabular_saint")
#'
#' tabular_saint(mode = "classification", num_attn_blocks = 4)
#' @export

tabular_saint <-
  function(
    mode = "unknown",
    engine = "brulee",
    epochs = NULL,
    num_embedding = NULL,
    attention_type = NULL,
    num_attn_heads = NULL,
    num_attn_blocks = NULL,
    dropout_attn = NULL,
    dropout_hidden = NULL,
    dropout_last = NULL,
    hidden_units = NULL,
    hidden_activations = NULL,
    target_token = NULL,
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
      attention_type = enquo(attention_type),
      num_attn_heads = enquo(num_attn_heads),
      num_attn_blocks = enquo(num_attn_blocks),
      dropout_attn = enquo(dropout_attn),
      dropout_hidden = enquo(dropout_hidden),
      dropout_last = enquo(dropout_last),
      hidden_units = enquo(hidden_units),
      hidden_activations = enquo(hidden_activations),
      target_token = enquo(target_token),
      penalty = enquo(penalty),
      mixture = enquo(mixture),
      learn_rate = enquo(learn_rate),
      rate_schedule = enquo(rate_schedule),
      momentum = enquo(momentum),
      batch_size = enquo(batch_size),
      class_weights = enquo(class_weights),
      stop_iter = enquo(stop_iter)
    )

    new_model_spec(
      "tabular_saint",
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

#' @method update tabular_saint
#' @rdname parsnip_update
#' @inheritParams tabular_saint
#' @export
update.tabular_saint <-
  function(
    object,
    parameters = NULL,
    epochs = NULL,
    num_embedding = NULL,
    attention_type = NULL,
    num_attn_heads = NULL,
    num_attn_blocks = NULL,
    dropout_attn = NULL,
    dropout_hidden = NULL,
    dropout_last = NULL,
    hidden_units = NULL,
    hidden_activations = NULL,
    target_token = NULL,
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
      attention_type = enquo(attention_type),
      num_attn_heads = enquo(num_attn_heads),
      num_attn_blocks = enquo(num_attn_blocks),
      dropout_attn = enquo(dropout_attn),
      dropout_hidden = enquo(dropout_hidden),
      dropout_last = enquo(dropout_last),
      hidden_units = enquo(hidden_units),
      hidden_activations = enquo(hidden_activations),
      target_token = enquo(target_token),
      penalty = enquo(penalty),
      mixture = enquo(mixture),
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
      cls = "tabular_saint",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @method check_args tabular_saint
#' @export
check_args.tabular_saint <- function(object, call = rlang::caller_env()) {
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
    args$dropout_attn,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "dropout_attn"
  )
  check_number_decimal(
    args$dropout_hidden,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "dropout_hidden"
  )
  check_number_decimal(
    args$dropout_last,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "dropout_last"
  )
  check_number_whole(
    args$epochs,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "epochs"
  )
  check_number_whole(
    args$num_embedding,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "num_embedding"
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
    args$stop_iter,
    min = 1,
    allow_null = TRUE,
    call = call,
    arg = "stop_iter"
  )

  if (!is.null(args$attention_type)) {
    arg_match0(
      args$attention_type,
      c("column", "row", "both"),
      arg_nm = "penalty_type",
      error_call = call
    )
  }

  invisible(object)
}

# ------------------------------------------------------------------------------

set_new_model("tabular_saint")
set_model_mode("tabular_saint", mode = "classification")
set_model_mode("tabular_saint", mode = "regression")

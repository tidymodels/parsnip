#' @details
#' This function only defines what _type_ of model is being fit. Once an engine
#'  is specified, the _method_ to fit the model is also defined. See
#'  [set_engine()] for more on setting the engine, including how to set engine
#'  arguments.
#'
#' The model is not trained or fit until the [`fit()`][fit.model_spec()] function is used
#' with the data.

#' Model Specification Information
#' 
#' 
#' An object with class "model_spec" is a container for
#'  information about a model that will be fit.
#' 
#' The main elements of the object are:
#'   
#'   * `args`: A vector of the main arguments for the model. The
#'  names of these arguments may be different form their
#'  counterparts n the underlying model function. For example, for a
#'  `glmnet` model, the argument name for the amount of the penalty
#'  is called "penalty" instead of "lambda" to make it more
#'  general and usable across different types of models (and to not
#'  be specific to a particular model function). The elements of
#'  `args` can be quoted expressions or `varying()`. If left to
#'  their defaults (`NULL`), the arguments will use the underlying
#'  model functions default value.
#' 
#'   * `other`: An optional vector of model-function-specific
#'  parameters. As with `args`, these can also be quoted or
#'  `varying()`.
#' 
#'   * `mode`: The type of model, such as "regression" or
#'  "classification". Other modes will be added once the package
#'  adds more functionality.

#' 
#'   * `method`: This is a slot that is filled in later by the
#'  model's constructor function. It generally contains lists of
#'  information that are used to create the fit and prediction code
#'  as well as required packages and similar data.
#' 
#'   * `engine`: This character string declares exactly what
#'  software will be used. It can be a package name or a technology
#'  type.
#' 
#'   This class and structure is the basis for how \pkg{parsnip}
#'  stores model objects prior to seeing the data.
#' @rdname model_spec 
#' @name model_spec
NULL

#' Model Fit Object Information
#' 
#' 
#' An object with class "model_fit" is a container for
#'  information about a model that has been fit to the data.
#' 
#' The main elements of the object are:
#'   
#'   * `lvl`: A vector of factor levels when the outcome is 
#'  is a factor. This is `NULL` when the outcome is not a factor
#'  vector. 
#' 
#'   * `spec`: A `model_spec` object.
#' 
#'   * `fit`: The object produced by the fitting function.
#' 
#'   * `preproc`: This contains any data-specific information
#'  required to process new a sample point for prediction. For
#'  example, if the underlying model function requires arguments `x`
#'  and `y` and the user passed a formula to `fit`, the `preproc`
#'  object would contain items such as the terms object and so on.
#'  When no information is required, this is `NA`.
#' 
#' 
#' This class and structure is the basis for how \pkg{parsnip}
#'  stores model objects after to seeing the data and applying a model.
#' @rdname model_fit 
#' @name model_fit
NULL


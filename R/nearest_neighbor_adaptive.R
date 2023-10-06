#' @title Discriminant Adaptive Nearest Neighbor Classification
#' @param mode A single character string for the type of model. The only
#' possible value for this model is "classification".
#' @param neighbors The number of data points used for final classification.
#' @param neighborhood The number of data points used to calculate between and
#' within class covariance.
#' @param matrix_diagonal Diagonal elements of a diagonal matrix. 1 is the
#' identity
#' matrix.
#' @param weighted weighted argument to ncoord. See [fpc::ncoord()] for details.
#' @param sphere One of "mcd", "mve", "classical", or "none" See [fpc::ncoord()]
#' for details.
#' @param num_comp Dimension of subspace used by dann. See [fpc::ncoord()] for
#' details.
#' @return  An S3 class of type nearest_neighbor_adaptive.
#' @details
#' Discriminant Adaptive Nearest Neighbor (dann) is a variation of k nearest
#' neighbors where the shape of the neighborhood is data driven. The
#' neighborhood is elongated along class boundaries and shrunk in the orthogonal
#' direction.
#'
#' This function has engines dann and sub_dann.
#'
#' @examples
#'
#' library(parsnip)
#' library(tidydann)
#'
#' data("two_class_dat", package = "modeldata")
#'
#' model <- nearest_neighbor_adaptive(neighbors = 2) %>%
#'   set_engine("dann") %>%
#'   fit(formula = Class ~ A + B, data = two_class_dat)
#'
#' model %>%
#'   predict(new_data = two_class_dat)
#'
#' model <- nearest_neighbor_adaptive(neighbors = 2) %>%
#'   set_engine("sub_dann") %>%
#'   fit(formula = Class ~ A + B, data = two_class_dat)
#'
#' model %>%
#'   predict(new_data = two_class_dat)
#'
#' @export
nearest_neighbor_adaptive <- function(mode = "classification", neighbors = NULL,
                                      neighborhood = NULL,
                                      matrix_diagonal = NULL, weighted = NULL,
                                      sphere = NULL, num_comp = NULL) {
  # Check for correct mode
  if (mode != "classification") {
    rlang::abort("`mode` should be 'classification'")
  }

  args <- list(
    neighbors = rlang::enquo(neighbors),
    neighborhood = rlang::enquo(neighborhood),
    matrix_diagonal = rlang::enquo(matrix_diagonal),
    weighted = rlang::enquo(weighted),
    sphere = rlang::enquo(sphere),
    num_comp = rlang::enquo(num_comp)
  )

  # Save some empty slots for future parts of the specification
  parsnip::new_model_spec(
    cls = "nearest_neighbor_adaptive",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

#' @method update nearest_neighbor_adaptive
#' @rdname parsnip_update
#' @inheritParams nearest_neighbor_adaptive
#' @export
update.nearest_neighbor_adaptive <- function(object, parameters = NULL,
                                             neighbors = NULL,
                                             neighborhood = NULL,
                                             matrix_diagonal = NULL,
                                             weighted = NULL, sphere = NULL,
                                             num_comp = NULL, fresh = FALSE,
                                             ...) {
  args <- list(
    neighbors = rlang::enquo(neighbors),
    neighborhood = rlang::enquo(neighborhood),
    matrix_diagonal = rlang::enquo(matrix_diagonal),
    weighted = rlang::enquo(weighted),
    sphere = rlang::enquo(sphere),
    num_comp = rlang::enquo(num_comp)
  )

  parsnip::update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "nearest_neighbor_adaptive",
    ...
  )
}

set_new_model("nearest_neighbor_adaptive")
set_model_mode("nearest_neighbor_adaptive", "classification")

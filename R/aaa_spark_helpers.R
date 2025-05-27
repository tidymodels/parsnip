# some spark helper functions

format_spark_probs <- function(results, object) {
  results <- dplyr::select(results, starts_with("probability_"))
  p <- ncol(results)
  lvl <- colnames(results)
  names(lvl) <- paste0("pred_", object$fit$index_labels)
  results |> dplyr::rename(!!!syms(lvl))
}

format_spark_class <- function(results, object) {
  results <- dplyr::select(results, predicted_label)
  results <- dplyr::rename(results, pred_class = predicted_label)
  results
}

format_spark_num <- function(results, object) {
  results <- dplyr::select(results, prediction)
  results <- dplyr::rename(results, pred = prediction)
  results
}

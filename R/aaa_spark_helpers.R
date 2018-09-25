# some spark helper functions

#' @importFrom dplyr starts_with rename rename_at vars funs
format_spark_probs <- function(results, object) {
  results <- dplyr::select(results, starts_with("probability_"))
  results <- dplyr::rename_at(
    results,
    vars(starts_with("probability_")),
    funs(gsub("probability", "pred", .))
  )
  results
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

#' @importFrom utils globalVariables
utils::globalVariables(c(".", "predicted_label", "prediction"))

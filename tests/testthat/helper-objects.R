library(modeldata)

data("wa_churn")
data("lending_club")
data("hpc_data")

# ------------------------------------------------------------------------------

ctrl          <- control_parsnip(verbosity = 1, catch = FALSE)
caught_ctrl   <- control_parsnip(verbosity = 1, catch = TRUE)
quiet_ctrl    <- control_parsnip(verbosity = 0, catch = TRUE)

run_glmnet <- utils::compareVersion('3.6.0', as.character(getRversion())) > 0

# ------------------------------------------------------------------------------
# for skips

is_tf_ok <- function() {
  tf_ver <- try(tensorflow::tf_version(), silent = TRUE)
  if (inherits(tf_ver, "try-error")) {
    res <- FALSE
  } else {
    res <- !is.null(tf_ver)
  }
  res
}

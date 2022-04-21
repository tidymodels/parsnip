
tune_check <- function() {
  if (rlang::is_installed("tune")) {
    res <- utils::packageVersion("tune") <= "0.1.6"
  } else {
    res <- TRUE
  }
  res
}


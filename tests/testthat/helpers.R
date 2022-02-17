
# In some cases, the test value needs to be wrapped in an empty
#  environment. If arguments are set in the model specification
# (as opposed to being set by a `translate` function), they will
# need this wrapper.

new_empty_quosure <- function(expr) {
  new_quosure(expr, env = empty_env())
}

tune_check <- function() {
  if (rlang::is_installed("tune")) {
    res <- utils::packageVersion("tune") <= "0.1.6"
  } else {
    res <- TRUE
  }
  res
}


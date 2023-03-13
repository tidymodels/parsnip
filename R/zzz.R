# nocov start

.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("ggplot2::autoplot", "model_fit")
  vctrs::s3_register("ggplot2::autoplot", "glmnet")
}

# nocov end

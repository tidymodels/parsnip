# nocov start

.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("generics::tidy", "model_fit")
  vctrs::s3_register("generics::tidy", "nullmodel")
  vctrs::s3_register("generics::tidy", "_elnet")
  vctrs::s3_register("generics::tidy", "_lognet")
  vctrs::s3_register("generics::tidy", "_multnet")
  vctrs::s3_register("generics::tidy", "_fishnet")
  vctrs::s3_register("generics::glance", "model_fit")
  vctrs::s3_register("generics::augment", "model_fit")

  vctrs::s3_register("ggplot2::autoplot", "model_fit")
  vctrs::s3_register("ggplot2::autoplot", "glmnet")
}

# nocov end

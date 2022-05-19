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
  vctrs::s3_register("generics::required_pkgs", "model_fit")
  vctrs::s3_register("generics::required_pkgs", "model_spec")

  vctrs::s3_register("ggplot2::autoplot", "model_fit")
  vctrs::s3_register("ggplot2::autoplot", "glmnet")

  # - If tune isn't installed, register the method (`packageVersion()` will error here)
  # - If tune >= 0.1.6.9001 is installed, register the method
  should_register_tune_args_method <- tryCatch(
    expr = utils::packageVersion("tune") >= "0.1.6.9001",
    error = function(cnd) TRUE
  )

  if (should_register_tune_args_method) {
    # `tune_args.model_spec()` moved from tune to parsnip
    vctrs::s3_register("generics::tune_args", "model_spec", tune_args_model_spec)
  }

  # - If tune isn't installed, register the method (`packageVersion()` will error here)
  # - If tune >= 0.1.6.9002 is installed, register the method
  should_register_tunable_method <- tryCatch(
    expr = utils::packageVersion("tune") >= "0.1.6.9002",
    error = function(cnd) TRUE
  )

  if (should_register_tunable_method) {
    # `tunable.model_spec()` and friends moved from tune to parsnip
    vctrs::s3_register("generics::tunable", "model_spec", tunable_model_spec)
    vctrs::s3_register("generics::tunable", "linear_reg", tunable_linear_reg)
    vctrs::s3_register("generics::tunable", "logistic_reg", tunable_logistic_reg)
    vctrs::s3_register("generics::tunable", "multinomial_reg", tunable_multinomial_reg)
    vctrs::s3_register("generics::tunable", "boost_tree", tunable_boost_tree)
    vctrs::s3_register("generics::tunable", "rand_forest", tunable_rand_forest)
    vctrs::s3_register("generics::tunable", "mars", tunable_mars)
    vctrs::s3_register("generics::tunable", "decision_tree", tunable_decision_tree)
    vctrs::s3_register("generics::tunable", "svm_poly", tunable_svm_poly)
    vctrs::s3_register("generics::tunable", "mlp", tunable_mlp)
  }

}

# nocov end

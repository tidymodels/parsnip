#' Attach model template objects to current environment
#'
#' `attach_parsnip_models()` will save a collection of `parsnip` model
#'  specifications to the an environment (the global environment, by default).
#'  These objects will have values of `tune"()` in most commonly used augment
#'  values (for use with the `tune` package). The naming convention is
#'  `{md}_{function}_{engine}` where `md` is an abbreviation of the model modes
#'  (e.g., "reg", or "cls"), `function` is the name of the model specification
#'  (e.g., "rand_forest"), and `engine` is the engine name.
#' @param pattern A character string for a regular expression used to filter
#' which specifications are attached.
#' @param mode One or more character strings for which collection of
#' specifications should be attached. Possible values are "all", "regression",
#' or "classification".
#' @param env An environment where the objects are attached.
#' @return NULL (invisibly) but multiple objects are added to the chosen
#' environment as a side-effect.
#' @export
attach_parsnip_models <- function(pattern = "*",
                                  mode = c("all", "regression", "classification"),
                                  env = rlang::global_env()) {
 mode <- match.arg(mode, c("regression", "classification"), several.ok = TRUE)
 tune <- rlang::call2("tune")

 if (any(mode %in% c("all", "regression"))) {
  reg_linear_reg_lm <-
   linear_reg() %>%
   set_engine("lm")

  reg_linear_reg_glmnet <-
   linear_reg(penalty = tune(), mixture = tune()) %>%
   set_engine("glmnet")

  reg_linear_reg_stan <-
   linear_reg(penalty = tune(), mixture = tune()) %>%
   set_engine("stan")

  reg_mlp_nnet <-
   mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
   set_engine("nnet") %>%
   set_mode("regression")

  reg_mars_earth <-
   mars(prod_degree = tune()) %>%
   set_engine("earth") %>%
   set_mode("regression")

  reg_svm_rbf_kernlab <-
   svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
   set_engine("kernlab") %>%
   set_mode("regression")

  reg_svm_rbf_liquidSVM <-
   svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
   set_engine("liquidSVM") %>%
   set_mode("regression")

  reg_svm_poly_kernlab <-
   svm_poly(cost = tune(), degree = tune()) %>%
   set_engine("kernlab") %>%
   set_mode("regression")

  reg_nearest_neighbor_kknn <-
   nearest_neighbor(neighbors = tune(), weight_func = tune()) %>%
   set_engine("kknn") %>%
   set_mode("regression")

  reg_decision_tree_rpart <-
   decision_tree(cost_complexity = tune(), min_n = tune()) %>%
   set_engine("rpart") %>%
   set_mode("regression")

  reg_rand_forest_ranger <-
   rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
   set_engine("ranger") %>%
   set_mode("regression")

  reg_rand_forest_randomForest <-
   rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
   set_engine("randomForest") %>%
   set_mode("regression")

  reg_boost_tree_xgboost <-
   boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(),
              min_n = tune(), sample_size = tune(), trees = tune()) %>%
   set_engine("xgboost") %>%
   set_mode("regression")
 }

 # ------------------------------------------------------------------------------

 if (any(mode %in% c("all", "classification"))) {
  cls_logistic_reg_lm <-
   logistic_reg() %>%
   set_engine("glm")

  cls_logistic_reg_glmnet <-
   logistic_reg(penalty = tune(), mixture = tune()) %>%
   set_engine("glmnet")

  cls_logistic_reg_stan <-
   logistic_reg() %>%
   set_engine("stan")

  cls_multinom_reg_glmnet <-
   logistic_reg(penalty = tune(), mixture = tune()) %>%
   set_engine("glmnet")

  cls_multinom_reg_stan <-
   logistic_reg() %>%
   set_engine("stan")

  cls_mlp_nnet <-
   mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
   set_engine("nnet") %>%
   set_mode("classification")

  cls_mars_earth <-
   mars(prod_degree = tune()) %>%
   set_engine("earth") %>%
   set_mode("classification")

  cls_svm_rbf_kernlab <-
   svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
   set_engine("kernlab") %>%
   set_mode("classification")

  cls_svm_rbf_liquidSVM <-
   svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
   set_engine("liquidSVM") %>%
   set_mode("classification")

  cls_svm_poly_kernlab <-
   svm_poly(cost = tune(), degree = tune()) %>%
   set_engine("kernlab") %>%
   set_mode("classification")

  cls_nearest_neighbor_kknn <-
   nearest_neighbor(neighbors = tune(), weight_func = tune()) %>%
   set_engine("kknn") %>%
   set_mode("classification")

  cls_decision_tree_rpart <-
   decision_tree(cost_complexity = tune(), min_n = tune()) %>%
   set_engine("rpart") %>%
   set_mode("classification")

  cls_decision_tree_C5.0 <-
   decision_tree(min_n = tune()) %>%
   set_engine("C5.0") %>%
   set_mode("classification")

  cls_rand_forest_ranger <-
   rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
   set_engine("ranger") %>%
   set_mode("classification")

  cls_rand_forest_randomForest <-
   rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
   set_engine("randomForest") %>%
   set_mode("classification")

  cls_boost_tree_xgboost <-
   boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(),
              min_n = tune(), sample_size = tune(), trees = tune()) %>%
   set_engine("xgboost") %>%
   set_mode("classification")

  cls_boost_tree_C5.0 <-
   boost_tree(min_n = tune(), trees = tune()) %>%
   set_engine("C5.0") %>%
   set_mode("classification")
 }

 obj_names <- ls(pattern = "(^reg_)|(^cls_)")
 obj_names <- grep(pattern, obj_names, value = TRUE)

 if (length(obj_names) > 0) {
  obj_values <- purrr::map(obj_names, ~get(.x, envir = rlang::current_env()))
  tmp <- purrr::map2(obj_names, obj_values, ~ assign(.x, .y, envir = env))
 } else {
    rlang::inform("No model specifications were attached.")
 }
 invisible(NULL)
}

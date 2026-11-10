#' Boosted trees via xgboost with native categorical predictors
#'
#' [xgboost::xgb.train()] creates a series of decision trees forming an
#' ensemble. Each tree depends on the results of previous trees. All trees in
#' the ensemble are combined to produce a final prediction. The `"xgboost3"`
#' engine passes factor predictors to xgboost as categorical features.
#'
#' @includeRmd man/rmd/boost_tree_xgboost3.md details
#'
#' @name details_boost_tree_xgboost3
#' @keywords internal
NULL

# See inst/README-DOCS.md for a description of how these files are processed

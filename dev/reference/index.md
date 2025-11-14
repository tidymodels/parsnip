# Package index

## Models

- [`auto_ml()`](https://parsnip.tidymodels.org/dev/reference/auto_ml.md)
  : Automatic Machine Learning
- [`bag_mars()`](https://parsnip.tidymodels.org/dev/reference/bag_mars.md)
  : Ensembles of MARS models
- [`bag_mlp()`](https://parsnip.tidymodels.org/dev/reference/bag_mlp.md)
  : Ensembles of neural networks
- [`bag_tree()`](https://parsnip.tidymodels.org/dev/reference/bag_tree.md)
  : Ensembles of decision trees
- [`bart()`](https://parsnip.tidymodels.org/dev/reference/bart.md) :
  Bayesian additive regression trees (BART)
- [`boost_tree()`](https://parsnip.tidymodels.org/dev/reference/boost_tree.md)
  : Boosted trees
- [`cubist_rules()`](https://parsnip.tidymodels.org/dev/reference/cubist_rules.md)
  : Cubist rule-based regression models
- [`C5_rules()`](https://parsnip.tidymodels.org/dev/reference/C5_rules.md)
  : C5.0 rule-based classification models
- [`decision_tree()`](https://parsnip.tidymodels.org/dev/reference/decision_tree.md)
  : Decision trees
- [`discrim_flexible()`](https://parsnip.tidymodels.org/dev/reference/discrim_flexible.md)
  : Flexible discriminant analysis
- [`discrim_linear()`](https://parsnip.tidymodels.org/dev/reference/discrim_linear.md)
  : Linear discriminant analysis
- [`discrim_quad()`](https://parsnip.tidymodels.org/dev/reference/discrim_quad.md)
  : Quadratic discriminant analysis
- [`discrim_regularized()`](https://parsnip.tidymodels.org/dev/reference/discrim_regularized.md)
  : Regularized discriminant analysis
- [`gen_additive_mod()`](https://parsnip.tidymodels.org/dev/reference/gen_additive_mod.md)
  : Generalized additive models (GAMs)
- [`glm_grouped()`](https://parsnip.tidymodels.org/dev/reference/glm_grouped.md)
  : Fit a grouped binomial outcome from a data set with case weights
- [`linear_reg()`](https://parsnip.tidymodels.org/dev/reference/linear_reg.md)
  : Linear regression
- [`logistic_reg()`](https://parsnip.tidymodels.org/dev/reference/logistic_reg.md)
  : Logistic regression
- [`mars()`](https://parsnip.tidymodels.org/dev/reference/mars.md) :
  Multivariate adaptive regression splines (MARS)
- [`mlp()`](https://parsnip.tidymodels.org/dev/reference/mlp.md) :
  Single layer neural network
- [`multinom_reg()`](https://parsnip.tidymodels.org/dev/reference/multinom_reg.md)
  : Multinomial regression
- [`naive_Bayes()`](https://parsnip.tidymodels.org/dev/reference/naive_Bayes.md)
  : Naive Bayes models
- [`nearest_neighbor()`](https://parsnip.tidymodels.org/dev/reference/nearest_neighbor.md)
  : K-nearest neighbors
- [`null_model()`](https://parsnip.tidymodels.org/dev/reference/null_model.md)
  : Null model
- [`pls()`](https://parsnip.tidymodels.org/dev/reference/pls.md) :
  Partial least squares (PLS)
- [`poisson_reg()`](https://parsnip.tidymodels.org/dev/reference/poisson_reg.md)
  : Poisson regression models
- [`proportional_hazards()`](https://parsnip.tidymodels.org/dev/reference/proportional_hazards.md)
  : Proportional hazards regression
- [`rand_forest()`](https://parsnip.tidymodels.org/dev/reference/rand_forest.md)
  : Random forest
- [`rule_fit()`](https://parsnip.tidymodels.org/dev/reference/rule_fit.md)
  : RuleFit models
- [`survival_reg()`](https://parsnip.tidymodels.org/dev/reference/survival_reg.md)
  : Parametric survival regression
- [`svm_linear()`](https://parsnip.tidymodels.org/dev/reference/svm_linear.md)
  : Linear support vector machines
- [`svm_poly()`](https://parsnip.tidymodels.org/dev/reference/svm_poly.md)
  : Polynomial support vector machines
- [`svm_rbf()`](https://parsnip.tidymodels.org/dev/reference/svm_rbf.md)
  : Radial basis function support vector machines

## Infrastructure

- [`autoplot(`*`<model_fit>`*`)`](https://parsnip.tidymodels.org/dev/reference/autoplot.model_fit.md)
  [`autoplot(`*`<glmnet>`*`)`](https://parsnip.tidymodels.org/dev/reference/autoplot.model_fit.md)
  : Create a ggplot for a model object
- [`add_rowindex()`](https://parsnip.tidymodels.org/dev/reference/add_rowindex.md)
  : Add a column of row numbers to a data frame
- [`augment(`*`<model_fit>`*`)`](https://parsnip.tidymodels.org/dev/reference/augment.md)
  : Augment data with predictions
- [`case_weights`](https://parsnip.tidymodels.org/dev/reference/case_weights.md)
  : Using case weights with parsnip
- [`case_weights_allowed()`](https://parsnip.tidymodels.org/dev/reference/case_weights_allowed.md)
  : Determine if case weights are used
- [`.cols()`](https://parsnip.tidymodels.org/dev/reference/descriptors.md)
  [`.preds()`](https://parsnip.tidymodels.org/dev/reference/descriptors.md)
  [`.obs()`](https://parsnip.tidymodels.org/dev/reference/descriptors.md)
  [`.lvls()`](https://parsnip.tidymodels.org/dev/reference/descriptors.md)
  [`.facts()`](https://parsnip.tidymodels.org/dev/reference/descriptors.md)
  [`.x()`](https://parsnip.tidymodels.org/dev/reference/descriptors.md)
  [`.y()`](https://parsnip.tidymodels.org/dev/reference/descriptors.md)
  [`.dat()`](https://parsnip.tidymodels.org/dev/reference/descriptors.md)
  : Data Set Characteristics Available when Fitting Models
- [`extract_spec_parsnip(`*`<model_fit>`*`)`](https://parsnip.tidymodels.org/dev/reference/extract-parsnip.md)
  [`extract_fit_engine(`*`<model_fit>`*`)`](https://parsnip.tidymodels.org/dev/reference/extract-parsnip.md)
  [`extract_parameter_set_dials(`*`<model_spec>`*`)`](https://parsnip.tidymodels.org/dev/reference/extract-parsnip.md)
  [`extract_parameter_dials(`*`<model_spec>`*`)`](https://parsnip.tidymodels.org/dev/reference/extract-parsnip.md)
  [`extract_fit_time(`*`<model_fit>`*`)`](https://parsnip.tidymodels.org/dev/reference/extract-parsnip.md)
  : Extract elements of a parsnip model object
- [`fit(`*`<model_spec>`*`)`](https://parsnip.tidymodels.org/dev/reference/fit.md)
  [`fit_xy(`*`<model_spec>`*`)`](https://parsnip.tidymodels.org/dev/reference/fit.md)
  : Fit a Model Specification to a Dataset
- [`reexports`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`autoplot`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`%>%`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`fit`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`fit_xy`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`tidy`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`glance`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`augment`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`required_pkgs`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`contr_one_hot`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`extract_spec_parsnip`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`extract_fit_engine`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`extract_parameter_set_dials`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`extract_parameter_dials`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`tune`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`frequency_weights`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`importance_weights`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`extract_fit_time`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`varying_args`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  : Objects exported from other packages
- [`control_parsnip()`](https://parsnip.tidymodels.org/dev/reference/control_parsnip.md)
  : Control the fit function
- [`glance(`*`<model_fit>`*`)`](https://parsnip.tidymodels.org/dev/reference/glance.model_fit.md)
  : Construct a single row summary "glance" of a model, fit, or other
  object
- [`matrix_to_quantile_pred()`](https://parsnip.tidymodels.org/dev/reference/matrix_to_quantile_pred.md)
  : Reformat quantile predictions
- [`model_fit`](https://parsnip.tidymodels.org/dev/reference/model_fit.md)
  : Model Fit Objects
- [`model_formula`](https://parsnip.tidymodels.org/dev/reference/model_formula.md)
  : Formulas with special terms in tidymodels
- [`model_spec`](https://parsnip.tidymodels.org/dev/reference/model_spec.md)
  : Model Specifications
- [`multi_predict()`](https://parsnip.tidymodels.org/dev/reference/multi_predict.md)
  : Model predictions across many sub-models
- [`parsnip_addin()`](https://parsnip.tidymodels.org/dev/reference/parsnip_addin.md)
  : Start an RStudio Addin that can write model specifications
- [`predict(`*`<model_fit>`*`)`](https://parsnip.tidymodels.org/dev/reference/predict.model_fit.md)
  [`predict_raw()`](https://parsnip.tidymodels.org/dev/reference/predict.model_fit.md)
  : Model predictions
- [`repair_call()`](https://parsnip.tidymodels.org/dev/reference/repair_call.md)
  : Repair a model call object
- [`set_args()`](https://parsnip.tidymodels.org/dev/reference/set_args.md)
  [`set_mode()`](https://parsnip.tidymodels.org/dev/reference/set_args.md)
  : Change elements of a model specification
- [`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md)
  : Declare a computational engine and specific arguments
- [`show_engines()`](https://parsnip.tidymodels.org/dev/reference/show_engines.md)
  : Display currently available engines for a model
- [`sparse_data`](https://parsnip.tidymodels.org/dev/reference/sparse_data.md)
  : Using sparse data with parsnip
- [`tidy(`*`<model_fit>`*`)`](https://parsnip.tidymodels.org/dev/reference/tidy.model_fit.md)
  : Turn a parsnip model object into a tidy tibble
- [`translate()`](https://parsnip.tidymodels.org/dev/reference/translate.md)
  : Resolve a Model Specification for a Computational Engine
- [`update(`*`<bag_mars>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<bag_mlp>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<bag_tree>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<bart>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<boost_tree>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<C5_rules>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<cubist_rules>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<decision_tree>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<discrim_flexible>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<discrim_linear>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<discrim_quad>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<discrim_regularized>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<gen_additive_mod>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<linear_reg>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<logistic_reg>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<mars>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<mlp>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<multinom_reg>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<naive_Bayes>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<nearest_neighbor>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<pls>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<poisson_reg>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<proportional_hazards>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<rand_forest>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<rule_fit>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<surv_reg>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<survival_reg>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<svm_linear>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<svm_poly>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  [`update(`*`<svm_rbf>`*`)`](https://parsnip.tidymodels.org/dev/reference/parsnip_update.md)
  : Updating a model specification
- [`ctree_train()`](https://parsnip.tidymodels.org/dev/reference/ctree_train.md)
  [`cforest_train()`](https://parsnip.tidymodels.org/dev/reference/ctree_train.md)
  : A wrapper function for conditional inference tree models

## Developer tools

- [`condense_control()`](https://parsnip.tidymodels.org/dev/reference/condense_control.md)
  : Condense control object into strictly smaller control object

- [`reexports`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`autoplot`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`%>%`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`fit`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`fit_xy`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`tidy`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`glance`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`augment`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`required_pkgs`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`contr_one_hot`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`extract_spec_parsnip`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`extract_fit_engine`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`extract_parameter_set_dials`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`extract_parameter_dials`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`tune`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`frequency_weights`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`importance_weights`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`extract_fit_time`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  [`varying_args`](https://parsnip.tidymodels.org/dev/reference/reexports.md)
  : Objects exported from other packages

- [`set_new_model()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  [`set_model_mode()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  [`set_model_engine()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  [`set_model_arg()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  [`set_dependency()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  [`get_dependency()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  [`set_fit()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  [`get_fit()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  [`set_pred()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  [`get_pred_type()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  [`show_model_info()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  [`pred_value_template()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  [`set_encoding()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  [`get_encoding()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  : Tools to Register Models

- [`maybe_matrix()`](https://parsnip.tidymodels.org/dev/reference/maybe_matrix.md)
  [`maybe_data_frame()`](https://parsnip.tidymodels.org/dev/reference/maybe_matrix.md)
  : Fuzzy conversions

- [`min_cols()`](https://parsnip.tidymodels.org/dev/reference/min_cols.md)
  [`min_rows()`](https://parsnip.tidymodels.org/dev/reference/min_cols.md)
  : Execution-time data dimension checks

- [`max_mtry_formula()`](https://parsnip.tidymodels.org/dev/reference/max_mtry_formula.md)
  :

  Determine largest value of mtry from formula. This function
  potentially caps the value of `mtry` based on a formula and data set.
  This is a safe approach for survival and/or multivariate models.

- [`required_pkgs(`*`<model_spec>`*`)`](https://parsnip.tidymodels.org/dev/reference/required_pkgs.model_spec.md)
  [`required_pkgs(`*`<model_fit>`*`)`](https://parsnip.tidymodels.org/dev/reference/required_pkgs.model_spec.md)
  : Determine required packages for a model

- [`req_pkgs()`](https://parsnip.tidymodels.org/dev/reference/req_pkgs.md)
  **\[deprecated\]** : Determine required packages for a model

- [`.extract_surv_status`](https://parsnip.tidymodels.org/dev/reference/dot-extract_surv_status.md)
  : Extract survival status

- [`.extract_surv_time`](https://parsnip.tidymodels.org/dev/reference/dot-extract_surv_time.md)
  : Extract survival time

- [`.model_param_name_key()`](https://parsnip.tidymodels.org/dev/reference/dot-model_param_name_key.md)
  : Translate names of model tuning parameters

- [`.get_prediction_column_names()`](https://parsnip.tidymodels.org/dev/reference/dot-get_prediction_column_names.md)
  : Obtain names of prediction columns for a fitted model or workflow

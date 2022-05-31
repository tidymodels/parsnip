# arguments (boost_tree)

    Code
      translate_args(basic_class %>% set_engine("xgboost"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $nthread
      [1] 1
      
      $verbose
      [1] 0
      

---

    Code
      translate_args(basic_class %>% set_engine("C5.0"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      

---

    Code
      translate_args(basic_class %>% set_engine("C5.0", rules = TRUE))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $rules
      [1] TRUE
      

---

    Code
      translate_args(basic_reg %>% set_engine("xgboost", print_every_n = 10L))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $print_every_n
      [1] 10
      
      $nthread
      [1] 1
      
      $verbose
      [1] 0
      

---

    Code
      translate_args(trees %>% set_engine("C5.0"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $trials
      [1] 15
      

---

    Code
      translate_args(trees %>% set_engine("xgboost"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $nrounds
      [1] 15
      
      $nthread
      [1] 1
      
      $verbose
      [1] 0
      

---

    Code
      translate_args(split_num %>% set_engine("C5.0"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $minCases
      [1] 15
      

---

    Code
      translate_args(split_num %>% set_engine("xgboost"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $min_child_weight
      [1] 15
      
      $nthread
      [1] 1
      
      $verbose
      [1] 0
      

# arguments (decision_tree)

    Code
      translate_args(basic_class %>% set_engine("rpart"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      

---

    Code
      translate_args(basic_class %>% set_engine("C5.0"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $trials
      [1] 1
      

---

    Code
      translate_args(basic_class %>% set_engine("C5.0", rules = TRUE))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $rules
      [1] TRUE
      
      $trials
      [1] 1
      

---

    Code
      translate_args(basic_reg %>% set_engine("rpart", model = TRUE))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $model
      [1] TRUE
      

---

    Code
      translate_args(cost_complexity %>% set_engine("rpart"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $cp
      [1] 15
      

---

    Code
      translate_args(split_num %>% set_engine("C5.0"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $minCases
      [1] 15
      
      $trials
      [1] 1
      

---

    Code
      translate_args(split_num %>% set_engine("rpart"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $minsplit
      min_rows(15, data)
      

# arguments (default)

    Code
      translate_args(basic %>% set_engine("parsnip"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      

---

    Code
      translate_args(basic %>% set_engine("parsnip", keepxy = FALSE))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $keepxy
      [1] FALSE
      

# arguments (linear_reg)

    Code
      translate_args(basic %>% set_engine("lm"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      

---

    Code
      translate_args(basic %>% set_engine("lm", model = FALSE))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $model
      [1] FALSE
      

---

    Code
      translate_args(basic %>% set_engine("glm"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $family
      stats::gaussian
      

---

    Code
      translate_args(basic %>% set_engine("glm", family = "quasipoisson"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $family
      [1] "quasipoisson"
      

---

    Code
      translate_args(basic %>% set_engine("stan"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $family
      stats::gaussian
      
      $refresh
      [1] 0
      

---

    Code
      translate_args(basic %>% set_engine("stan", chains = 1, iter = 5))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $chains
      [1] 1
      
      $iter
      [1] 5
      
      $family
      stats::gaussian
      
      $refresh
      [1] 0
      

---

    Code
      translate_args(basic %>% set_engine("spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weight_col
      missing_arg()
      

---

    Code
      translate_args(basic %>% set_engine("spark", max_iter = 20))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weight_col
      missing_arg()
      
      $max_iter
      [1] 20
      

---

    Code
      translate_args(basic %>% set_engine("glmnet"))
    Condition
      Error in `.check_glmnet_penalty_fit()`:
      ! For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      * There are 0 values for `penalty`.
      * To try multiple values for total regularization, use the tune package.
      * To predict multiple penalties, use `multi_predict()`

---

    Code
      translate_args(basic %>% set_engine("glmnet", path_values = 4:2))
    Condition
      Error in `.check_glmnet_penalty_fit()`:
      ! For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      * There are 0 values for `penalty`.
      * To try multiple values for total regularization, use the tune package.
      * To predict multiple penalties, use `multi_predict()`

---

    Code
      translate_args(mixture %>% set_engine("spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weight_col
      missing_arg()
      
      $elastic_net_param
      [1] 0.128
      

---

    Code
      translate_args(mixture_v %>% set_engine("spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weight_col
      missing_arg()
      
      $elastic_net_param
      tune()
      

---

    Code
      translate_args(mixture %>% set_engine("glmnet"))
    Condition
      Error in `.check_glmnet_penalty_fit()`:
      ! For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      * There are 0 values for `penalty`.
      * To try multiple values for total regularization, use the tune package.
      * To predict multiple penalties, use `multi_predict()`

---

    Code
      translate_args(penalty %>% set_engine("glmnet"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $family
      [1] "gaussian"
      

---

    Code
      translate_args(penalty %>% set_engine("glmnet", nlambda = 10))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $nlambda
      [1] 10
      
      $family
      [1] "gaussian"
      

---

    Code
      translate_args(penalty %>% set_engine("spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weight_col
      missing_arg()
      
      $reg_param
      [1] 1
      

# arguments (logistic_reg)

    Code
      translate_args(basic %>% set_engine("glm"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $family
      stats::binomial
      

---

    Code
      translate_args(basic %>% set_engine("glm", family = binomial(link = "probit")))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $family
      binomial(link = "probit")
      

---

    Code
      translate_args(basic %>% set_engine("glmnet"))
    Condition
      Error in `.check_glmnet_penalty_fit()`:
      ! For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      * There are 0 values for `penalty`.
      * To try multiple values for total regularization, use the tune package.
      * To predict multiple penalties, use `multi_predict()`

---

    Code
      translate_args(basic %>% set_engine("LiblineaR"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $wi
      missing_arg()
      
      $verbose
      [1] FALSE
      

---

    Code
      translate_args(basic %>% set_engine("LiblineaR", bias = 0))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $wi
      missing_arg()
      
      $bias
      [1] 0
      
      $verbose
      [1] FALSE
      

---

    Code
      translate_args(basic %>% set_engine("stan"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $family
      stats::binomial
      
      $refresh
      [1] 0
      

---

    Code
      translate_args(basic %>% set_engine("stan", chains = 1, iter = 5))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $chains
      [1] 1
      
      $iter
      [1] 5
      
      $family
      stats::binomial
      
      $refresh
      [1] 0
      

---

    Code
      translate_args(basic %>% set_engine("spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weight_col
      missing_arg()
      
      $family
      [1] "binomial"
      

---

    Code
      translate_args(basic %>% set_engine("spark", max_iter = 20))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weight_col
      missing_arg()
      
      $max_iter
      [1] 20
      
      $family
      [1] "binomial"
      

---

    Code
      translate_args(mixture %>% set_engine("glmnet"))
    Condition
      Error in `.check_glmnet_penalty_fit()`:
      ! For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      * There are 0 values for `penalty`.
      * To try multiple values for total regularization, use the tune package.
      * To predict multiple penalties, use `multi_predict()`

---

    Code
      translate_args(mixture %>% set_engine("spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weight_col
      missing_arg()
      
      $elastic_net_param
      [1] 0.128
      
      $family
      [1] "binomial"
      

---

    Code
      translate_args(penalty %>% set_engine("glmnet"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $family
      [1] "binomial"
      

---

    Code
      translate_args(penalty %>% set_engine("glmnet", nlambda = 10))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $nlambda
      [1] 10
      
      $family
      [1] "binomial"
      

---

    Code
      translate_args(penalty %>% set_engine("glmnet", path_values = 4:2))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $lambda
      4:2
      
      $family
      [1] "binomial"
      

---

    Code
      translate_args(penalty %>% set_engine("LiblineaR"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $wi
      missing_arg()
      
      $cost
      1/new_penalty
      
      $verbose
      [1] FALSE
      

---

    Code
      translate_args(penalty %>% set_engine("spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weight_col
      missing_arg()
      
      $reg_param
      [1] 1
      
      $family
      [1] "binomial"
      

---

    Code
      translate_args(mixture_v %>% set_engine("glmnet"))
    Condition
      Error in `.check_glmnet_penalty_fit()`:
      ! For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      * There are 0 values for `penalty`.
      * To try multiple values for total regularization, use the tune package.
      * To predict multiple penalties, use `multi_predict()`

---

    Code
      translate_args(mixture_v %>% set_engine("LiblineaR"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $wi
      missing_arg()
      
      $type
      tune()
      
      $verbose
      [1] FALSE
      

---

    Code
      translate_args(mixture_v %>% set_engine("spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weight_col
      missing_arg()
      
      $elastic_net_param
      tune()
      
      $family
      [1] "binomial"
      

# arguments (mars)

    Code
      translate_args(basic %>% set_engine("earth"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $keepxy
      [1] TRUE
      

---

    Code
      translate_args(basic %>% set_engine("earth", keepxy = FALSE))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $keepxy
      [1] FALSE
      

---

    Code
      translate_args(num_terms %>% set_engine("earth"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $nprune
      [1] 4
      
      $glm
      <quosure>
      expr: ^list(family = stats::binomial)
      env:  empty
      
      $keepxy
      [1] TRUE
      

---

    Code
      translate_args(prod_degree %>% set_engine("earth"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $degree
      [1] 1
      
      $keepxy
      [1] TRUE
      

---

    Code
      translate_args(prune_method_v %>% set_engine("earth"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $pmethod
      tune()
      
      $keepxy
      [1] TRUE
      

# arguments (mlp)

    Code
      translate_args(hidden_units %>% set_engine("nnet"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $size
      [1] 4
      
      $trace
      [1] FALSE
      
      $linout
      [1] TRUE
      

---

    Code
      translate_args(hidden_units %>% set_engine("keras"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $hidden_units
      [1] 4
      

---

    Code
      translate_args(no_hidden_units %>% set_engine("nnet"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $size
      [1] 5
      
      $trace
      [1] FALSE
      
      $linout
      [1] TRUE
      

---

    Code
      translate_args(no_hidden_units %>% set_engine("nnet", abstol = tune()))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $size
      [1] 5
      
      $abstol
      tune()
      
      $trace
      [1] FALSE
      
      $linout
      [1] TRUE
      

---

    Code
      translate_args(no_hidden_units %>% set_engine("keras", validation_split = 0.2))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $validation_split
      [1] 0.2
      

---

    Code
      translate_args(hess %>% set_engine("nnet", Hess = TRUE))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $size
      [1] 5
      
      $Hess
      [1] TRUE
      
      $trace
      [1] FALSE
      
      $linout
      [1] FALSE
      

---

    Code
      translate_args(all_args %>% set_engine("nnet"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $size
      [1] 4
      
      $decay
      [1] 1e-04
      
      $maxit
      [1] 2
      
      $trace
      [1] FALSE
      
      $linout
      [1] FALSE
      

---

    Code
      translate_args(all_args %>% set_engine("keras"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $hidden_units
      [1] 4
      
      $penalty
      [1] 1e-04
      
      $dropout
      [1] 0
      
      $epochs
      [1] 2
      
      $activation
      [1] "softmax"
      

# arguments (multinom_reg)

    Code
      translate_args(basic %>% set_engine("glmnet"))
    Condition
      Error in `.check_glmnet_penalty_fit()`:
      ! For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      * There are 0 values for `penalty`.
      * To try multiple values for total regularization, use the tune package.
      * To predict multiple penalties, use `multi_predict()`

---

    Code
      translate_args(mixture %>% set_engine("glmnet"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $alpha
      [1] 0.128
      
      $family
      [1] "multinomial"
      

---

    Code
      translate_args(penalty %>% set_engine("glmnet"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $family
      [1] "multinomial"
      

---

    Code
      translate_args(penalty %>% set_engine("glmnet", path_values = 4:2))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $lambda
      4:2
      
      $family
      [1] "multinomial"
      

---

    Code
      translate_args(penalty %>% set_engine("glmnet", nlambda = 10))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $nlambda
      [1] 10
      
      $family
      [1] "multinomial"
      

---

    Code
      translate_args(mixture_v %>% set_engine("glmnet"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $alpha
      tune()
      
      $family
      [1] "multinomial"
      

# arguments (nearest_neighbor)

    Code
      translate_args(basic %>% set_engine("kknn"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $ks
      min_rows(5, data, 5)
      

---

    Code
      translate_args(neighbors %>% set_engine("kknn"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $ks
      min_rows(2, data, 5)
      

---

    Code
      translate_args(neighbors %>% set_engine("kknn", scale = FALSE))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $ks
      min_rows(2, data, 5)
      
      $scale
      [1] FALSE
      

---

    Code
      translate_args(weight_func %>% set_engine("kknn"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      [1] "triangular"
      
      $ks
      min_rows(5, data, 5)
      

---

    Code
      translate_args(dist_power %>% set_engine("kknn"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $distance
      [1] 2
      
      $ks
      min_rows(5, data, 5)
      

# arguments (rand_forest)

    Code
      translate_args(basic %>% set_engine("randomForest", norm.votes = FALSE))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $norm.votes
      [1] FALSE
      

---

    Code
      translate_args(basic %>% set_engine("spark", min_info_gain = 2))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $type
      [1] "regression"
      
      $min_info_gain
      [1] 2
      
      $seed
      sample.int(10^5, 1)
      

---

    Code
      translate_args(mtry %>% set_engine("ranger"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $case.weights
      missing_arg()
      
      $mtry
      min_cols(4, x)
      
      $num.threads
      [1] 1
      
      $verbose
      [1] FALSE
      
      $seed
      sample.int(10^5, 1)
      

---

    Code
      translate_args(mtry %>% set_engine("randomForest"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $mtry
      min_cols(4, x)
      

---

    Code
      translate_args(mtry %>% set_engine("spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $type
      [1] "regression"
      
      $feature_subset_strategy
      [1] "4"
      
      $seed
      sample.int(10^5, 1)
      

---

    Code
      translate_args(trees %>% set_engine("ranger"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $case.weights
      missing_arg()
      
      $num.trees
      [1] 1000
      
      $num.threads
      [1] 1
      
      $verbose
      [1] FALSE
      
      $seed
      sample.int(10^5, 1)
      
      $probability
      [1] TRUE
      

---

    Code
      translate_args(trees %>% set_engine("ranger", importance = "impurity"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $case.weights
      missing_arg()
      
      $num.trees
      [1] 1000
      
      $importance
      [1] "impurity"
      
      $num.threads
      [1] 1
      
      $verbose
      [1] FALSE
      
      $seed
      sample.int(10^5, 1)
      
      $probability
      [1] TRUE
      

---

    Code
      translate_args(trees %>% set_engine("randomForest"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $ntree
      [1] 1000
      

---

    Code
      translate_args(trees %>% set_engine("spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $type
      [1] "classification"
      
      $num_trees
      [1] 1000
      
      $seed
      sample.int(10^5, 1)
      

---

    Code
      translate_args(min_n %>% set_engine("ranger"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $case.weights
      missing_arg()
      
      $min.node.size
      min_rows(5, x)
      
      $num.threads
      [1] 1
      
      $verbose
      [1] FALSE
      
      $seed
      sample.int(10^5, 1)
      

---

    Code
      translate_args(min_n %>% set_engine("randomForest"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $nodesize
      min_rows(5, x)
      

---

    Code
      translate_args(min_n %>% set_engine("spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $type
      [1] "regression"
      
      $min_instances_per_node
      min_rows(5, x)
      
      $seed
      sample.int(10^5, 1)
      

# arguments (surv_reg)

    Code
      translate_args(basic %>% set_engine("flexsurv"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      

---

    Code
      translate_args(basic %>% set_engine("flexsurv", cl = 0.99))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $cl
      [1] 0.99
      

---

    Code
      translate_args(normal %>% set_engine("flexsurv"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $dist
      [1] "lnorm"
      

---

    Code
      translate_args(dist_v %>% set_engine("flexsurv"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $dist
      tune()
      

# arguments (svm_linear)

    Code
      translate_args(basic %>% set_engine("LiblineaR"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $wi
      missing_arg()
      
      $type
      [1] 11
      
      $svr_eps
      [1] 0.1
      

---

    Code
      translate_args(basic %>% set_engine("LiblineaR", type = 12))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $wi
      missing_arg()
      
      $type
      [1] 12
      
      $svr_eps
      [1] 0.1
      

---

    Code
      translate_args(basic %>% set_engine("kernlab"))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      [1] "vanilladot"
      

---

    Code
      translate_args(basic %>% set_engine("kernlab", cross = 10))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $cross
      [1] 10
      
      $kernel
      [1] "vanilladot"
      

# arguments (svm_poly)

    Code
      translate_args(basic %>% set_engine("kernlab"))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      [1] "polydot"
      

---

    Code
      translate_args(basic %>% set_engine("kernlab", cross = 10))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $cross
      [1] 10
      
      $kernel
      [1] "polydot"
      

---

    Code
      translate_args(degree %>% set_engine("kernlab"))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      [1] "polydot"
      
      $kpar
      list(degree = 2)
      

---

    Code
      translate_args(degree_scale %>% set_engine("kernlab"))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      [1] "polydot"
      
      $kpar
      list(degree = 2, scale = 1.2)
      

# arguments (svm_rbf)

    Code
      translate_args(basic %>% set_engine("kernlab"))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      [1] "rbfdot"
      

---

    Code
      translate_args(basic %>% set_engine("kernlab", cross = 10))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $cross
      [1] 10
      
      $kernel
      [1] "rbfdot"
      

---

    Code
      translate_args(rbf_sigma %>% set_engine("kernlab"))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      [1] "rbfdot"
      
      $kpar
      list(sigma = 0.2)
      


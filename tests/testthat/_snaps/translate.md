# arguments (boost_tree)

    Code
      translate_args(set_engine(basic_class, "xgboost"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $nthread
      [1] 1
      
      $verbose
      [1] 0
      

---

    Code
      translate_args(set_engine(basic_class, "C5.0"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      

---

    Code
      translate_args(set_engine(basic_class, "C5.0", rules = TRUE))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $rules
      <quosure>
      expr: ^TRUE
      env:  empty
      

---

    Code
      translate_args(set_engine(basic_reg, "xgboost", print_every_n = 10L))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $print_every_n
      <quosure>
      expr: ^10L
      env:  empty
      
      $nthread
      [1] 1
      
      $verbose
      [1] 0
      

---

    Code
      translate_args(set_engine(trees, "C5.0"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $trials
      <quosure>
      expr: ^15
      env:  empty
      

---

    Code
      translate_args(set_engine(trees, "xgboost"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $nrounds
      <quosure>
      expr: ^15
      env:  empty
      
      $nthread
      [1] 1
      
      $verbose
      [1] 0
      

---

    Code
      translate_args(set_engine(split_num, "C5.0"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $minCases
      <quosure>
      expr: ^15
      env:  empty
      

---

    Code
      translate_args(set_engine(split_num, "xgboost"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $min_child_weight
      <quosure>
      expr: ^15
      env:  empty
      
      $nthread
      [1] 1
      
      $verbose
      [1] 0
      

# arguments (decision_tree)

    Code
      translate_args(set_engine(basic_class, "rpart"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      

---

    Code
      translate_args(set_engine(basic_class, "C5.0"))
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
      translate_args(set_engine(basic_class, "C5.0", rules = TRUE))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $rules
      <quosure>
      expr: ^TRUE
      env:  empty
      
      $trials
      [1] 1
      

---

    Code
      translate_args(set_engine(basic_reg, "rpart", model = TRUE))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $model
      <quosure>
      expr: ^TRUE
      env:  empty
      

---

    Code
      translate_args(set_engine(cost_complexity, "rpart"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $cp
      <quosure>
      expr: ^15
      env:  empty
      

---

    Code
      translate_args(set_engine(split_num, "C5.0"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $minCases
      <quosure>
      expr: ^15
      env:  empty
      
      $trials
      [1] 1
      

---

    Code
      translate_args(set_engine(split_num, "rpart"))
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
      translate_args(set_engine(basic, "parsnip"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      

---

    Code
      translate_args(set_engine(basic, "parsnip", keepxy = FALSE))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $keepxy
      <quosure>
      expr: ^FALSE
      env:  empty
      

# arguments (linear_reg)

    Code
      translate_args(set_engine(basic, "lm"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      

---

    Code
      translate_args(set_engine(basic, "lm", model = FALSE))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $model
      <quosure>
      expr: ^FALSE
      env:  empty
      

---

    Code
      translate_args(set_engine(basic, "glm"))
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
      translate_args(set_engine(basic, "glm", family = "quasipoisson"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $family
      <quosure>
      expr: ^"quasipoisson"
      env:  empty
      

---

    Code
      translate_args(set_engine(basic, "stan"))
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
      translate_args(set_engine(basic, "stan", chains = 1, iter = 5))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $chains
      <quosure>
      expr: ^1
      env:  empty
      
      $iter
      <quosure>
      expr: ^5
      env:  empty
      
      $family
      stats::gaussian
      
      $refresh
      [1] 0
      

---

    Code
      translate_args(set_engine(basic, "spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weights
      missing_arg()
      

---

    Code
      translate_args(set_engine(basic, "spark", max_iter = 20))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weights
      missing_arg()
      
      $max_iter
      <quosure>
      expr: ^20
      env:  empty
      

---

    Code
      translate_args(set_engine(basic, "glmnet"))
    Condition
      Error in `translate()`:
      x For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      ! There are 0 values for `penalty`.
      i To try multiple values for total regularization, use the tune package.
      i To predict multiple penalties, use `multi_predict()`.

---

    Code
      translate_args(set_engine(mixture, "spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weights
      missing_arg()
      
      $elastic_net_param
      <quosure>
      expr: ^0.128
      env:  empty
      

---

    Code
      translate_args(set_engine(mixture_v, "spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weights
      missing_arg()
      
      $elastic_net_param
      <quosure>
      expr: ^tune()
      env:  empty
      

---

    Code
      translate_args(set_engine(mixture, "glmnet"))
    Condition
      Error in `translate()`:
      x For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      ! There are 0 values for `penalty`.
      i To try multiple values for total regularization, use the tune package.
      i To predict multiple penalties, use `multi_predict()`.

---

    Code
      translate_args(set_engine(penalty, "glmnet"))
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
      translate_args(set_engine(penalty, "glmnet", nlambda = 10))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $nlambda
      <quosure>
      expr: ^10
      env:  empty
      
      $family
      [1] "gaussian"
      

---

    Code
      translate_args(set_engine(penalty, "glmnet", path_values = 4:2))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $lambda
      <quosure>
      expr: ^4:2
      env:  empty
      
      $family
      [1] "gaussian"
      

---

    Code
      translate_args(set_engine(penalty, "spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weights
      missing_arg()
      
      $reg_param
      <quosure>
      expr: ^1
      env:  empty
      

# arguments (logistic_reg)

    Code
      translate_args(set_engine(basic, "glm"))
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
      translate_args(set_engine(basic, "glm", family = binomial(link = "probit")))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $family
      <quosure>
      expr: ^binomial(link = "probit")
      env:  empty
      

---

    Code
      translate_args(set_engine(basic, "glmnet"))
    Condition
      Error in `translate()`:
      x For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      ! There are 0 values for `penalty`.
      i To try multiple values for total regularization, use the tune package.
      i To predict multiple penalties, use `multi_predict()`.

---

    Code
      translate_args(set_engine(basic, "LiblineaR"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $verbose
      [1] FALSE
      

---

    Code
      translate_args(set_engine(basic, "LiblineaR", bias = 0))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $bias
      <quosure>
      expr: ^0
      env:  empty
      
      $verbose
      [1] FALSE
      

---

    Code
      translate_args(set_engine(basic, "stan"))
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
      translate_args(set_engine(basic, "stan", chains = 1, iter = 5))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $chains
      <quosure>
      expr: ^1
      env:  empty
      
      $iter
      <quosure>
      expr: ^5
      env:  empty
      
      $family
      stats::binomial
      
      $refresh
      [1] 0
      

---

    Code
      translate_args(set_engine(basic, "spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weights
      missing_arg()
      
      $family
      [1] "binomial"
      

---

    Code
      translate_args(set_engine(basic, "spark", max_iter = 20))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weights
      missing_arg()
      
      $max_iter
      <quosure>
      expr: ^20
      env:  empty
      
      $family
      [1] "binomial"
      

---

    Code
      translate_args(set_engine(mixture, "glmnet"))
    Condition
      Error in `translate()`:
      x For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      ! There are 0 values for `penalty`.
      i To try multiple values for total regularization, use the tune package.
      i To predict multiple penalties, use `multi_predict()`.

---

    Code
      translate_args(set_engine(mixture, "spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weights
      missing_arg()
      
      $elastic_net_param
      <quosure>
      expr: ^0.128
      env:  empty
      
      $family
      [1] "binomial"
      

---

    Code
      translate_args(set_engine(penalty, "glmnet"))
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
      translate_args(set_engine(penalty, "glmnet", nlambda = 10))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $nlambda
      <quosure>
      expr: ^10
      env:  empty
      
      $family
      [1] "binomial"
      

---

    Code
      translate_args(set_engine(penalty, "glmnet", path_values = 4:2))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $lambda
      <quosure>
      expr: ^4:2
      env:  empty
      
      $family
      [1] "binomial"
      

---

    Code
      translate_args(set_engine(penalty, "LiblineaR"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $cost
      <quosure>
      expr: ^1
      env:  empty
      
      $verbose
      [1] FALSE
      

---

    Code
      translate_args(set_engine(penalty, "spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weights
      missing_arg()
      
      $reg_param
      <quosure>
      expr: ^1
      env:  empty
      
      $family
      [1] "binomial"
      

---

    Code
      translate_args(set_engine(mixture_v, "glmnet"))
    Condition
      Error in `translate()`:
      x For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      ! There are 0 values for `penalty`.
      i To try multiple values for total regularization, use the tune package.
      i To predict multiple penalties, use `multi_predict()`.

---

    Code
      translate_args(set_engine(mixture_v, "LiblineaR"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $type
      <quosure>
      expr: ^tune()
      env:  empty
      
      $verbose
      [1] FALSE
      

---

    Code
      translate_args(set_engine(mixture_v, "spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $weights
      missing_arg()
      
      $elastic_net_param
      <quosure>
      expr: ^tune()
      env:  empty
      
      $family
      [1] "binomial"
      

# arguments (mars)

    Code
      translate_args(set_engine(basic, "earth"))
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
      translate_args(set_engine(basic, "earth", keepxy = FALSE))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $keepxy
      <quosure>
      expr: ^FALSE
      env:  empty
      

---

    Code
      translate_args(set_engine(num_terms, "earth"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $nprune
      <quosure>
      expr: ^4
      env:  empty
      
      $glm
      <quosure>
      expr: ^list(family = stats::binomial)
      env:  empty
      
      $keepxy
      [1] TRUE
      

---

    Code
      translate_args(set_engine(prod_degree, "earth"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $degree
      <quosure>
      expr: ^1
      env:  empty
      
      $keepxy
      [1] TRUE
      

---

    Code
      translate_args(set_engine(prune_method_v, "earth"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $pmethod
      <quosure>
      expr: ^tune()
      env:  empty
      
      $keepxy
      [1] TRUE
      

# arguments (mlp)

    Code
      translate_args(set_engine(hidden_units, "nnet"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $size
      <quosure>
      expr: ^4
      env:  empty
      
      $trace
      [1] FALSE
      
      $linout
      [1] TRUE
      

---

    Code
      translate_args(set_engine(hidden_units, "keras"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $hidden_units
      <quosure>
      expr: ^4
      env:  empty
      

---

    Code
      translate_args(set_engine(no_hidden_units, "nnet"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $size
      [1] 5
      
      $trace
      [1] FALSE
      
      $linout
      [1] TRUE
      

---

    Code
      translate_args(set_engine(no_hidden_units, "nnet", abstol = tune()))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $size
      [1] 5
      
      $abstol
      <quosure>
      expr: ^tune()
      env:  empty
      
      $trace
      [1] FALSE
      
      $linout
      [1] TRUE
      

---

    Code
      translate_args(set_engine(no_hidden_units, "keras", validation_split = 0.2))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $validation_split
      <quosure>
      expr: ^0.2
      env:  empty
      

---

    Code
      translate_args(set_engine(hess, "nnet", Hess = TRUE))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $size
      [1] 5
      
      $Hess
      <quosure>
      expr: ^TRUE
      env:  empty
      
      $trace
      [1] FALSE
      
      $linout
      [1] FALSE
      

---

    Code
      translate_args(set_engine(all_args, "nnet"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $size
      <quosure>
      expr: ^4
      env:  empty
      
      $decay
      <quosure>
      expr: ^1e-04
      env:  empty
      
      $maxit
      <quosure>
      expr: ^2
      env:  empty
      
      $trace
      [1] FALSE
      
      $linout
      [1] FALSE
      

---

    Code
      translate_args(set_engine(all_args, "keras"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $hidden_units
      <quosure>
      expr: ^4
      env:  empty
      
      $penalty
      <quosure>
      expr: ^1e-04
      env:  empty
      
      $dropout
      <quosure>
      expr: ^0
      env:  empty
      
      $epochs
      <quosure>
      expr: ^2
      env:  empty
      
      $activation
      <quosure>
      expr: ^"softmax"
      env:  empty
      

# arguments (multinom_reg)

    Code
      translate_args(set_engine(basic, "glmnet"))
    Condition
      Error in `translate()`:
      x For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      ! There are 0 values for `penalty`.
      i To try multiple values for total regularization, use the tune package.
      i To predict multiple penalties, use `multi_predict()`.

---

    Code
      translate_args(set_engine(mixture, "glmnet"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $alpha
      <quosure>
      expr: ^0.128
      env:  empty
      
      $family
      [1] "multinomial"
      

---

    Code
      translate_args(set_engine(penalty, "glmnet"))
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
      translate_args(set_engine(penalty, "glmnet", path_values = 4:2))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $lambda
      <quosure>
      expr: ^4:2
      env:  empty
      
      $family
      [1] "multinomial"
      

---

    Code
      translate_args(set_engine(penalty, "glmnet", nlambda = 10))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $nlambda
      <quosure>
      expr: ^10
      env:  empty
      
      $family
      [1] "multinomial"
      

---

    Code
      translate_args(set_engine(mixture_v, "glmnet"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $alpha
      <quosure>
      expr: ^tune()
      env:  empty
      
      $family
      [1] "multinomial"
      

# arguments (nearest_neighbor)

    Code
      translate_args(set_engine(basic, "kknn"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $ks
      min_rows(5, data, 5)
      

---

    Code
      translate_args(set_engine(neighbors, "kknn"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $ks
      min_rows(2, data, 5)
      

---

    Code
      translate_args(set_engine(neighbors, "kknn", scale = FALSE))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $ks
      min_rows(2, data, 5)
      
      $scale
      <quosure>
      expr: ^FALSE
      env:  empty
      

---

    Code
      translate_args(set_engine(weight_func, "kknn"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      <quosure>
      expr: ^"triangular"
      env:  empty
      
      $ks
      min_rows(5, data, 5)
      

---

    Code
      translate_args(set_engine(dist_power, "kknn"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $distance
      <quosure>
      expr: ^2
      env:  empty
      
      $ks
      min_rows(5, data, 5)
      

# arguments (proportional_hazards)

    Code
      translate_args(basic)
    Output
      list()

---

    Code
      translate_args(basic_incomplete)
    Condition
      Error in `translate()`:
      x For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      ! There are 0 values for `penalty`.
      i To try multiple values for total regularization, use the tune package.
      i To predict multiple penalties, use `multi_predict()`.

# arguments (rand_forest)

    Code
      translate_args(set_engine(basic, "randomForest", norm.votes = FALSE))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $norm.votes
      <quosure>
      expr: ^FALSE
      env:  empty
      

---

    Code
      translate_args(set_engine(basic, "spark", min_info_gain = 2))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $type
      [1] "regression"
      
      $min_info_gain
      <quosure>
      expr: ^2
      env:  empty
      
      $seed
      sample.int(10^5, 1)
      

---

    Code
      translate_args(set_engine(mtry, "ranger"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $mtry
      min_cols(~4, x)
      
      $num.threads
      [1] 1
      
      $verbose
      [1] FALSE
      
      $seed
      sample.int(10^5, 1)
      

---

    Code
      translate_args(set_engine(mtry, "randomForest"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $mtry
      min_cols(~4, x)
      

---

    Code
      translate_args(set_engine(mtry, "spark"))
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
      translate_args(set_engine(trees, "ranger"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $num.trees
      <quosure>
      expr: ^1000
      env:  empty
      
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
      translate_args(set_engine(trees, "ranger", importance = "impurity"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $num.trees
      <quosure>
      expr: ^1000
      env:  empty
      
      $importance
      <quosure>
      expr: ^"impurity"
      env:  empty
      
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
      translate_args(set_engine(trees, "randomForest"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $ntree
      <quosure>
      expr: ^1000
      env:  empty
      

---

    Code
      translate_args(set_engine(trees, "spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $type
      [1] "classification"
      
      $num_trees
      <quosure>
      expr: ^1000
      env:  empty
      
      $seed
      sample.int(10^5, 1)
      

---

    Code
      translate_args(set_engine(min_n, "ranger"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $weights
      missing_arg()
      
      $min.node.size
      min_rows(~5, x)
      
      $num.threads
      [1] 1
      
      $verbose
      [1] FALSE
      
      $seed
      sample.int(10^5, 1)
      

---

    Code
      translate_args(set_engine(min_n, "randomForest"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $nodesize
      min_rows(~5, x)
      

---

    Code
      translate_args(set_engine(min_n, "spark"))
    Output
      $x
      missing_arg()
      
      $formula
      missing_arg()
      
      $type
      [1] "regression"
      
      $min_instances_per_node
      min_rows(~5, x)
      
      $seed
      sample.int(10^5, 1)
      

# arguments (surv_reg)

    Code
      translate_args(set_engine(basic, "flexsurv"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      

---

    Code
      translate_args(set_engine(basic, "flexsurv", cl = 0.99))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $cl
      <quosure>
      expr: ^0.99
      env:  empty
      

---

    Code
      translate_args(set_engine(normal, "flexsurv"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $dist
      <quosure>
      expr: ^"lnorm"
      env:  empty
      

---

    Code
      translate_args(set_engine(dist_v, "flexsurv"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $dist
      <quosure>
      expr: ^tune()
      env:  empty
      

# arguments (survival_reg)

    Code
      translate_args(basic)
    Output
      list()

# arguments (svm_linear)

    Code
      translate_args(set_engine(basic, "LiblineaR"))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $type
      [1] 11
      
      $svr_eps
      [1] 0.1
      

---

    Code
      translate_args(set_engine(basic, "LiblineaR", type = 12))
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $type
      <quosure>
      expr: ^12
      env:  empty
      
      $svr_eps
      [1] 0.1
      

---

    Code
      translate_args(set_engine(basic, "kernlab"))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      [1] "vanilladot"
      

---

    Code
      translate_args(set_engine(basic, "kernlab", cross = 10))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $cross
      <quosure>
      expr: ^10
      env:  empty
      
      $kernel
      [1] "vanilladot"
      

# arguments (svm_poly)

    Code
      translate_args(set_engine(basic, "kernlab"))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      [1] "polydot"
      

---

    Code
      translate_args(set_engine(basic, "kernlab", cross = 10))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $cross
      <quosure>
      expr: ^10
      env:  empty
      
      $kernel
      [1] "polydot"
      

---

    Code
      translate_args(set_engine(degree, "kernlab"))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      [1] "polydot"
      
      $kpar
      list(degree = ~2)
      

---

    Code
      translate_args(set_engine(degree_scale, "kernlab"))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      [1] "polydot"
      
      $kpar
      list(degree = ~2, scale = ~1.2)
      

# arguments (svm_rbf)

    Code
      translate_args(set_engine(basic, "kernlab"))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      [1] "rbfdot"
      

---

    Code
      translate_args(set_engine(basic, "kernlab", cross = 10))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $cross
      <quosure>
      expr: ^10
      env:  empty
      
      $kernel
      [1] "rbfdot"
      

---

    Code
      translate_args(set_engine(rbf_sigma, "kernlab"))
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $kernel
      [1] "rbfdot"
      
      $kpar
      list(sigma = ~0.2)
      

# translate tuning paramter names

    Code
      .model_param_name_key(mod)
    Output
      # A tibble: 2 x 3
        user            parsnip engine          
        <chr>           <chr>   <chr>           
      1 number of trees trees   nrounds         
      2 min_n           min_n   min_child_weight

---

    Code
      .model_param_name_key(mod, as_tibble = FALSE)
    Output
      $user_to_parsnip
                  trees             min_n 
      "number of trees"           "min_n" 
      
      $parsnip_to_engine
               nrounds min_child_weight 
               "trees"          "min_n" 
      

---

    Code
      .model_param_name_key(linear_reg())
    Output
      # A tibble: 0 x 3
      # i 3 variables: user <chr>, parsnip <chr>, engine <chr>

---

    Code
      .model_param_name_key(linear_reg(), as_tibble = FALSE)
    Output
      $user_to_parsnip
      named character(0)
      
      $parsnip_to_engine
      named character(0)
      

---

    `object` should be a model specification or workflow.


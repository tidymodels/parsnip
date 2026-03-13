# tunable parameters for rand_forest + ranger

    Code
      display_tunable_call_info(set_engine(rand_forest(), "ranger"))
    Output
      mtry                      | pkg: dials, fun: mtry | main
      trees                     | pkg: dials, fun: trees | main
      min_n                     | pkg: dials, fun: min_n | main
      regularization.factor     | pkg: dials, fun: regularization_factor | engine
      regularization.usedepth   | pkg: dials, fun: regularize_depth | engine
      alpha                     | pkg: dials, fun: significance_threshold | engine
      minprop                   | pkg: dials, fun: lower_quantile | engine
      splitrule                 | pkg: dials, fun: splitting_rule | engine
      num.random.splits         | pkg: dials, fun: num_random_splits | engine

# tunable parameters for rand_forest + randomForest

    Code
      display_tunable_call_info(set_engine(rand_forest(), "randomForest"))
    Output
      mtry                      | pkg: dials, fun: mtry | main
      trees                     | pkg: dials, fun: trees | main
      min_n                     | pkg: dials, fun: min_n | main
      maxnodes                  | pkg: dials, fun: max_nodes | engine

# tunable parameters for rand_forest + spark

    Code
      display_tunable_call_info(set_engine(rand_forest(), "spark"))
    Output
      mtry                      | pkg: dials, fun: mtry | main
      trees                     | pkg: dials, fun: trees | main
      min_n                     | pkg: dials, fun: min_n | main

# tunable parameters for rand_forest + partykit

    Code
      display_tunable_call_info(set_engine(rand_forest(), "partykit"))
    Output
      mincriterion              | pkg: dials, fun: conditional_min_criterion | engine
      teststat                  | pkg: dials, fun: conditional_test_statistic | engine
      testtype                  | pkg: dials, fun: conditional_test_type | engine

# tunable parameters for rand_forest + aorsf

    Code
      display_tunable_call_info(set_engine(rand_forest(), "aorsf"))
    Output
      split_min_stat            | pkg: dials, fun: conditional_min_criterion | engine


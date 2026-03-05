# tunable parameters for boost_tree + xgboost

    Code
      display_tunable_call_info(set_engine(boost_tree(), "xgboost"))
    Output
      tree_depth                | pkg: dials, fun: tree_depth | main
      trees                     | pkg: dials, fun: trees | main
      learn_rate                | pkg: dials, fun: learn_rate, range: c(-3.00, -0.50) | main
      mtry                      | pkg: dials, fun: mtry | main
      min_n                     | pkg: dials, fun: min_n | main
      loss_reduction            | pkg: dials, fun: loss_reduction | main
      sample_size               | pkg: dials, fun: sample_prop, range: c(0.50, 1.00) | main
      stop_iter                 | pkg: dials, fun: stop_iter | main

# tunable parameters for boost_tree + C5.0

    Code
      display_tunable_call_info(set_engine(boost_tree(), "C5.0"))
    Output
      trees                     | pkg: dials, fun: trees, range: c(  1.00, 100.00) | main
      min_n                     | pkg: dials, fun: min_n | main
      sample_size               | pkg: dials, fun: sample_prop, range: c(0.50, 1.00) | main

# tunable parameters for boost_tree + spark

    Code
      display_tunable_call_info(set_engine(boost_tree(), "spark"))
    Output
      tree_depth                | pkg: dials, fun: tree_depth | main
      trees                     | pkg: dials, fun: trees | main
      learn_rate                | pkg: dials, fun: learn_rate | main
      mtry                      | pkg: dials, fun: mtry | main
      min_n                     | pkg: dials, fun: min_n | main
      loss_reduction            | pkg: dials, fun: loss_reduction | main
      sample_size               | pkg: dials, fun: sample_size | main

# tunable parameters for boost_tree + lightgbm

    Code
      display_tunable_call_info(set_engine(boost_tree(), "lightgbm"))
    Output
      No tunable parameters.

# tunable parameters for boost_tree + catboost

    Code
      display_tunable_call_info(set_engine(boost_tree(), "catboost"))
    Output
      No tunable parameters.


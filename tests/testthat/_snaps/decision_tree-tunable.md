# tunable parameters for decision_tree + rpart

    Code
      display_tunable_call_info(set_engine(decision_tree(), "rpart"))
    Output
      tree_depth                | pkg: dials, fun: tree_depth | main
      min_n                     | pkg: dials, fun: min_n | main
      cost_complexity           | pkg: dials, fun: cost_complexity | main

# tunable parameters for decision_tree + C5.0

    Code
      display_tunable_call_info(set_engine(decision_tree(), "C5.0"))
    Output
      min_n                     | pkg: dials, fun: min_n | main
      CF                        | pkg: dials, fun: confidence_factor | engine
      noGlobalPruning           | pkg: dials, fun: no_global_pruning | engine
      winnow                    | pkg: dials, fun: predictor_winnowing | engine
      fuzzyThreshold            | pkg: dials, fun: fuzzy_thresholding | engine
      bands                     | pkg: dials, fun: rule_bands | engine

# tunable parameters for decision_tree + spark

    Code
      display_tunable_call_info(set_engine(decision_tree(), "spark"))
    Output
      tree_depth                | pkg: dials, fun: tree_depth | main
      min_n                     | pkg: dials, fun: min_n | main

# tunable parameters for decision_tree + partykit

    Code
      display_tunable_call_info(set_engine(decision_tree(), "partykit"))
    Output
      mincriterion              | pkg: dials, fun: conditional_min_criterion | engine
      teststat                  | pkg: dials, fun: conditional_test_statistic | engine
      testtype                  | pkg: dials, fun: conditional_test_type | engine


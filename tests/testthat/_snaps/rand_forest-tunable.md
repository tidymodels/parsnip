# tunable parameters for rand_forest + ranger

    Code
      display_tunable_call_info(set_engine(rand_forest(), "ranger"))
    Output
      mtry                      | pkg: dials, fun: mtry | main
      trees                     | pkg: dials, fun: trees | main
      min_n                     | pkg: dials, fun: min_n | main

# tunable parameters for rand_forest + randomForest

    Code
      display_tunable_call_info(set_engine(rand_forest(), "randomForest"))
    Output
      mtry                      | pkg: dials, fun: mtry | main
      trees                     | pkg: dials, fun: trees | main
      min_n                     | pkg: dials, fun: min_n | main

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
      No tunable parameters.

# tunable parameters for rand_forest + aorsf

    Code
      display_tunable_call_info(set_engine(rand_forest(), "aorsf"))
    Output
      No tunable parameters.


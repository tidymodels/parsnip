# tunable parameters for multinom_reg + glmnet

    Code
      display_tunable_call_info(set_engine(multinom_reg(), "glmnet"))
    Output
      penalty                   | pkg: dials, fun: penalty | main
      mixture                   | pkg: dials, fun: mixture, range: c(0.05, 1.00) | main

# tunable parameters for multinom_reg + spark

    Code
      display_tunable_call_info(set_engine(multinom_reg(), "spark"))
    Output
      penalty                   | pkg: dials, fun: penalty | main
      mixture                   | pkg: dials, fun: mixture | main

# tunable parameters for multinom_reg + keras

    Code
      display_tunable_call_info(set_engine(multinom_reg(), "keras"))
    Output
      penalty                   | pkg: dials, fun: penalty | main

# tunable parameters for multinom_reg + nnet

    Code
      display_tunable_call_info(set_engine(multinom_reg(), "nnet"))
    Output
      penalty                   | pkg: dials, fun: penalty | main

# tunable parameters for multinom_reg + brulee

    Code
      display_tunable_call_info(set_engine(multinom_reg(), "brulee"))
    Output
      epochs                    | pkg: dials, fun: epochs, range: c(  5, 500) | engine
      penalty                   | pkg: dials, fun: penalty | main
      mixture                   | pkg: dials, fun: mixture | main
      learn_rate                | pkg: dials, fun: learn_rate, range: c(-3.00, -0.20) | engine
      momentum                  | pkg: dials, fun: momentum, range: c(0.00, 0.99) | engine
      batch_size                | pkg: dials, fun: batch_size, range: c(3, 8) | engine
      class_weights             | pkg: dials, fun: class_weights | engine
      stop_iter                 | pkg: dials, fun: stop_iter | engine
      rate_schedule             | pkg: dials, fun: rate_schedule, values: none, decay_time, decay_expo, cyclic, step | engine


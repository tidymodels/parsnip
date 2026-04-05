# tunable parameters for mlp + keras

    Code
      display_tunable_call_info(set_engine(mlp(), "keras"))
    Output
      hidden_units              | pkg: dials, fun: hidden_units | main
      penalty                   | pkg: dials, fun: penalty | main
      dropout                   | pkg: dials, fun: dropout | main
      epochs                    | pkg: dials, fun: epochs | main
      activation                | pkg: dials, fun: activation | main

# tunable parameters for mlp + nnet

    Code
      display_tunable_call_info(set_engine(mlp(), "nnet"))
    Output
      hidden_units              | pkg: dials, fun: hidden_units | main
      penalty                   | pkg: dials, fun: penalty | main
      epochs                    | pkg: dials, fun: epochs | main

# tunable parameters for mlp + brulee

    Code
      display_tunable_call_info(set_engine(mlp(), "brulee"))
    Output
      epochs                    | pkg: dials, fun: epochs, range: c(  5, 500) | main
      hidden_units              | pkg: dials, fun: hidden_units, range: c( 2, 50) | main
      activation                | pkg: dials, fun: activation, values: relu, tanh, elu, log_sigmoid, tanhshrink | main
      penalty                   | pkg: dials, fun: penalty | main
      mixture                   | pkg: dials, fun: mixture | engine
      dropout                   | pkg: dials, fun: dropout | main
      learn_rate                | pkg: dials, fun: learn_rate, range: c(-3.00, -0.20) | main
      momentum                  | pkg: dials, fun: momentum, range: c(0.00, 0.99) | engine
      batch_size                | pkg: dials, fun: batch_size, range: c(3, 8) | engine
      class_weights             | pkg: dials, fun: class_weights | engine
      stop_iter                 | pkg: dials, fun: stop_iter | engine
      rate_schedule             | pkg: dials, fun: rate_schedule, values: none, decay_time, decay_expo, cyclic, step | engine

# tunable parameters for mlp + brulee_two_layer

    Code
      display_tunable_call_info(set_engine(mlp(), "brulee_two_layer"))
    Output
      epochs                    | pkg: dials, fun: epochs, range: c(  5, 500) | main
      hidden_units              | pkg: dials, fun: hidden_units, range: c( 2, 50) | main
      hidden_units_2            | pkg: dials, fun: hidden_units_2, range: c( 2, 50) | engine
      activation                | pkg: dials, fun: activation, values: relu, tanh, elu, log_sigmoid, tanhshrink | main
      activation_2              | pkg: dials, fun: activation_2, values: relu, tanh, elu, log_sigmoid, tanhshrink | engine
      penalty                   | pkg: dials, fun: penalty | main
      mixture                   | pkg: dials, fun: mixture | engine
      dropout                   | pkg: dials, fun: dropout | main
      learn_rate                | pkg: dials, fun: learn_rate, range: c(-3.00, -0.20) | main
      momentum                  | pkg: dials, fun: momentum, range: c(0.00, 0.99) | engine
      batch_size                | pkg: dials, fun: batch_size, range: c(3, 8) | engine
      class_weights             | pkg: dials, fun: class_weights | engine
      stop_iter                 | pkg: dials, fun: stop_iter | engine
      rate_schedule             | pkg: dials, fun: rate_schedule, values: none, decay_time, decay_expo, cyclic, step | engine


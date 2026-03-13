# tunable parameters for linear_reg + lm

    Code
      display_tunable_call_info(set_engine(linear_reg(), "lm"))
    Output
      No tunable parameters.

# tunable parameters for linear_reg + glm

    Code
      display_tunable_call_info(set_engine(linear_reg(), "glm"))
    Output
      No tunable parameters.

# tunable parameters for linear_reg + glmnet

    Code
      display_tunable_call_info(set_engine(linear_reg(), "glmnet"))
    Output
      penalty                   | pkg: dials, fun: penalty | main
      mixture                   | pkg: dials, fun: mixture, range: c(0.05, 1.00) | main

# tunable parameters for linear_reg + stan

    Code
      display_tunable_call_info(set_engine(linear_reg(), "stan"))
    Output
      No tunable parameters.

# tunable parameters for linear_reg + spark

    Code
      display_tunable_call_info(set_engine(linear_reg(), "spark"))
    Output
      penalty                   | pkg: dials, fun: penalty | main
      mixture                   | pkg: dials, fun: mixture | main

# tunable parameters for linear_reg + keras

    Code
      display_tunable_call_info(set_engine(linear_reg(), "keras"))
    Output
      penalty                   | pkg: dials, fun: penalty | main

# tunable parameters for linear_reg + brulee

    Code
      display_tunable_call_info(set_engine(linear_reg(), "brulee"))
    Output
      epochs                    | pkg: dials, fun: epochs, range: c(  5, 500) | engine
      penalty                   | pkg: dials, fun: penalty | main
      mixture                   | pkg: dials, fun: mixture | main
      learn_rate                | pkg: dials, fun: learn_rate, range: c(-3.00, -0.20) | engine
      momentum                  | pkg: dials, fun: momentum, range: c(0.00, 0.99) | engine
      batch_size                | pkg: dials, fun: batch_size, range: c(3, 8) | engine
      stop_iter                 | pkg: dials, fun: stop_iter | engine
      rate_schedule             | pkg: dials, fun: rate_schedule, values: none, decay_time, decay_expo, cyclic, step | engine

# tunable parameters for linear_reg + quantreg

    Code
      display_tunable_call_info(set_engine(linear_reg(), "quantreg"))
    Output
      No tunable parameters.

# tunable parameters for linear_reg + gee

    Code
      display_tunable_call_info(set_engine(linear_reg(), "gee"))
    Output
      No tunable parameters.

# tunable parameters for linear_reg + lme

    Code
      display_tunable_call_info(set_engine(linear_reg(), "lme"))
    Output
      No tunable parameters.

# tunable parameters for linear_reg + lmer

    Code
      display_tunable_call_info(set_engine(linear_reg(), "lmer"))
    Output
      No tunable parameters.

# tunable parameters for linear_reg + glmer

    Code
      display_tunable_call_info(set_engine(linear_reg(), "glmer"))
    Output
      No tunable parameters.

# tunable parameters for linear_reg + gls

    Code
      display_tunable_call_info(set_engine(linear_reg(), "gls"))
    Output
      No tunable parameters.

# tunable parameters for linear_reg + stan_glmer

    Code
      display_tunable_call_info(set_engine(linear_reg(), "stan_glmer"))
    Output
      No tunable parameters.


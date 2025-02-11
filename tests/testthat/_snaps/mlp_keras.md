# keras execution, classification

    Code
      res <- parsnip::fit(hpc_keras, class ~ novar, data = hpc, control = ctrl)
    Condition
      Error:
      ! object 'novar' not found

# all keras activation functions

    Code
      mlp(mode = "classification", hidden_units = 2, penalty = 0.01, epochs = 2,
        activation = "invalid") %>% set_engine("keras", verbose = 0) %>% parsnip::fit(
        Class ~ A + B, data = modeldata::two_class_dat)
    Condition
      Error in `parsnip::keras_mlp()`:
      ! `activation` should be one of: elu, exponential, gelu, hard_sigmoid, linear, relu, selu, sigmoid, softmax, softplus, softsign, swish, and tanh, not "invalid".


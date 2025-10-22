# keras execution, classification

    Code
      res <- parsnip::fit(hpc_keras, class ~ novar, data = hpc, control = ctrl)
    Condition
      Error:
      ! object 'novar' not found

# all keras activation functions

    Code
      parsnip::fit(set_engine(mlp(mode = "classification", hidden_units = 2, penalty = 0.01,
        epochs = 2, activation = "invalid"), "keras", verbose = 0), Class ~ A + B,
      data = modeldata::two_class_dat)
    Condition
      Error in `parsnip::keras_mlp()`:
      ! `activation` should be one of: elu, exponential, gelu, hardsigmoid, linear, relu, selu, sigmoid, softmax, softplus, softsign, swish, and tanh, not "invalid".


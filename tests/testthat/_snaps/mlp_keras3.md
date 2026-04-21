# keras3 execution, classification

    Code
      res <- parsnip::fit(hpc_keras3, class ~ novar, data = hpc, control = ctrl)
    Condition
      Error:
      ! object 'novar' not found

# all keras3 activation functions

    Code
      parsnip::fit(set_engine(mlp(mode = "classification", hidden_units = 2, penalty = 0.01,
        epochs = 2, activation = "invalid"), "keras3", verbose = 0), Class ~ A + B,
      data = modeldata::two_class_dat)
    Condition
      Error in `parsnip::keras3_mlp()`:
      ! `activation` should be one of: elu, exponential, gelu, hardsigmoid, linear, relu, selu, sigmoid, softmax, softplus, softsign, swish, and tanh, not "invalid".


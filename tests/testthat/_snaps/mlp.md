# updating

    Code
      mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE) %>%
        update(hidden_units = tune(), Hess = tune())
    Output
      Single Layer Neural Network Model Specification (classification)
      
      Main Arguments:
        hidden_units = tune()
      
      Engine-Specific Arguments:
        Hess = tune()
      
      Computational engine: nnet 
      

# bad input

    Code
      mlp(mode = "time series")
    Condition
      Error in `mlp()`:
      ! "time series" is not a known mode for model `mlp()`.

---

    Code
      translate(mlp(mode = "classification") %>% set_engine("wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `mlp()`
      i See `show_engines("mlp")`.

---

    Code
      translate(mlp(mode = "regression") %>% set_engine("nnet", formula = y ~ x))
    Condition
      Warning:
      The argument `formula` cannot be manually modified and was removed.
    Output
      Single Layer Neural Network Model Specification (regression)
      
      Main Arguments:
        hidden_units = 5
      
      Computational engine: nnet 
      
      Model fit template:
      nnet::nnet(formula = missing_arg(), data = missing_arg(), size = 5, 
          trace = FALSE, linout = TRUE)

# check_args() works

    Code
      spec <- mlp(penalty = -1) %>% set_engine("keras") %>% set_mode("classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      spec <- mlp(dropout = -1) %>% set_engine("keras") %>% set_mode("classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `dropout` must be a number between 0 and 1 or `NULL`, not the number -1.

---

    Code
      spec <- mlp(dropout = 1, penalty = 3) %>% set_engine("keras") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! Both weight decay and dropout should not be specified.


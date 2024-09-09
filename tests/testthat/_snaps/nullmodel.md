# bad input

    Code
      translate(null_model(mode = "regression") %>% set_engine())
    Condition
      Error in `set_engine()`:
      ! Missing engine. Possible mode/engine combinations are: classification {parsnip} and regression {parsnip}.

---

    Code
      translate(null_model() %>% set_engine("wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `null_model()`
      i See `show_engines("null_model")`.

---

    Code
      translate(null_model(formula = y ~ x))
    Condition
      Error in `null_model()`:
      ! unused argument (formula = y ~ x)

---

    Code
      translate(null_model(mode = "regression") %>% set_engine("parsnip", x = hpc[, 1:
        3], y = hpc$class))
    Condition
      Warning:
      The argument `x, y` cannot be manually modified and was removed.
    Output
      Null Model Specification (regression)
      
      Computational engine: parsnip 
      
      Model fit template:
      parsnip::nullmodel(x = missing_arg(), y = missing_arg())

# nullmodel execution

    Code
      res <- fit(null_model(mode = "regression") %>% set_engine("parsnip"),
      hpc_bad_form, data = hpc)
    Condition
      Error:
      ! object 'term' not found

# null_model printing

    Code
      print(null_model(mode = "classification"))
    Output
      Null Model Specification (classification)
      
      Computational engine: parsnip 
      

---

    Code
      print(null_model(mode = "classification") %>% set_engine("parsnip") %>%
        translate())
    Output
      Null Model Specification (classification)
      
      Computational engine: parsnip 
      
      Model fit template:
      parsnip::nullmodel(x = missing_arg(), y = missing_arg())


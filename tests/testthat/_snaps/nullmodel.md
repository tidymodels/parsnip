# bad input

    Code
      translate(null_model(mode = "regression") %>% set_engine())
    Condition
      Error in `set_engine.model_spec()`:
      ! argument "engine" is missing, with no default

---

    Code
      translate(null_model() %>% set_engine("wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `null_model()`
      i See `show_engines("null_model")`.

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


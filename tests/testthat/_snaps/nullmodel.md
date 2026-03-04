# bad input

    Code
      translate(set_engine(null_model(mode = "regression")))
    Condition
      Error in `set_engine()`:
      ! Missing engine. Possible mode/engine combinations are: classification {parsnip} and regression {parsnip}.

---

    Code
      translate(set_engine(null_model(), "wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `null_model()`
      i See `show_engines("null_model")`.

# nullmodel execution

    Code
      res <- fit(set_engine(null_model(mode = "regression"), "parsnip"), hpc_bad_form,
      data = hpc)
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
      print(translate(set_engine(null_model(mode = "classification"), "parsnip")))
    Output
      Null Model Specification (classification)
      
      Computational engine: parsnip 
      
      Model fit template:
      parsnip::nullmodel(x = missing_arg(), y = missing_arg())


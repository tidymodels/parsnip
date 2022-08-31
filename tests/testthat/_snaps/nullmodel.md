# null_model printing

    Code
      print(null_model(mode = "classification"))
    Output
      Null Model Specification (classification)
      

---

    Code
      print(null_model(mode = "classification") %>% set_engine("parsnip") %>%
        translate())
    Output
      Null Model Specification (classification)
      
      Computational engine: parsnip 
      
      Model fit template:
      parsnip::nullmodel(x = missing_arg(), y = missing_arg())


# model spec print methods work (whole game)

    Code
      svm_poly()
    Output
      Polynomial Support Vector Machine Model Specification (unknown mode)
      
      Computational engine: kernlab 
      

---

    Code
      boost_tree(mtry = 5)
    Output
      Boosted Tree Model Specification (unknown mode)
      
      Main Arguments:
        mtry = 5
      
      Computational engine: xgboost 
      

---

    Code
      set_mode(rand_forest(), "regression")
    Output
      Random Forest Model Specification (regression)
      
      Computational engine: ranger 
      

---

    Code
      set_engine(logistic_reg(), "glmnet", penalty = 0.5)
    Output
      Logistic Regression Model Specification (classification)
      
      Engine-Specific Arguments:
        penalty = 0.5
      
      Computational engine: glmnet 
      

---

    Code
      translate(set_mode(mlp(), "classification"))
    Output
      Single Layer Neural Network Model Specification (classification)
      
      Main Arguments:
        hidden_units = 5
      
      Computational engine: nnet 
      
      Model fit template:
      nnet::nnet(formula = missing_arg(), data = missing_arg(), size = 5, 
          trace = FALSE, linout = FALSE)

# `print_model_spec()` handles args correctly

    Code
      print_model_spec(linear_reg())
    Output
      Linear Regression Model Specification (regression)
      
      Computational engine: lm 
      

---

    Code
      print_model_spec(lr)
    Message
      ! parsnip could not locate an implementation for `beep` model specifications.
    Output
      beep Model Specification (regression)
      
      Computational engine: lm 
      

---

    Code
      print_model_spec(lr, cls = "boop")
    Message
      ! parsnip could not locate an implementation for `boop` model specifications.
    Output
      boop Model Specification (regression)
      
      Computational engine: lm 
      

---

    Code
      print_model_spec(lr, cls = "boop", desc = "Boop")
    Message
      ! parsnip could not locate an implementation for `boop` model specifications.
    Output
      Boop Model Specification (regression)
      
      Computational engine: lm 
      


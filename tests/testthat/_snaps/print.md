# model spec print methods work (whole game)

    Code
      svm_poly()
    Output
      Polynomial Support Vector Machine Model Specification (unknown)
      
      Computational engine: kernlab 
      

---

    Code
      boost_tree(mtry = 5)
    Output
      Boosted Tree Model Specification (unknown)
      
      Main Arguments:
        mtry = 5
      
      Computational engine: xgboost 
      

---

    Code
      rand_forest() %>% set_mode("regression")
    Output
      Random Forest Model Specification (regression)
      
      Computational engine: ranger 
      

---

    Code
      logistic_reg() %>% set_engine("glmnet", penalty = 0.5)
    Output
      Logistic Regression Model Specification (classification)
      
      Engine-Specific Arguments:
        penalty = 0.5
      
      Computational engine: glmnet 
      

---

    Code
      mlp() %>% set_mode("classification") %>% translate()
    Output
      Single Layer Neural Network Model Specification (classification)
      
      Main Arguments:
        hidden_units = 5
      
      Computational engine: nnet 
      
      Model fit template:
      nnet::nnet(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
          size = 5, trace = FALSE, linout = FALSE)

# `print_model_spec()` handles args correctly

    Code
      print_model_spec(linear_reg())
    Output
      Linear Regression Model Specification (regression)
      
      Computational engine: lm 
      

---

    Code
      print_model_spec(lr)
    Output
      beep Model Specification (regression)
      
      Computational engine: lm 
      

---

    Code
      print_model_spec(lr, cls = "boop")
    Output
      boop Model Specification (regression)
      
      Computational engine: lm 
      

---

    Code
      print_model_spec(lr, cls = "boop", desc = "Boop")
    Output
      Boop Model Specification (regression)
      
      Computational engine: lm 
      


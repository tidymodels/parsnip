# updating

    Code
      expr1 %>% update(mixture = 0)
    Output
      Multinomial Regression Model Specification (classification)
      
      Main Arguments:
        mixture = 0
      
      Engine-Specific Arguments:
        intercept = TRUE
      
      Computational engine: glmnet 
      

---

    Code
      expr2 %>% update(nlambda = 10)
    Output
      Multinomial Regression Model Specification (classification)
      
      Main Arguments:
        mixture = tune()
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      expr3 %>% update(mixture = 1, fresh = TRUE)
    Output
      Multinomial Regression Model Specification (classification)
      
      Main Arguments:
        mixture = 1
      
      Computational engine: glmnet 
      

---

    Code
      expr4 %>% update(param_tibb)
    Output
      Multinomial Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 1
        mixture = 0.333333333333333
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      expr4 %>% update(param_list)
    Output
      Multinomial Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 1
        mixture = 0.333333333333333
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      expr5 %>% update() %>% set_engine("glmnet", nlambda = 10, pmax = 2)
    Output
      Multinomial Regression Model Specification (classification)
      
      Main Arguments:
        mixture = 1
      
      Engine-Specific Arguments:
        nlambda = 10
        pmax = 2
      
      Computational engine: glmnet 
      


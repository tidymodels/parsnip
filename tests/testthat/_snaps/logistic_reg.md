# updating

    Code
      expr1 %>% update(mixture = 0)
    Output
      Logistic Regression Model Specification (classification)
      
      Main Arguments:
        mixture = 0
      
      Engine-Specific Arguments:
        family = expr(binomial(link = "probit"))
      
      Computational engine: glm 
      

---

    Code
      expr2 %>% update(nlambda = 10)
    Output
      Logistic Regression Model Specification (classification)
      
      Main Arguments:
        mixture = tune()
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      expr3 %>% update(mixture = 1, fresh = TRUE, nlambda = 10)
    Output
      Logistic Regression Model Specification (classification)
      
      Main Arguments:
        mixture = 1
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      expr4 %>% update(param_tibb)
    Output
      Logistic Regression Model Specification (classification)
      
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
      Logistic Regression Model Specification (classification)
      
      Main Arguments:
        penalty = 1
        mixture = 0.333333333333333
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      


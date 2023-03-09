# updating

    Code
      logistic_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10) %>% update(
        mixture = tune(), nlambda = tune())
    Output
      Logistic Regression Model Specification (classification)
      
      Main Arguments:
        mixture = tune()
      
      Engine-Specific Arguments:
        nlambda = tune()
      
      Computational engine: glmnet 
      

# bad input

    Code
      res <- mtcars %>% dplyr::mutate(cyl = as.factor(cyl)) %>% fit(logistic_reg(),
      cyl ~ mpg, data = .)
    Condition
      Warning:
      ! Logistic regression is intended for modeling binary outcomes, but there are 3 levels in the outcome.
      i If this is unintended, adjust outcome levels accordingly or see the `multinom_reg()` function.
      Warning:
      glm.fit: algorithm did not converge
      Warning:
      glm.fit: fitted probabilities numerically 0 or 1 occurred


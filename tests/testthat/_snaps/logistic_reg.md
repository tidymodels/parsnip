# updating

    Code
      update(set_engine(logistic_reg(mixture = 0), "glmnet", nlambda = 10), mixture = tune(),
      nlambda = tune())
    Output
      Logistic Regression Model Specification (classification)
      
      Main Arguments:
        mixture = tune()
      
      Engine-Specific Arguments:
        nlambda = tune()
      
      Computational engine: glmnet 
      

# bad input

    Code
      logistic_reg(mode = "regression")
    Condition
      Error in `logistic_reg()`:
      ! "regression" is not a known mode for model `logistic_reg()`.

---

    Code
      translate(set_engine(logistic_reg(mixture = 0.5), engine = "LiblineaR"))
    Condition
      Error in `translate()`:
      ! For the LiblineaR engine, `mixture` must be 0 or 1.

---

    Code
      res <- fit(dplyr::mutate(mtcars, cyl = as.factor(cyl)), logistic_reg(), cyl ~
        mpg, data = .)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'fit' applied to an object of class "data.frame"

# glm execution

    Code
      res <- fit(lc_basic, funded_amnt ~ term, data = lending_club, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a <factor>, not an integer vector.

---

    Code
      glm_form_catch <- fit(lc_basic, funded_amnt ~ term, data = lending_club,
      control = caught_ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a <factor>, not an integer vector.

---

    Code
      glm_xy_catch <- fit_xy(lc_basic, control = caught_ctrl, x = lending_club[,
        num_pred], y = lending_club$total_bal_il)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a <factor>, not an integer vector.

# liblinear execution

    Code
      res <- fit(ll_basic, funded_amnt ~ term, data = lending_club, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a <factor>, not an integer vector.

---

    Code
      glm_form_catch <- fit(ll_basic, funded_amnt ~ term, data = lending_club,
      control = caught_ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a <factor>, not an integer vector.

---

    Code
      glm_xy_catch <- fit_xy(ll_basic, control = caught_ctrl, x = lending_club[,
        num_pred], y = lending_club$total_bal_il)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a <factor>, not an integer vector.

# check_args() works

    Code
      spec <- set_mode(set_engine(logistic_reg(mixture = -1), "glm"),
      "classification")
      fit(spec, Class ~ ., lending_club)
    Condition
      Error in `fit()`:
      ! `mixture` must be a number between 0 and 1 or `NULL`, not the number -1.

---

    Code
      spec <- set_mode(set_engine(logistic_reg(penalty = -1), "glm"),
      "classification")
      fit(spec, Class ~ ., lending_club)
    Condition
      Error in `fit()`:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      spec <- set_mode(set_engine(logistic_reg(mixture = 0.5), "LiblineaR"),
      "classification")
      fit(spec, Class ~ ., lending_club)
    Condition
      Error in `fit()`:
      x For the LiblineaR engine, mixture must be 0 or 1, not 0.5.
      i Choose a pure ridge model with `mixture = 0` or a pure lasso model with `mixture = 1`.
      ! The Liblinear engine does not support other values.

---

    Code
      spec <- set_mode(set_engine(logistic_reg(penalty = 0), "LiblineaR"),
      "classification")
      fit(spec, Class ~ ., lending_club)
    Condition
      Error in `fit()`:
      ! For the LiblineaR engine, `penalty` must be `> 0`, not 0.

# tunables

    Code
      tunable(logistic_reg())
    Output
      # A tibble: 0 x 5
      # i 5 variables: name <chr>, call_info <list>, source <chr>, component <chr>,
      #   component_id <chr>

---

    Code
      tunable(set_engine(logistic_reg(), "brulee"))
    Output
      # A tibble: 9 x 5
        name          call_info        source     component    component_id
        <chr>         <list>           <chr>      <chr>        <chr>       
      1 epochs        <named list [3]> model_spec logistic_reg engine      
      2 penalty       <named list [2]> model_spec logistic_reg main        
      3 mixture       <named list [2]> model_spec logistic_reg main        
      4 learn_rate    <named list [3]> model_spec logistic_reg engine      
      5 momentum      <named list [3]> model_spec logistic_reg engine      
      6 batch_size    <named list [3]> model_spec logistic_reg engine      
      7 class_weights <named list [2]> model_spec logistic_reg engine      
      8 stop_iter     <named list [2]> model_spec logistic_reg engine      
      9 rate_schedule <named list [3]> model_spec logistic_reg engine      

---

    Code
      tunable(set_engine(logistic_reg(), "glmnet"))
    Output
      # A tibble: 2 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec logistic_reg main        
      2 mixture <named list [3]> model_spec logistic_reg main        

---

    Code
      tunable(set_engine(logistic_reg(), "keras"))
    Output
      # A tibble: 1 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec logistic_reg main        


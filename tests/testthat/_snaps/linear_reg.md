# updating

    Code
      linear_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10) %>% update(
        mixture = tune(), nlambda = tune())
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        mixture = tune()
      
      Engine-Specific Arguments:
        nlambda = tune()
      
      Computational engine: glmnet 
      

# bad input

    Code
      linear_reg(mode = "classification")
    Condition
      Error in `linear_reg()`:
      ! "classification" is not a known mode for model `linear_reg()`.

---

    Code
      translate(linear_reg(), engine = "wat?")
    Condition
      Error in `translate.default()`:
      x Engine "wat?" is not supported for `linear_reg()`
      i See `show_engines("linear_reg")`.

---

    Code
      translate(linear_reg(), engine = NULL)
    Condition
      Error in `translate.default()`:
      ! Please set an engine.

# lm execution

    Code
      res <- fit_xy(hpc_basic, x = hpc[, num_pred], y = hpc$class, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be <numeric>, not a <factor> object.

---

    Code
      res <- fit_xy(hpc_basic, x = hpc[, num_pred], y = as.character(hpc$class),
      control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be <numeric>, not a character vector.

---

    Code
      res <- fit(hpc_basic, hpc_bad_form, data = hpc, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be <numeric>, not a <factor> object.

---

    Code
      lm_form_catch <- fit(hpc_basic, hpc_bad_form, data = hpc, control = caught_ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be <numeric>, not a <factor> object.

# glm execution

    Code
      res <- fit_xy(hpc_glm, x = hpc[, num_pred], y = hpc$class, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be <numeric>, not a <factor> object.

---

    Code
      res <- fit(hpc_glm, hpc_bad_form, data = hpc, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be <numeric>, not a <factor> object.

---

    Code
      lm_form_catch <- fit(hpc_glm, hpc_bad_form, data = hpc, control = caught_ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a regression model, the outcome should be <numeric>, not a <factor> object.

# newdata error trapping

    Code
      predict(res_xy, newdata = hpc[1:3, num_pred])
    Condition
      Error in `predict()`:
      ! Please use `new_data` instead of `newdata`.

# show engine

    Code
      show_engines("linear_re")
    Condition
      Error in `show_engines()`:
      ! No results found for model function "x".

# lm can handle rankdeficient predictions

    Code
      preds <- linear_reg() %>% fit(y ~ ., data = data) %>% predict(new_data = data2)
    Condition
      Warning in `predict.lm()`:
      prediction from rank-deficient fit; consider predict(., rankdeficient="NA")

# check_args() works

    Code
      spec <- linear_reg(mixture = -1) %>% set_engine("lm") %>% set_mode("regression")
      fit(spec, compounds ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `mixture` must be a number between 0 and 1 or `NULL`, not the number -1.

---

    Code
      spec <- linear_reg(penalty = -1) %>% set_engine("lm") %>% set_mode("regression")
      fit(spec, compounds ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

# prevent using a Poisson family

    Code
      linear_reg(penalty = 1) %>% set_engine("glmnet", family = poisson) %>% fit(mpg ~
        ., data = mtcars)
    Condition
      Error in `fit()`:
      ! Please install the glmnet package to use this engine.

---

    Code
      linear_reg(penalty = 1) %>% set_engine("glmnet", family = stats::poisson) %>%
        fit(mpg ~ ., data = mtcars)
    Condition
      Error in `fit()`:
      ! Please install the glmnet package to use this engine.

---

    Code
      linear_reg(penalty = 1) %>% set_engine("glmnet", family = stats::poisson()) %>%
        fit(mpg ~ ., data = mtcars)
    Condition
      Error in `fit()`:
      ! Please install the glmnet package to use this engine.

---

    Code
      linear_reg(penalty = 1) %>% set_engine("glmnet", family = "poisson") %>% fit(
        mpg ~ ., data = mtcars)
    Condition
      Error in `fit()`:
      ! Please install the glmnet package to use this engine.

# tunables

    Code
      linear_reg() %>% tunable()
    Output
      # A tibble: 0 x 5
      # i 5 variables: name <chr>, call_info <list>, source <chr>, component <chr>,
      #   component_id <chr>

---

    Code
      linear_reg() %>% set_engine("brulee") %>% tunable()
    Output
      # A tibble: 8 x 5
        name          call_info        source     component  component_id
        <chr>         <list>           <chr>      <chr>      <chr>       
      1 epochs        <named list [3]> model_spec linear_reg engine      
      2 penalty       <named list [2]> model_spec linear_reg main        
      3 mixture       <named list [2]> model_spec linear_reg main        
      4 learn_rate    <named list [3]> model_spec linear_reg engine      
      5 momentum      <named list [3]> model_spec linear_reg engine      
      6 batch_size    <named list [2]> model_spec linear_reg engine      
      7 stop_iter     <named list [2]> model_spec linear_reg engine      
      8 rate_schedule <named list [3]> model_spec linear_reg engine      

---

    Code
      linear_reg() %>% set_engine("glmnet") %>% tunable()
    Output
      # A tibble: 2 x 5
        name    call_info        source     component  component_id
        <chr>   <list>           <chr>      <chr>      <chr>       
      1 penalty <named list [2]> model_spec linear_reg main        
      2 mixture <named list [3]> model_spec linear_reg main        

---

    Code
      linear_reg() %>% set_engine("quantreg") %>% tunable()
    Output
      # A tibble: 0 x 5
      # i 5 variables: name <chr>, call_info <list>, source <chr>, component <chr>,
      #   component_id <chr>

---

    Code
      linear_reg() %>% set_engine("keras") %>% tunable()
    Output
      # A tibble: 1 x 5
        name    call_info        source     component  component_id
        <chr>   <list>           <chr>      <chr>      <chr>       
      1 penalty <named list [2]> model_spec linear_reg main        


# updating

    Code
      multinom_reg(mixture = 0) %>% set_engine("glmnet", nlambda = 10) %>% update(
        mixture = tune(), nlambda = tune())
    Output
      Multinomial Regression Model Specification (classification)
      
      Main Arguments:
        mixture = tune()
      
      Engine-Specific Arguments:
        nlambda = tune()
      
      Computational engine: glmnet 
      

# bad input

    Code
      multinom_reg(mode = "regression")
    Condition
      Error in `multinom_reg()`:
      ! "regression" is not a known mode for model `multinom_reg()`.

---

    Code
      translate(multinom_reg(penalty = 0.1) %>% set_engine("wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `multinom_reg()`
      i See `show_engines("multinom_reg")`.

---

    Code
      multinom_reg(penalty = 0.1) %>% set_engine()
    Condition
      Error in `set_engine()`:
      ! Missing engine. Possible mode/engine combinations are: classification {glmnet, spark, keras, nnet, brulee}.

# check_args() works

    Code
      spec <- multinom_reg(mixture = -1) %>% set_engine("keras") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `mixture` must be a number between 0 and 1 or `NULL`, not the number -1.

---

    Code
      spec <- multinom_reg(penalty = -1) %>% set_engine("keras") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

# tunables

    Code
      multinom_reg() %>% tunable()
    Output
      # A tibble: 1 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec multinom_reg main        

---

    Code
      multinom_reg() %>% set_engine("brulee") %>% tunable()
    Output
      # A tibble: 9 x 5
        name          call_info        source     component    component_id
        <chr>         <list>           <chr>      <chr>        <chr>       
      1 epochs        <named list [3]> model_spec multinom_reg engine      
      2 penalty       <named list [2]> model_spec multinom_reg main        
      3 mixture       <named list [2]> model_spec multinom_reg main        
      4 learn_rate    <named list [3]> model_spec multinom_reg engine      
      5 momentum      <named list [3]> model_spec multinom_reg engine      
      6 batch_size    <named list [2]> model_spec multinom_reg engine      
      7 class_weights <named list [2]> model_spec multinom_reg engine      
      8 stop_iter     <named list [2]> model_spec multinom_reg engine      
      9 rate_schedule <named list [3]> model_spec multinom_reg engine      

---

    Code
      multinom_reg() %>% set_engine("nnet") %>% tunable()
    Output
      # A tibble: 1 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec multinom_reg main        

---

    Code
      multinom_reg() %>% set_engine("glmnet") %>% tunable()
    Output
      # A tibble: 2 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec multinom_reg main        
      2 mixture <named list [3]> model_spec multinom_reg main        

---

    Code
      multinom_reg() %>% set_engine("keras") %>% tunable()
    Output
      # A tibble: 1 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec multinom_reg main        


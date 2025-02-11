# updating

    Code
      mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE) %>%
        update(hidden_units = tune(), Hess = tune())
    Output
      Single Layer Neural Network Model Specification (classification)
      
      Main Arguments:
        hidden_units = tune()
      
      Engine-Specific Arguments:
        Hess = tune()
      
      Computational engine: nnet 
      

# bad input

    Code
      mlp(mode = "time series")
    Condition
      Error in `mlp()`:
      ! "time series" is not a known mode for model `mlp()`.

---

    Code
      translate(mlp(mode = "classification") %>% set_engine("wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `mlp()`
      i See `show_engines("mlp")`.

# check_args() works

    Code
      spec <- mlp(penalty = -1) %>% set_engine("keras") %>% set_mode("classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      spec <- mlp(dropout = -1) %>% set_engine("keras") %>% set_mode("classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `dropout` must be a number between 0 and 1 or `NULL`, not the number -1.

---

    Code
      spec <- mlp(dropout = 1, penalty = 3) %>% set_engine("keras") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! Both weight decay and dropout should not be specified.

# tunables

    Code
      mlp() %>% set_engine("brulee") %>% tunable()
    Output
      # A tibble: 12 x 5
         name          call_info        source     component component_id
         <chr>         <list>           <chr>      <chr>     <chr>       
       1 epochs        <named list [3]> model_spec mlp       main        
       2 hidden_units  <named list [3]> model_spec mlp       main        
       3 activation    <named list [3]> model_spec mlp       main        
       4 penalty       <named list [2]> model_spec mlp       main        
       5 mixture       <named list [2]> model_spec mlp       engine      
       6 dropout       <named list [2]> model_spec mlp       main        
       7 learn_rate    <named list [3]> model_spec mlp       main        
       8 momentum      <named list [3]> model_spec mlp       engine      
       9 batch_size    <named list [2]> model_spec mlp       engine      
      10 class_weights <named list [2]> model_spec mlp       engine      
      11 stop_iter     <named list [2]> model_spec mlp       engine      
      12 rate_schedule <named list [3]> model_spec mlp       engine      

---

    Code
      mlp() %>% set_engine("brulee_two_layer") %>% tunable()
    Output
      # A tibble: 14 x 5
         name           call_info        source     component component_id
         <chr>          <list>           <chr>      <chr>     <chr>       
       1 epochs         <named list [3]> model_spec mlp       main        
       2 hidden_units   <named list [3]> model_spec mlp       main        
       3 hidden_units_2 <named list [3]> model_spec mlp       engine      
       4 activation     <named list [3]> model_spec mlp       main        
       5 activation_2   <named list [3]> model_spec mlp       engine      
       6 penalty        <named list [2]> model_spec mlp       main        
       7 mixture        <named list [2]> model_spec mlp       engine      
       8 dropout        <named list [2]> model_spec mlp       main        
       9 learn_rate     <named list [3]> model_spec mlp       main        
      10 momentum       <named list [3]> model_spec mlp       engine      
      11 batch_size     <named list [2]> model_spec mlp       engine      
      12 class_weights  <named list [2]> model_spec mlp       engine      
      13 stop_iter      <named list [2]> model_spec mlp       engine      
      14 rate_schedule  <named list [3]> model_spec mlp       engine      

---

    Code
      mlp() %>% set_engine("nnet") %>% tunable()
    Output
      # A tibble: 3 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 hidden_units <named list [2]> model_spec mlp       main        
      2 penalty      <named list [2]> model_spec mlp       main        
      3 epochs       <named list [2]> model_spec mlp       main        

---

    Code
      mlp() %>% set_engine("keras") %>% tunable()
    Output
      # A tibble: 5 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 hidden_units <named list [2]> model_spec mlp       main        
      2 penalty      <named list [2]> model_spec mlp       main        
      3 dropout      <named list [2]> model_spec mlp       main        
      4 epochs       <named list [2]> model_spec mlp       main        
      5 activation   <named list [2]> model_spec mlp       main        


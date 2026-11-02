# updating

    Code
      update(set_engine(mlp(mode = "classification", hidden_units = 2), "nnet", Hess = FALSE),
      hidden_units = tune(), Hess = tune())
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
      translate(set_engine(mlp(mode = "classification"), "wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `mlp()`.
      i See `show_engines("mlp")`.

# check_args() works

    Code
      spec <- set_mode(set_engine(mlp(penalty = -1), "keras"), "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      spec <- set_mode(set_engine(mlp(dropout = -1), "keras"), "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `dropout` must be a number between 0 and 1 or `NULL`, not the number -1.

# tunables

    Code
      tunable(set_engine(mlp(), "brulee"))
    Output
      # A tibble: 19 x 5
         name          call_info        source     component component_id
         <chr>         <list>           <chr>      <chr>     <chr>       
       1 hidden_units  <named list [3]> model_spec mlp       main        
       2 penalty       <named list [2]> model_spec mlp       main        
       3 epochs        <named list [3]> model_spec mlp       main        
       4 dropout       <named list [2]> model_spec mlp       main        
       5 learn_rate    <named list [3]> model_spec mlp       main        
       6 activation    <named list [3]> model_spec mlp       main        
       7 mixture       <named list [2]> model_spec mlp       engine      
       8 momentum      <named list [3]> model_spec mlp       engine      
       9 batch_size    <named list [3]> model_spec mlp       engine      
      10 class_weights <named list [2]> model_spec mlp       engine      
      11 stop_iter     <named list [2]> model_spec mlp       engine      
      12 validation    <named list [2]> model_spec mlp       engine      
      13 rate_schedule <named list [3]> model_spec mlp       engine      
      14 initial       <named list [2]> model_spec mlp       engine      
      15 largest       <named list [2]> model_spec mlp       engine      
      16 reduction     <named list [2]> model_spec mlp       engine      
      17 steps         <named list [2]> model_spec mlp       engine      
      18 step_size     <named list [2]> model_spec mlp       engine      
      19 decay         <named list [2]> model_spec mlp       engine      

---

    Code
      tunable(set_engine(mlp(), "brulee_two_layer"))
    Output
      # A tibble: 21 x 5
         name           call_info        source     component component_id
         <chr>          <list>           <chr>      <chr>     <chr>       
       1 hidden_units   <named list [3]> model_spec mlp       main        
       2 penalty        <named list [2]> model_spec mlp       main        
       3 epochs         <named list [3]> model_spec mlp       main        
       4 dropout        <named list [2]> model_spec mlp       main        
       5 learn_rate     <named list [3]> model_spec mlp       main        
       6 activation     <named list [3]> model_spec mlp       main        
       7 hidden_units_2 <named list [3]> model_spec mlp       engine      
       8 activation_2   <named list [3]> model_spec mlp       engine      
       9 mixture        <named list [2]> model_spec mlp       engine      
      10 momentum       <named list [3]> model_spec mlp       engine      
      # i 11 more rows

---

    Code
      tunable(set_engine(mlp(), "nnet"))
    Output
      # A tibble: 3 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 hidden_units <named list [2]> model_spec mlp       main        
      2 penalty      <named list [2]> model_spec mlp       main        
      3 epochs       <named list [2]> model_spec mlp       main        

---

    Code
      tunable(set_engine(mlp(), "keras"))
    Output
      # A tibble: 5 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 hidden_units <named list [2]> model_spec mlp       main        
      2 penalty      <named list [2]> model_spec mlp       main        
      3 dropout      <named list [2]> model_spec mlp       main        
      4 epochs       <named list [2]> model_spec mlp       main        
      5 activation   <named list [2]> model_spec mlp       main        


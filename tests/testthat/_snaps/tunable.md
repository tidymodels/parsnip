# tunable.linear_reg()

    Code
      tunable(spec)
    Output
      # A tibble: 0 x 5
      # i 5 variables: name <chr>, call_info <list>, source <chr>, component <chr>,
      #   component_id <chr>

---

    Code
      tunable(spec %>% set_engine("lm"))
    Output
      # A tibble: 0 x 5
      # i 5 variables: name <chr>, call_info <list>, source <chr>, component <chr>,
      #   component_id <chr>

---

    Code
      tunable(spec %>% set_engine("glmnet"))
    Output
      # A tibble: 2 x 5
        name    call_info        source     component  component_id
        <chr>   <list>           <chr>      <chr>      <chr>       
      1 penalty <named list [2]> model_spec linear_reg main        
      2 mixture <named list [3]> model_spec linear_reg main        

---

    Code
      tunable(spec %>% set_engine("brulee"))
    Output
      # A tibble: 2 x 5
        name    call_info        source     component  component_id
        <chr>   <list>           <chr>      <chr>      <chr>       
      1 penalty <named list [2]> model_spec linear_reg main        
      2 mixture <named list [2]> model_spec linear_reg main        

---

    Code
      tunable(spec %>% set_engine("glmnet", dfmax = tune()))
    Output
      # A tibble: 2 x 5
        name    call_info        source     component  component_id
        <chr>   <list>           <chr>      <chr>      <chr>       
      1 penalty <named list [2]> model_spec linear_reg main        
      2 mixture <named list [3]> model_spec linear_reg main        

# tunable.logistic_reg()

    Code
      tunable(spec)
    Output
      # A tibble: 0 x 5
      # i 5 variables: name <chr>, call_info <list>, source <chr>, component <chr>,
      #   component_id <chr>

---

    Code
      tunable(spec %>% set_engine("glm"))
    Output
      # A tibble: 0 x 5
      # i 5 variables: name <chr>, call_info <list>, source <chr>, component <chr>,
      #   component_id <chr>

---

    Code
      tunable(spec %>% set_engine("glmnet"))
    Output
      # A tibble: 2 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec logistic_reg main        
      2 mixture <named list [3]> model_spec logistic_reg main        

---

    Code
      tunable(spec %>% set_engine("brulee"))
    Output
      # A tibble: 2 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec logistic_reg main        
      2 mixture <named list [2]> model_spec logistic_reg main        

---

    Code
      tunable(spec %>% set_engine("glmnet", dfmax = tune()))
    Output
      # A tibble: 2 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec logistic_reg main        
      2 mixture <named list [3]> model_spec logistic_reg main        

# tunable.multinom_reg()

    Code
      tunable(spec)
    Output
      # A tibble: 1 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec multinom_reg main        

---

    Code
      tunable(spec %>% set_engine("glmnet"))
    Output
      # A tibble: 2 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec multinom_reg main        
      2 mixture <named list [2]> model_spec multinom_reg main        

---

    Code
      tunable(spec %>% set_engine("spark"))
    Output
      # A tibble: 2 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec multinom_reg main        
      2 mixture <named list [2]> model_spec multinom_reg main        

---

    Code
      tunable(spec %>% set_engine("keras"))
    Output
      # A tibble: 1 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec multinom_reg main        

---

    Code
      tunable(spec %>% set_engine("nnet"))
    Output
      # A tibble: 1 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec multinom_reg main        

---

    Code
      tunable(spec %>% set_engine("brulee"))
    Output
      # A tibble: 2 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec multinom_reg main        
      2 mixture <named list [2]> model_spec multinom_reg main        

---

    Code
      tunable(spec %>% set_engine("glmnet", dfmax = tune()))
    Output
      # A tibble: 3 x 5
        name    call_info        source     component    component_id
        <chr>   <list>           <chr>      <chr>        <chr>       
      1 penalty <named list [2]> model_spec multinom_reg main        
      2 mixture <named list [2]> model_spec multinom_reg main        
      3 dfmax   <NULL>           model_spec multinom_reg engine      

# tunable.boost_tree()

    Code
      tunable(spec)
    Output
      # A tibble: 8 x 5
        name           call_info        source     component  component_id
        <chr>          <list>           <chr>      <chr>      <chr>       
      1 tree_depth     <named list [2]> model_spec boost_tree main        
      2 trees          <named list [2]> model_spec boost_tree main        
      3 learn_rate     <named list [3]> model_spec boost_tree main        
      4 mtry           <named list [2]> model_spec boost_tree main        
      5 min_n          <named list [2]> model_spec boost_tree main        
      6 loss_reduction <named list [2]> model_spec boost_tree main        
      7 sample_size    <named list [2]> model_spec boost_tree main        
      8 stop_iter      <named list [2]> model_spec boost_tree main        

---

    Code
      tunable(spec %>% set_engine("xgboost"))
    Output
      # A tibble: 8 x 5
        name           call_info        source     component  component_id
        <chr>          <list>           <chr>      <chr>      <chr>       
      1 tree_depth     <named list [2]> model_spec boost_tree main        
      2 trees          <named list [2]> model_spec boost_tree main        
      3 learn_rate     <named list [3]> model_spec boost_tree main        
      4 mtry           <named list [2]> model_spec boost_tree main        
      5 min_n          <named list [2]> model_spec boost_tree main        
      6 loss_reduction <named list [2]> model_spec boost_tree main        
      7 sample_size    <named list [2]> model_spec boost_tree main        
      8 stop_iter      <named list [2]> model_spec boost_tree main        

---

    Code
      tunable(spec %>% set_engine("C5.0"))
    Output
      # A tibble: 3 x 5
        name        call_info        source     component  component_id
        <chr>       <list>           <chr>      <chr>      <chr>       
      1 trees       <named list [3]> model_spec boost_tree main        
      2 min_n       <named list [2]> model_spec boost_tree main        
      3 sample_size <named list [2]> model_spec boost_tree main        

---

    Code
      tunable(spec %>% set_engine("spark"))
    Output
      # A tibble: 7 x 5
        name           call_info        source     component  component_id
        <chr>          <list>           <chr>      <chr>      <chr>       
      1 tree_depth     <named list [2]> model_spec boost_tree main        
      2 trees          <named list [2]> model_spec boost_tree main        
      3 learn_rate     <named list [2]> model_spec boost_tree main        
      4 mtry           <named list [2]> model_spec boost_tree main        
      5 min_n          <named list [2]> model_spec boost_tree main        
      6 loss_reduction <named list [2]> model_spec boost_tree main        
      7 sample_size    <named list [2]> model_spec boost_tree main        

---

    Code
      tunable(spec %>% set_engine("xgboost", feval = tune()))
    Output
      # A tibble: 8 x 5
        name           call_info        source     component  component_id
        <chr>          <list>           <chr>      <chr>      <chr>       
      1 tree_depth     <named list [2]> model_spec boost_tree main        
      2 trees          <named list [2]> model_spec boost_tree main        
      3 learn_rate     <named list [3]> model_spec boost_tree main        
      4 mtry           <named list [2]> model_spec boost_tree main        
      5 min_n          <named list [2]> model_spec boost_tree main        
      6 loss_reduction <named list [2]> model_spec boost_tree main        
      7 sample_size    <named list [2]> model_spec boost_tree main        
      8 stop_iter      <named list [2]> model_spec boost_tree main        

# tunable.rand_forest()

    Code
      tunable(spec)
    Output
      # A tibble: 3 x 5
        name  call_info        source     component   component_id
        <chr> <list>           <chr>      <chr>       <chr>       
      1 mtry  <named list [2]> model_spec rand_forest main        
      2 trees <named list [2]> model_spec rand_forest main        
      3 min_n <named list [2]> model_spec rand_forest main        

---

    Code
      tunable(spec %>% set_engine("ranger"))
    Output
      # A tibble: 3 x 5
        name  call_info        source     component   component_id
        <chr> <list>           <chr>      <chr>       <chr>       
      1 mtry  <named list [2]> model_spec rand_forest main        
      2 trees <named list [2]> model_spec rand_forest main        
      3 min_n <named list [2]> model_spec rand_forest main        

---

    Code
      tunable(spec %>% set_engine("randomForest"))
    Output
      # A tibble: 3 x 5
        name  call_info        source     component   component_id
        <chr> <list>           <chr>      <chr>       <chr>       
      1 mtry  <named list [2]> model_spec rand_forest main        
      2 trees <named list [2]> model_spec rand_forest main        
      3 min_n <named list [2]> model_spec rand_forest main        

---

    Code
      tunable(spec %>% set_engine("spark"))
    Output
      # A tibble: 3 x 5
        name  call_info        source     component   component_id
        <chr> <list>           <chr>      <chr>       <chr>       
      1 mtry  <named list [2]> model_spec rand_forest main        
      2 trees <named list [2]> model_spec rand_forest main        
      3 min_n <named list [2]> model_spec rand_forest main        

---

    Code
      tunable(spec %>% set_engine("ranger", min.bucket = tune()))
    Output
      # A tibble: 3 x 5
        name  call_info        source     component   component_id
        <chr> <list>           <chr>      <chr>       <chr>       
      1 mtry  <named list [2]> model_spec rand_forest main        
      2 trees <named list [2]> model_spec rand_forest main        
      3 min_n <named list [2]> model_spec rand_forest main        

# tunable.mars()

    Code
      tunable(spec)
    Output
      # A tibble: 3 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 num_terms    <named list [3]> model_spec mars      main        
      2 prod_degree  <named list [2]> model_spec mars      main        
      3 prune_method <named list [2]> model_spec mars      main        

---

    Code
      tunable(spec %>% set_engine("earth"))
    Output
      # A tibble: 3 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 num_terms    <named list [3]> model_spec mars      main        
      2 prod_degree  <named list [2]> model_spec mars      main        
      3 prune_method <named list [2]> model_spec mars      main        

---

    Code
      tunable(spec %>% set_engine("earth", minspan = tune()))
    Output
      # A tibble: 3 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 num_terms    <named list [3]> model_spec mars      main        
      2 prod_degree  <named list [2]> model_spec mars      main        
      3 prune_method <named list [2]> model_spec mars      main        

# tunable.decision_tree()

    Code
      tunable(spec)
    Output
      # A tibble: 3 x 5
        name            call_info        source     component     component_id
        <chr>           <list>           <chr>      <chr>         <chr>       
      1 tree_depth      <named list [2]> model_spec decision_tree main        
      2 min_n           <named list [2]> model_spec decision_tree main        
      3 cost_complexity <named list [2]> model_spec decision_tree main        

---

    Code
      tunable(spec %>% set_engine("rpart"))
    Output
      # A tibble: 3 x 5
        name            call_info        source     component     component_id
        <chr>           <list>           <chr>      <chr>         <chr>       
      1 tree_depth      <named list [2]> model_spec decision_tree main        
      2 min_n           <named list [2]> model_spec decision_tree main        
      3 cost_complexity <named list [2]> model_spec decision_tree main        

---

    Code
      tunable(spec %>% set_engine("C5.0"))
    Output
      # A tibble: 1 x 5
        name  call_info        source     component     component_id
        <chr> <list>           <chr>      <chr>         <chr>       
      1 min_n <named list [2]> model_spec decision_tree main        

---

    Code
      tunable(spec %>% set_engine("spark"))
    Output
      # A tibble: 2 x 5
        name       call_info        source     component     component_id
        <chr>      <list>           <chr>      <chr>         <chr>       
      1 tree_depth <named list [2]> model_spec decision_tree main        
      2 min_n      <named list [2]> model_spec decision_tree main        

---

    Code
      tunable(spec %>% set_engine("rpart", parms = tune()))
    Output
      # A tibble: 3 x 5
        name            call_info        source     component     component_id
        <chr>           <list>           <chr>      <chr>         <chr>       
      1 tree_depth      <named list [2]> model_spec decision_tree main        
      2 min_n           <named list [2]> model_spec decision_tree main        
      3 cost_complexity <named list [2]> model_spec decision_tree main        

# tunable.svm_poly()

    Code
      tunable(spec)
    Output
      # A tibble: 4 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 cost         <named list [3]> model_spec svm_poly  main        
      2 degree       <named list [3]> model_spec svm_poly  main        
      3 scale_factor <named list [2]> model_spec svm_poly  main        
      4 margin       <named list [2]> model_spec svm_poly  main        

---

    Code
      tunable(spec %>% set_engine("kernlab"))
    Output
      # A tibble: 4 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 cost         <named list [3]> model_spec svm_poly  main        
      2 degree       <named list [3]> model_spec svm_poly  main        
      3 scale_factor <named list [2]> model_spec svm_poly  main        
      4 margin       <named list [2]> model_spec svm_poly  main        

---

    Code
      tunable(spec %>% set_engine("kernlab", tol = tune()))
    Output
      # A tibble: 4 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 cost         <named list [3]> model_spec svm_poly  main        
      2 degree       <named list [3]> model_spec svm_poly  main        
      3 scale_factor <named list [2]> model_spec svm_poly  main        
      4 margin       <named list [2]> model_spec svm_poly  main        

# tunable.mlp()

    Code
      tunable(spec)
    Output
      # A tibble: 3 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 hidden_units <named list [2]> model_spec mlp       main        
      2 penalty      <named list [2]> model_spec mlp       main        
      3 epochs       <named list [2]> model_spec mlp       main        

---

    Code
      tunable(spec %>% set_engine("keras"))
    Output
      # A tibble: 5 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 hidden_units <named list [2]> model_spec mlp       main        
      2 penalty      <named list [2]> model_spec mlp       main        
      3 dropout      <named list [2]> model_spec mlp       main        
      4 epochs       <named list [2]> model_spec mlp       main        
      5 activation   <named list [2]> model_spec mlp       main        

---

    Code
      tunable(spec %>% set_engine("nnet"))
    Output
      # A tibble: 3 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 hidden_units <named list [2]> model_spec mlp       main        
      2 penalty      <named list [2]> model_spec mlp       main        
      3 epochs       <named list [2]> model_spec mlp       main        

---

    Code
      tunable(spec %>% set_engine("brulee"))
    Output
      # A tibble: 6 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 hidden_units <named list [2]> model_spec mlp       main        
      2 penalty      <named list [2]> model_spec mlp       main        
      3 epochs       <named list [3]> model_spec mlp       main        
      4 dropout      <named list [2]> model_spec mlp       main        
      5 learn_rate   <named list [3]> model_spec mlp       main        
      6 activation   <named list [3]> model_spec mlp       main        

---

    Code
      tunable(spec %>% set_engine("keras", ragged = tune()))
    Output
      # A tibble: 5 x 5
        name         call_info        source     component component_id
        <chr>        <list>           <chr>      <chr>     <chr>       
      1 hidden_units <named list [2]> model_spec mlp       main        
      2 penalty      <named list [2]> model_spec mlp       main        
      3 dropout      <named list [2]> model_spec mlp       main        
      4 epochs       <named list [2]> model_spec mlp       main        
      5 activation   <named list [2]> model_spec mlp       main        

# tunable.survival_reg()

    Code
      tunable(spec)
    Output
      # A tibble: 0 x 5
      # i 5 variables: name <chr>, call_info <list>, source <chr>, component <chr>,
      #   component_id <chr>

---

    Code
      tunable(spec %>% set_engine("survival"))
    Output
      # A tibble: 0 x 5
      # i 5 variables: name <chr>, call_info <list>, source <chr>, component <chr>,
      #   component_id <chr>

---

    Code
      tunable(spec %>% set_engine("survival", parms = tune()))
    Output
      # A tibble: 0 x 5
      # i 5 variables: name <chr>, call_info <list>, source <chr>, component <chr>,
      #   component_id <chr>


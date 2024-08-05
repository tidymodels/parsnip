# existing modes

    Code
      get_from_env("modes")
    Output
      [1] "classification"      "regression"          "censored regression"
      [4] "quantile regression" "unknown"            

# adding a new engine

    Code
      set_model_engine("sponge", mode = "regression", eng = "gum")
    Condition
      Error in `check_mode_for_new_engine()`:
      ! "regression" is not a known mode for model `sponge()`.

# showing model info

    Code
      show_model_info("rand_forest")
    Output
      Information for `rand_forest`
       modes: unknown, classification, regression, censored regression 
      
       engines: 
         classification: randomForest, ranger1, spark
         regression:     randomForest, ranger1, spark
      
      1The model can use case weights.
      
       arguments: 
         ranger:       
            mtry  --> mtry
            trees --> num.trees
            min_n --> min.node.size
         randomForest: 
            mtry  --> mtry
            trees --> ntree
            min_n --> nodesize
         spark:        
            mtry  --> feature_subset_strategy
            trees --> num_trees
            min_n --> min_instances_per_node
      
       fit modules:
               engine           mode
               ranger classification
               ranger     regression
         randomForest classification
         randomForest     regression
                spark classification
                spark     regression
      
       prediction modules:
                   mode       engine                    methods
         classification randomForest           class, prob, raw
         classification       ranger class, conf_int, prob, raw
         classification        spark                class, prob
             regression randomForest               numeric, raw
             regression       ranger     conf_int, numeric, raw
             regression        spark                    numeric
      

---

    Code
      show_model_info("mlp")
    Output
      Information for `mlp`
       modes: unknown, classification, regression 
      
       engines: 
         classification: brulee, keras, nnet
         regression:     brulee, keras, nnet
      
      
       arguments: 
         keras:  
            hidden_units --> hidden_units
            penalty      --> penalty
            dropout      --> dropout
            epochs       --> epochs
            activation   --> activation
         nnet:   
            hidden_units --> size
            penalty      --> decay
            epochs       --> maxit
         brulee: 
            hidden_units --> hidden_units
            penalty      --> penalty
            epochs       --> epochs
            dropout      --> dropout
            learn_rate   --> learn_rate
            activation   --> activation
      
       fit modules:
         engine           mode
          keras     regression
          keras classification
           nnet     regression
           nnet classification
         brulee     regression
         brulee classification
      
       prediction modules:
                   mode engine          methods
         classification brulee      class, prob
         classification  keras class, prob, raw
         classification   nnet class, prob, raw
             regression brulee          numeric
             regression  keras     numeric, raw
             regression   nnet     numeric, raw
      


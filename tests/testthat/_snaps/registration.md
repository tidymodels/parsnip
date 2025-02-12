# adding a new model

    Code
      set_new_model()
    Condition
      Error in `set_new_model()`:
      ! `model` must be a single string, not absent.

---

    Code
      set_new_model(2)
    Condition
      Error in `set_new_model()`:
      ! `model` must be a single string, not the number 2.

---

    Code
      set_new_model(letters[1:2])
    Condition
      Error in `set_new_model()`:
      ! `model` must be a single string, not a character vector.

# existing modes

    Code
      get_from_env("modes")
    Output
      [1] "classification"      "regression"          "censored regression"
      [4] "quantile regression" "unknown"            

# adding a new mode

    Code
      set_model_mode("sponge")
    Condition
      Error in `set_model_mode()`:
      ! `mode` must be a single string, not absent.

# adding a new engine

    Code
      set_model_engine("sponge", eng = "gum")
    Condition
      Error in `set_model_engine()`:
      ! `mode` must be a single string, not absent.

---

    Code
      set_model_engine("sponge", mode = "classification")
    Condition
      Error in `set_model_engine()`:
      ! `eng` must be a single string, not absent.

---

    Code
      set_model_engine("sponge", mode = "regression", eng = "gum")
    Condition
      Error in `set_model_engine()`:
      ! "regression" is not a known mode for model `sponge()`.

# adding a new package

    Code
      set_dependency("sponge", "gum", letters[1:2])
    Condition
      Error in `set_dependency()`:
      ! `pkg` must be a single string, not a character vector.

---

    Code
      set_dependency("sponge", "gummies", "trident")
    Condition
      Error in `set_dependency()`:
      ! The engine "gummies" has not been registered for model "sponge".

---

    Code
      set_dependency("sponge", "gum", "trident", mode = "regression")
    Condition
      Error in `set_dependency()`:
      ! mode "regression" is not a valid mode for "sponge".

# adding a new argument

    Code
      set_model_arg(model = "lunchroom", eng = "gum", parsnip = "modeling", original = "modelling",
        func = list(pkg = "foo", fun = "bar"), has_submodel = FALSE)
    Condition
      Error in `set_model_arg()`:
      ! Model "lunchroom" has not been registered.

---

    Code
      set_model_arg(model = "sponge", eng = "gum", parsnip = "modeling", func = list(
        pkg = "foo", fun = "bar"), has_submodel = FALSE)
    Condition
      Error in `set_model_arg()`:
      ! `original` must be a single string, not absent.

---

    Code
      set_model_arg(model = "sponge", eng = "gum", original = "modelling", func = list(
        pkg = "foo", fun = "bar"), has_submodel = FALSE)
    Condition
      Error in `set_model_arg()`:
      ! `parsnip` must be a single string, not absent.

---

    Code
      set_model_arg(model = "sponge", eng = "gum", parsnip = "modeling", original = "modelling",
        func = "foo::bar", has_submodel = FALSE)
    Condition
      Error in `set_model_arg()`:
      ! `func` should be a named vector with element fun and the optional elements pkg, range, trans, and values. func and pkg should both be single character strings.

---

    Code
      set_model_arg(model = "sponge", eng = "gum", parsnip = "modeling", original = "modelling",
        func = list(pkg = "foo", fun = "bar"), has_submodel = 2)
    Condition
      Error in `set_model_arg()`:
      ! `has_submodel` must be `TRUE` or `FALSE`, not the number 2.

---

    Code
      set_model_arg(model = "sponge", eng = "gum", parsnip = "modeling", original = "modelling",
        func = list(pkg = "foo", fun = "bar"))
    Condition
      Error in `set_model_arg()`:
      ! `has_submodel` must be `TRUE` or `FALSE`, not absent.

---

    Code
      set_model_arg(model = "sponge", eng = "gum", parsnip = "yodeling", original = "yodelling",
        func = c(foo = "a", bar = "b"), has_submodel = FALSE)
    Condition
      Error in `set_model_arg()`:
      ! `func` should be a named vector with element fun and the optional elements pkg, range, trans, and values. func and pkg should both be single character strings.

---

    Code
      set_model_arg(model = "sponge", eng = "gum", parsnip = "yodeling", original = "yodelling",
        func = c(foo = "a"), has_submodel = FALSE)
    Condition
      Error in `set_model_arg()`:
      ! `func` should be a named vector with element fun and the optional elements pkg, range, trans, and values. func and pkg should both be single character strings.

---

    Code
      set_model_arg(model = "sponge", eng = "gum", parsnip = "yodeling", original = "yodelling",
        func = c(fun = 2, pkg = 1), has_submodel = FALSE)
    Condition
      Error in `set_model_arg()`:
      ! The `fun` element of `func` must be a single string, not the number 2.

# adding a new fit

    Code
      set_fit(model = "cactus", eng = "gum", mode = "classification", value = fit_vals)
    Condition
      Error in `set_fit()`:
      ! Model "cactus" has not been registered.

---

    Code
      set_fit(model = "sponge", eng = "nose", mode = "classification", value = fit_vals)
    Condition
      Error in `set_fit()`:
      ! The combination of engine `nose` and mode `classification` has not been registered for model `sponge`.

---

    Code
      set_fit(model = "sponge", eng = "gum", mode = "frog", value = fit_vals)
    Condition
      Error in `set_fit()`:
      ! "frog" is not a known mode for model `sponge()`.

---

    Code
      set_fit(model = "sponge", eng = "gum", mode = "classification", value = fit_vals[
        -i])
    Condition
      Error in `set_fit()`:
      ! The `value` argument should have elements: defaults, func, interface, and protect.

---

    Code
      set_fit(model = "sponge", eng = "gum", mode = "classification", value = fit_vals[
        -i])
    Condition
      Error in `set_fit()`:
      ! The `value` argument should have elements: defaults, func, interface, and protect.

---

    Code
      set_fit(model = "sponge", eng = "gum", mode = "classification", value = fit_vals[
        -i])
    Condition
      Error in `set_fit()`:
      ! The `value` argument should have elements: defaults, func, interface, and protect.

---

    Code
      set_fit(model = "sponge", eng = "gum", mode = "classification", value = fit_vals[
        -i])
    Condition
      Error in `set_fit()`:
      ! The `value` argument should have elements: defaults, func, interface, and protect.

---

    Code
      set_fit(model = "sponge", eng = "gum", mode = "classification", value = fit_vals_0)
    Condition
      Error in `check_interface_val()`:
      ! The interface element should have a single of: data.frame, formula, and matrix.

---

    Code
      set_fit(model = "sponge", eng = "gum", mode = "classification", value = fit_vals_1)
    Condition
      Error in `set_fit()`:
      ! The defaults element should be a list.

---

    Code
      set_fit(model = "sponge", eng = "gum", mode = "classification", value = fit_vals_2)
    Condition
      Error in `check_fit_info()`:
      ! `func` should be a named vector with element fun and the optional elements pkg, range, trans, and values. func and pkg should both be single character strings.

---

    Code
      set_fit(model = "sponge", eng = "gum", mode = "classification", value = fit_vals_3)
    Condition
      Error in `check_interface_val()`:
      ! The interface element should have a single of: data.frame, formula, and matrix.

# adding a new predict method

    Code
      set_pred(model = "cactus", eng = "gum", mode = "classification", type = "class",
        value = class_vals)
    Condition
      Error in `set_pred()`:
      ! Model "cactus" has not been registered.

---

    Code
      set_pred(model = "sponge", eng = "nose", mode = "classification", type = "class",
        value = class_vals)
    Condition
      Error in `set_pred()`:
      ! The combination of engine `nose` and mode `classification` has not been registered for model `sponge`.

---

    Code
      set_pred(model = "sponge", eng = "gum", mode = "classification", type = "eggs",
        value = class_vals)
    Condition
      Error in `set_pred()`:
      ! The prediction type should be one of: "raw", "numeric", "class", "prob", "conf_int", "pred_int", "quantile", "time", "survival", "linear_pred", and "hazard".

---

    Code
      set_pred(model = "sponge", eng = "gum", mode = "frog", type = "class", value = class_vals)
    Condition
      Error in `set_pred()`:
      ! "frog" is not a known mode for model `sponge()`.

---

    Code
      set_pred(model = "sponge", eng = "gum", mode = "classification", type = "class",
        value = class_vals[-i])
    Condition
      Error in `set_pred()`:
      ! The predict module should have elements: "args", "func", "post", and "pre".

---

    Code
      set_pred(model = "sponge", eng = "gum", mode = "classification", type = "class",
        value = class_vals[-i])
    Condition
      Error in `set_pred()`:
      ! The predict module should have elements: "args", "func", "post", and "pre".

---

    Code
      set_pred(model = "sponge", eng = "gum", mode = "classification", type = "class",
        value = class_vals[-i])
    Condition
      Error in `set_pred()`:
      ! The predict module should have elements: "args", "func", "post", and "pre".

---

    Code
      set_pred(model = "sponge", eng = "gum", mode = "classification", type = "class",
        value = class_vals[-i])
    Condition
      Error in `set_pred()`:
      ! The predict module should have elements: "args", "func", "post", and "pre".

---

    Code
      set_pred(model = "sponge", eng = "gum", mode = "classification", type = "class",
        value = class_vals_0)
    Condition
      Error in `set_pred()`:
      ! The `pre` element of `pred_obj` must be a function or `NULL`, not the string "I".

---

    Code
      set_pred(model = "sponge", eng = "gum", mode = "classification", type = "class",
        value = class_vals_1)
    Condition
      Error in `set_pred()`:
      ! The `post` element of `pred_obj` must be a function or `NULL`, not the string "I".

---

    Code
      set_pred(model = "sponge", eng = "gum", mode = "classification", type = "class",
        value = class_vals_2)
    Condition
      Error in `check_pred_info()`:
      ! `func` should be a named vector with element fun and the optional elements pkg, range, trans, and values. func and pkg should both be single character strings.

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
         classification: brulee, brulee_two_layer, keras, nnet
         regression:     brulee, brulee_two_layer, keras, nnet
      
      
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
         brulee_two_layer: 
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
         brulee_two_layer     regression
         brulee_two_layer classification
      
       prediction modules:
                   mode           engine          methods
         classification           brulee      class, prob
         classification brulee_two_layer      class, prob
         classification            keras class, prob, raw
         classification             nnet class, prob, raw
             regression           brulee          numeric
             regression brulee_two_layer          numeric
             regression            keras     numeric, raw
             regression             nnet     numeric, raw
      


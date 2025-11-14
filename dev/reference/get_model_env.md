# Working with the parsnip model environment

These functions read and write to the environment where the package
stores information about model specifications.

## Usage

``` r
get_model_env()

get_from_env(items)

set_in_env(...)

set_env_val(name, value)
```

## Arguments

- items:

  A character string of objects in the model environment.

- ...:

  Named values that will be assigned to the model environment.

- name:

  A single character value for a new symbol in the model environment.

- value:

  A single value for a new value in the model environment.

## References

"How to build a parsnip model"
<https://www.tidymodels.org/learn/develop/models/>

## Examples

``` r
# Access the model data:
current_code <- get_model_env()
ls(envir = current_code)
#>   [1] "C5_rules"                     "C5_rules_args"               
#>   [3] "C5_rules_fit"                 "C5_rules_modes"              
#>   [5] "C5_rules_pkgs"                "C5_rules_predict"            
#>   [7] "auto_ml"                      "auto_ml_args"                
#>   [9] "auto_ml_fit"                  "auto_ml_modes"               
#>  [11] "auto_ml_pkgs"                 "auto_ml_predict"             
#>  [13] "bag_mars"                     "bag_mars_args"               
#>  [15] "bag_mars_fit"                 "bag_mars_modes"              
#>  [17] "bag_mars_pkgs"                "bag_mars_predict"            
#>  [19] "bag_mlp"                      "bag_mlp_args"                
#>  [21] "bag_mlp_fit"                  "bag_mlp_modes"               
#>  [23] "bag_mlp_pkgs"                 "bag_mlp_predict"             
#>  [25] "bag_tree"                     "bag_tree_args"               
#>  [27] "bag_tree_fit"                 "bag_tree_modes"              
#>  [29] "bag_tree_pkgs"                "bag_tree_predict"            
#>  [31] "bart"                         "bart_args"                   
#>  [33] "bart_encoding"                "bart_fit"                    
#>  [35] "bart_modes"                   "bart_pkgs"                   
#>  [37] "bart_predict"                 "boost_tree"                  
#>  [39] "boost_tree_args"              "boost_tree_encoding"         
#>  [41] "boost_tree_fit"               "boost_tree_modes"            
#>  [43] "boost_tree_pkgs"              "boost_tree_predict"          
#>  [45] "cubist_rules"                 "cubist_rules_args"           
#>  [47] "cubist_rules_fit"             "cubist_rules_modes"          
#>  [49] "cubist_rules_pkgs"            "cubist_rules_predict"        
#>  [51] "decision_tree"                "decision_tree_args"          
#>  [53] "decision_tree_encoding"       "decision_tree_fit"           
#>  [55] "decision_tree_modes"          "decision_tree_pkgs"          
#>  [57] "decision_tree_predict"        "discrim_flexible"            
#>  [59] "discrim_flexible_args"        "discrim_flexible_fit"        
#>  [61] "discrim_flexible_modes"       "discrim_flexible_pkgs"       
#>  [63] "discrim_flexible_predict"     "discrim_linear"              
#>  [65] "discrim_linear_args"          "discrim_linear_fit"          
#>  [67] "discrim_linear_modes"         "discrim_linear_pkgs"         
#>  [69] "discrim_linear_predict"       "discrim_quad"                
#>  [71] "discrim_quad_args"            "discrim_quad_fit"            
#>  [73] "discrim_quad_modes"           "discrim_quad_pkgs"           
#>  [75] "discrim_quad_predict"         "discrim_regularized"         
#>  [77] "discrim_regularized_args"     "discrim_regularized_fit"     
#>  [79] "discrim_regularized_modes"    "discrim_regularized_pkgs"    
#>  [81] "discrim_regularized_predict"  "gen_additive_mod"            
#>  [83] "gen_additive_mod_args"        "gen_additive_mod_encoding"   
#>  [85] "gen_additive_mod_fit"         "gen_additive_mod_modes"      
#>  [87] "gen_additive_mod_pkgs"        "gen_additive_mod_predict"    
#>  [89] "linear_reg"                   "linear_reg_args"             
#>  [91] "linear_reg_encoding"          "linear_reg_fit"              
#>  [93] "linear_reg_modes"             "linear_reg_pkgs"             
#>  [95] "linear_reg_predict"           "logistic_reg"                
#>  [97] "logistic_reg_args"            "logistic_reg_encoding"       
#>  [99] "logistic_reg_fit"             "logistic_reg_modes"          
#> [101] "logistic_reg_pkgs"            "logistic_reg_predict"        
#> [103] "mars"                         "mars_args"                   
#> [105] "mars_encoding"                "mars_fit"                    
#> [107] "mars_modes"                   "mars_pkgs"                   
#> [109] "mars_predict"                 "mlp"                         
#> [111] "mlp_args"                     "mlp_encoding"                
#> [113] "mlp_fit"                      "mlp_modes"                   
#> [115] "mlp_pkgs"                     "mlp_predict"                 
#> [117] "models"                       "modes"                       
#> [119] "multinom_reg"                 "multinom_reg_args"           
#> [121] "multinom_reg_encoding"        "multinom_reg_fit"            
#> [123] "multinom_reg_modes"           "multinom_reg_pkgs"           
#> [125] "multinom_reg_predict"         "naive_Bayes"                 
#> [127] "naive_Bayes_args"             "naive_Bayes_fit"             
#> [129] "naive_Bayes_modes"            "naive_Bayes_pkgs"            
#> [131] "naive_Bayes_predict"          "nearest_neighbor"            
#> [133] "nearest_neighbor_args"        "nearest_neighbor_encoding"   
#> [135] "nearest_neighbor_fit"         "nearest_neighbor_modes"      
#> [137] "nearest_neighbor_pkgs"        "nearest_neighbor_predict"    
#> [139] "null_model"                   "null_model_args"             
#> [141] "null_model_encoding"          "null_model_fit"              
#> [143] "null_model_modes"             "null_model_pkgs"             
#> [145] "null_model_predict"           "pls"                         
#> [147] "pls_args"                     "pls_fit"                     
#> [149] "pls_modes"                    "pls_pkgs"                    
#> [151] "pls_predict"                  "poisson_reg"                 
#> [153] "poisson_reg_args"             "poisson_reg_fit"             
#> [155] "poisson_reg_modes"            "poisson_reg_pkgs"            
#> [157] "poisson_reg_predict"          "proportional_hazards"        
#> [159] "proportional_hazards_args"    "proportional_hazards_fit"    
#> [161] "proportional_hazards_modes"   "proportional_hazards_pkgs"   
#> [163] "proportional_hazards_predict" "rand_forest"                 
#> [165] "rand_forest_args"             "rand_forest_encoding"        
#> [167] "rand_forest_fit"              "rand_forest_modes"           
#> [169] "rand_forest_pkgs"             "rand_forest_predict"         
#> [171] "rule_fit"                     "rule_fit_args"               
#> [173] "rule_fit_fit"                 "rule_fit_modes"              
#> [175] "rule_fit_pkgs"                "rule_fit_predict"            
#> [177] "surv_reg"                     "surv_reg_args"               
#> [179] "surv_reg_encoding"            "surv_reg_fit"                
#> [181] "surv_reg_modes"               "surv_reg_pkgs"               
#> [183] "surv_reg_predict"             "survival_reg"                
#> [185] "survival_reg_args"            "survival_reg_fit"            
#> [187] "survival_reg_modes"           "survival_reg_pkgs"           
#> [189] "survival_reg_predict"         "svm_linear"                  
#> [191] "svm_linear_args"              "svm_linear_encoding"         
#> [193] "svm_linear_fit"               "svm_linear_modes"            
#> [195] "svm_linear_pkgs"              "svm_linear_predict"          
#> [197] "svm_poly"                     "svm_poly_args"               
#> [199] "svm_poly_encoding"            "svm_poly_fit"                
#> [201] "svm_poly_modes"               "svm_poly_pkgs"               
#> [203] "svm_poly_predict"             "svm_rbf"                     
#> [205] "svm_rbf_args"                 "svm_rbf_encoding"            
#> [207] "svm_rbf_fit"                  "svm_rbf_modes"               
#> [209] "svm_rbf_pkgs"                 "svm_rbf_predict"             
```

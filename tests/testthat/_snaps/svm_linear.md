# updating

    Code
      svm_linear(mode = "regression", cost = 2) %>% set_engine("kernlab", cross = 10) %>%
        update(cross = tune(), cost = tune())
    Output
      Linear Support Vector Machine Model Specification (regression)
      
      Main Arguments:
        cost = tune()
      
      Engine-Specific Arguments:
        cross = tune()
      
      Computational engine: kernlab 
      

# linear svm classification prediction: LiblineaR

    Code
      predict(cls_form, hpc_no_m[ind, -5], type = "prob")
    Condition
      Error in `check_spec_pred_type()`:
      ! No "prob" prediction method available for this model. Value for `type` should be one of: "class" and "raw".

---

    Code
      predict(cls_xy_form, hpc_no_m[ind, -5], type = "prob")
    Condition
      Error in `check_spec_pred_type()`:
      ! No "prob" prediction method available for this model. Value for `type` should be one of: "class" and "raw".


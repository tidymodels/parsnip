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
      

# bad input

    Code
      translate(svm_linear(mode = "regression") %>% set_engine(NULL))
    Condition
      Error in `translate.default()`:
      ! Please set an engine.

---

    Code
      svm_linear(mode = "reallyunknown")
    Condition
      Error in `svm_linear()`:
      ! "reallyunknown" is not a known mode for model `svm_linear()`.

---

    Code
      translate(svm_linear(mode = "regression") %>% set_engine("LiblineaR", type = 3))
    Condition
      Error in `translate()`:
      ! The LiblineaR engine argument `type = 3` does not correspond to an SVM regression model.

---

    Code
      translate(svm_linear(mode = "classification") %>% set_engine("LiblineaR", type = 11))
    Condition
      Error in `translate()`:
      ! The LiblineaR engine argument of `type = 11` does not correspond to an SVM classification model.

# linear svm classification prediction: LiblineaR

    Code
      predict(cls_form, hpc_no_m[ind, -5], type = "prob")
    Condition
      Error in `predict()`:
      ! No "prob" prediction method available for this model. `type` should be one of: "class" and "raw".

---

    Code
      predict(cls_xy_form, hpc_no_m[ind, -5], type = "prob")
    Condition
      Error in `predict()`:
      ! No "prob" prediction method available for this model. `type` should be one of: "class" and "raw".


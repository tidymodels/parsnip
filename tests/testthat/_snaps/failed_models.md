# numeric model

    Code
      num_res <- predict(lm_mod, hpc_bad[1:11, -1])
    Condition
      Warning:
      Model fit failed; cannot make predictions.

---

    Code
      ci_res <- predict(lm_mod, hpc_bad[1:11, -1], type = "conf_int")
    Condition
      Warning:
      Model fit failed; cannot make predictions.

---

    Code
      pi_res <- predict(lm_mod, hpc_bad[1:11, -1], type = "pred_int")
    Condition
      Warning:
      Model fit failed; cannot make predictions.

# classification model

    Code
      cls_res <- predict(log_reg, lending_club %>% dplyr::slice(1:7) %>% dplyr::select(
        -Class))
    Condition
      Warning:
      Model fit failed; cannot make predictions.

---

    Code
      prb_res <- predict(log_reg, lending_club %>% dplyr::slice(1:7) %>% dplyr::select(
        -Class), type = "prob")
    Condition
      Warning:
      Model fit failed; cannot make predictions.

---

    Code
      ci_res <- predict(log_reg, lending_club %>% dplyr::slice(1:7) %>% dplyr::select(
        -Class), type = "conf_int")
    Condition
      Warning:
      Model fit failed; cannot make predictions.


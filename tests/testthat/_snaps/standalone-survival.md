# convert survival data to factor

    Code
      .time_as_binary_event(surv_obj, 11:12)
    Condition
      Error in `.time_as_binary_event()`:
      ! 'eval_time' should be a single, complete, finite numeric value.

---

    Code
      .time_as_binary_event(surv_obj, Inf)
    Condition
      Error in `.time_as_binary_event()`:
      ! 'eval_time' should be a single, complete, finite numeric value.

---

    Code
      .time_as_binary_event(surv_obj, NA)
    Condition
      Error in `.time_as_binary_event()`:
      ! 'eval_time' should be a single, complete, finite numeric value.

---

    Code
      .time_as_binary_event(surv_obj, -1)
    Condition
      Error in `.time_as_binary_event()`:
      ! 'eval_time' should be a single, complete, finite numeric value.

---

    Code
      .time_as_binary_event(surv_obj, "potato")
    Condition
      Error in `.time_as_binary_event()`:
      ! 'eval_time' should be a single, complete, finite numeric value.


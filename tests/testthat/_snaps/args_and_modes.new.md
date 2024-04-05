# can't set a mode that isn't allowed by the model spec

    Code
      set_mode(linear_reg(), "classification")
    Condition
      Error in `set_mode()`:
      ! "classification" is not a known mode for model `linear_reg()`.

# unavailable modes for an engine and vice-versa

    Code
      decision_tree() %>% set_mode("regression") %>% set_engine("C5.0")
    Condition
      Error in `set_engine()`:
      ! Available modes for engine C5.0 are: "unknown" and "classification".

---

    Code
      decision_tree(mode = "regression", engine = "C5.0")
    Condition
      Error in `decision_tree()`:
      ! Available modes for engine C5.0 are: "unknown" and "classification".

---

    Code
      decision_tree() %>% set_engine("C5.0") %>% set_mode("regression")
    Condition
      Error in `new_model_spec()`:
      ! The engine C5.0 only supports the mode "classification", "unknown" was requested.

---

    Code
      decision_tree(engine = NULL) %>% set_engine("C5.0") %>% set_mode("regression")
    Condition
      Error in `if (object$engine == "C5.0" && object$mode != "classification") ...`:
      ! missing value where TRUE/FALSE needed

---

    Code
      decision_tree(engine = NULL) %>% set_mode("regression") %>% set_engine("C5.0")
    Condition
      Error in `if (object$engine == "C5.0" && object$mode != "classification") ...`:
      ! missing value where TRUE/FALSE needed

---

    Code
      proportional_hazards() %>% set_mode("regression")
    Condition
      Error in `set_mode()`:
      ! "regression" is not a known mode for model `proportional_hazards()`.

---

    Code
      linear_reg() %>% set_mode()
    Condition
      Error in `set_mode()`:
      ! Available modes for model type linear_reg are: "unknown" and "regression".

---

    Code
      linear_reg(engine = "boop")
    Condition
      Error in `linear_reg()`:
      x Engine "boop" is not supported for `linear_reg()`
      i See `show_engines("linear_reg")`.

---

    Code
      linear_reg() %>% set_engine()
    Condition
      Error in `set_engine()`:
      ! Missing engine. Possible mode/engine combinations are: regression {lm, glm, glmnet, stan, spark, keras, brulee}.

---

    Code
      proportional_hazards() %>% set_engine()
    Condition
      Error in `set_engine()`:
      ! No known engines for `proportional_hazards()`.

# set_* functions error when input isn't model_spec

    Code
      set_mode(mtcars, "regression")
    Condition
      Error in `set_mode()`:
      ! `set_mode()` expected a model specification to be supplied to the `object` argument, but received a(n) `data.frame` object.

---

    Code
      set_args(mtcars, blah = "blah")
    Condition
      Error in `set_args()`:
      ! `set_args()` expected a model specification to be supplied to the `object` argument, but received a(n) `data.frame` object.

---

    Code
      bag_tree %>% set_mode("classification")
    Condition
      Error in `set_mode()`:
      ! `set_mode()` expected a model specification to be supplied to the `object` argument, but received a(n) `function` object.
      i Did you mistakenly pass `model_function` rather than `model_function()`?

---

    Code
      bag_tree %>% set_engine("rpart")
    Condition
      Error in `set_engine()`:
      ! `set_engine()` expected a model specification to be supplied to the `object` argument, but received a(n) `function` object.
      i Did you mistakenly pass `model_function` rather than `model_function()`?

---

    Code
      bag_tree %>% set_args(boop = "bop")
    Condition
      Error in `set_args()`:
      ! `set_args()` expected a model specification to be supplied to the `object` argument, but received a(n) `function` object.
      i Did you mistakenly pass `model_function` rather than `model_function()`?

---

    Code
      1L %>% set_args(mode = "classification")
    Condition
      Error in `set_args()`:
      ! `set_args()` expected a model specification to be supplied to the `object` argument, but received a(n) `integer` object.

---

    Code
      bag_tree %>% set_mode("classification")
    Condition
      Error in `set_mode()`:
      ! `set_mode()` expected a model specification to be supplied to the `object` argument, but received a(n) `function` object.
      i Did you mistakenly pass `model_function` rather than `model_function()`?


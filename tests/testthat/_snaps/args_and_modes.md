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


# pipe arguments

    Code
      set_args(rand_forest())
    Condition
      Error in `set_args()`:
      ! Please pass at least one named argument.

# pipe engine

    Code
      set_mode(rand_forest())
    Condition
      Error in `set_mode()`:
      ! Available modes for model type rand_forest are: "unknown", "classification", "regression", and "censored regression".

---

    Code
      set_mode(rand_forest(), 2)
    Condition
      Error in `set_mode()`:
      ! 2 is not a known mode for model `rand_forest()`.

---

    Code
      set_mode(rand_forest(), "haberdashery")
    Condition
      Error in `set_mode()`:
      ! "haberdashery" is not a known mode for model `rand_forest()`.

# can't set a mode that isn't allowed by the model spec

    Code
      set_mode(linear_reg(), "classification")
    Condition
      Error in `set_mode()`:
      ! "classification" is not a known mode for model `linear_reg()`.

# unavailable modes for an engine and vice-versa

    Code
      set_engine(set_mode(decision_tree(), "regression"), "C5.0")
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
      set_mode(set_engine(decision_tree(), "C5.0"), "regression")
    Condition
      Error in `set_mode()`:
      ! Available modes for engine C5.0 are: "unknown" and "classification".

---

    Code
      set_mode(set_engine(decision_tree(engine = NULL), "C5.0"), "regression")
    Condition
      Error in `set_mode()`:
      ! Available modes for engine C5.0 are: "unknown" and "classification".

---

    Code
      set_engine(set_mode(decision_tree(engine = NULL), "regression"), "C5.0")
    Condition
      Error in `set_engine()`:
      ! Available modes for engine C5.0 are: "unknown" and "classification".

---

    Code
      set_mode(proportional_hazards(), "regression")
    Condition
      Error in `set_mode()`:
      ! "regression" is not a known mode for model `proportional_hazards()`.

---

    Code
      set_mode(linear_reg())
    Condition
      Error in `set_mode()`:
      ! Available modes for model type linear_reg are: "unknown", "regression", and "quantile regression".

---

    Code
      linear_reg(engine = "boop")
    Condition
      Error in `linear_reg()`:
      x Engine "boop" is not supported for `linear_reg()`
      i See `show_engines("linear_reg")`.

---

    Code
      set_engine(linear_reg())
    Condition
      Error in `set_engine()`:
      ! Missing engine. Possible mode/engine combinations are: quantile regression {quantreg} and regression {lm, glm, glmnet, stan, spark, keras, brulee}.

---

    Code
      set_engine(proportional_hazards())
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
      set_mode(bag_tree, "classification")
    Condition
      Error in `set_mode()`:
      ! `set_mode()` expected a model specification to be supplied to the `object` argument, but received a(n) `function` object.
      i Did you mistakenly pass `model_function` rather than `model_function()`?

---

    Code
      set_engine(bag_tree, "rpart")
    Condition
      Error in `set_engine()`:
      ! `set_engine()` expected a model specification to be supplied to the `object` argument, but received a(n) `function` object.
      i Did you mistakenly pass `model_function` rather than `model_function()`?

---

    Code
      set_args(bag_tree, boop = "bop")
    Condition
      Error in `set_args()`:
      ! `set_args()` expected a model specification to be supplied to the `object` argument, but received a(n) `function` object.
      i Did you mistakenly pass `model_function` rather than `model_function()`?

---

    Code
      set_args(1L, mode = "classification")
    Condition
      Error in `set_args()`:
      ! `set_args()` expected a model specification to be supplied to the `object` argument, but received a(n) `integer` object.

---

    Code
      set_mode(bag_tree, "classification")
    Condition
      Error in `set_mode()`:
      ! `set_mode()` expected a model specification to be supplied to the `object` argument, but received a(n) `function` object.
      i Did you mistakenly pass `model_function` rather than `model_function()`?


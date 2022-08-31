# combine_words helper works

    Code
      combine_words(1)
    Output
      1

---

    Code
      combine_words(1:2)
    Output
      1 and 2

---

    Code
      combine_words(1:3)
    Output
      1, 2, and 3

---

    Code
      combine_words(1:4)
    Output
      1, 2, 3, and 4

# model type functions message informatively with unknown implementation

    Code
      bag_tree() %>% set_engine("rpart")
    Message
      parsnip could not locate an implementation for `bag_tree`  model specifications using the `rpart` engine.
    Output
      Bagged Decision Tree Model Specification (unknown)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

# set_engine works as a generic

    Code
      set_engine(mtcars, "rpart")
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'set_engine' applied to an object of class "data.frame"


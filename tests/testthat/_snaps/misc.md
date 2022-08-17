# model type functions message informatively with unknown implementation

    Code
      bag_tree() %>% set_engine("rpart") %>% set_mode("regression")
    Message
      ! parsnip could not locate an implementation for `bag_tree` regression model specifications using the `rpart` engine.
      i The parsnip extension package baguette implements support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (regression)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

---

    Code
      bag_tree() %>% set_mode("censored regression")
    Message
      ! parsnip could not locate an implementation for `bag_tree` censored regression model specifications using the `rpart` engine.
      i The parsnip extension package censored implements support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (censored regression)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

---

    Code
      bag_tree()
    Message
      ! parsnip could not locate an implementation for `bag_tree` model specifications using the `rpart` engine.
      i The parsnip extension packages censored and baguette implement support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (unknown)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

---

    Code
      bag_tree() %>% set_engine("rpart")
    Message
      ! parsnip could not locate an implementation for `bag_tree` model specifications using the `rpart` engine.
      i The parsnip extension packages censored and baguette implement support for this specification.
      i Please install (if needed) and load to continue.
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


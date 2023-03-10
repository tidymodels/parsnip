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
      ! parsnip could not locate an implementation for `bag_tree` censored regression model specifications.
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
      ! parsnip could not locate an implementation for `bag_tree` model specifications.
      i The parsnip extension packages censored and baguette implement support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (unknown mode)
      
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
      Bagged Decision Tree Model Specification (unknown mode)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

# missing implementation checks prompt conservatively with old objects

    Code
      bt
    Message
      ! parsnip could not locate an implementation for `bag_tree` model specifications.
      i The parsnip extension packages censored and baguette implement support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (regression)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

# set_engine works as a generic

    Code
      set_engine(mtcars, "rpart")
    Condition
      Error in `set_engine()`:
      ! `set_engine()` expected a model specification to be supplied to the `object` argument, but received a(n) `data.frame` object.

# check_for_newdata points out correct context

    Code
      fn(newdata = "boop!")
    Condition
      Error in `fn()`:
      ! Please use `new_data` instead of `newdata`.


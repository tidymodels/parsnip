
# In some cases, the test value needs to be wrapped in an empty
#  environment. If arguments are set in the model specification
# (as opposed to being set by a `translate` function), they will 
# need this wrapper. 

new_empty_quosure <- function(expr) {
  new_quosure(expr, env = empty_env())
}


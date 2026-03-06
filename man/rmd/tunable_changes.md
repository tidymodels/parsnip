

There's a few different times that we want to alter the tuning parameter information: 

 - To change the default for a specific model and engine (e.g. boosted trees often use higher `learn_rate` values than other models).
 - To add possible engine arguments. 

`add_engine_parameters()` adds engine-specific tunable parameters to a base result. If any base parameters have the same name as engine parameters, the engine versions replace them.

`apply_tunable_spec()` is a higher-level function that applies a specification list to modify tunable parameters. It supports three patterns:

- Adding parameters, 
- Updating existing parameters, or 
- Completely replacing the parameter value's information.

Examples are below. 

## Structure of the `call_info`

`call_info` is just a list that is used to create a new \pkg{dials} parameter object. For example: 


``` r
penalty_info <- list(pkg = "dials", fun = "penalty")

# How it is used internally: 
rlang::call2(.ns = penalty_info$pkg,  penalty_info$fun)
#> dials::penalty()
```

The list names should match parameter names, and values should be lists with `pkg`, `fun`, and optionally `range` or `values`. Use this to change the \pkg{dials} function or range for existing parameters. 

**Note**: for engines defined in \pkg{parsnip} and its extensions, parameter ranges should be set when the argument is registered via `set_model_arg()`. The updates mechanism is a another way of doing the same thing if the preferred method cannot be used. 

## Examples

Let's say that you are writing a parsnip extension package for your fancy new _GlamNet_ model (a fabulous regularized linear regression technique). Your package has registration code that looks like: 

```r
set_model_engine("linear_reg", "regression", "GlamNet")
set_dependency("linear_reg", "GlamNet", "GlamNet", mode = "regression")

set_fit(
  model = "linear_reg",
  eng = "GlamNet",
  mode = "regression",
  value = list(
    interface = "matrix",
    protect = c("x", "y", "weights"),
    func = c(pkg = "GlamNet", fun = "GlamNet")
  )
)

set_encoding(
  model = "linear_reg",
  eng = "GlamNet",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = TRUE
  )
)

set_model_arg(
  model = "linear_reg",
  eng = "GlamNet",
  parsnip = "penalty",
  original = "lambda",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = TRUE
)

set_model_arg(
  model = "linear_reg",
  eng = "GlamNet",
  parsnip = "mixture",
  original = "alpha",
  func = list(pkg = "dials", fun = "mixture"),
  has_submodel = FALSE
)

# etc etc
```

However, you want to do two things: 

 - Update the main argument `mixture` to have a range of [0.9, 1].
 - Tell tidymodels about the engine argument `glitter_amount` in case someone wants to tune it. 

For the first issue, you can modify the registration code to be

```r
set_model_arg(
  model = "linear_reg",
  eng = "GlamNet",
  parsnip = "mixture",
  original = "alpha",
  func = list(pkg = "dials", fun = "mixture", range = c(0.9, 1.0)),
  has_submodel = FALSE
)
```

This is the _preferred_ method but another will be shown below. 

For the second issue related to engine arguments, you make a tibble of information about how to call a \pkg{dials} parameter function for `glitter_amount`:


``` r
glam_engine_params <- tibble::tibble(
  name = "glitter_amount",
  # The dials parameter can be in your extension package or in dials
  call_info = list(list(pkg = "GlamNet", fun = "glitter_amount")),
  source = "model_spec",
  component = "linear_reg",
  component_id = "engine"
)
```

Let's _pretend_ that the `GlamNet` model spec is the same as what we would get from `glmnet`: 


``` r
glam_spec <- linear_reg() |> set_engine("glmnet")
```

If we want to append the engine parameter information to what we would get from `tunable()`, we use


``` r
add_engine_parameters(tunable(glam_spec), glam_engine_params)
#> # A tibble: 3 x 5
#>   name           call_info        source     component  component_id
#>   <chr>          <list>           <chr>      <chr>      <chr>       
#> 1 penalty        <named list [2]> model_spec linear_reg main        
#> 2 mixture        <named list [3]> model_spec linear_reg main        
#> 3 glitter_amount <named list [2]> model_spec linear_reg engine
```


In my `tunable()` methods, I can call 

First, create a tibble of information about the main arguments.


``` r
library(parsnip)

# Example base tunable result (as would come from NextMethod())
base_result <- tibble::tibble(
  name = c("penalty", "mixture"),
  call_info = list(
    list(pkg = "dials", fun = "penalty"),
    list(pkg = "dials", fun = "mixture")
  ),
  source = "model_spec",
  component = "linear_reg",
  component_id = "main"
)

# Adding engine-specific parameters with add_engine_parameters()
engine_params <- tibble::tibble(
  name = "num_leaves",
  call_info = list(list(pkg = "dials", fun = "num_leaves")),
  source = "model_spec",
  component = "linear_reg",
  component_id = "engine"
)

add_engine_parameters(base_result, engine_params)
#> # A tibble: 3 x 5
#>   name       call_info        source     component  component_id
#>   <chr>      <list>           <chr>      <chr>      <chr>       
#> 1 penalty    <named list [2]> model_spec linear_reg main        
#> 2 mixture    <named list [2]> model_spec linear_reg main        
#> 3 num_leaves <named list [2]> model_spec linear_reg engine
```

Using `apply_tunable_spec()` with `add_params`:


``` r
spec_add <- list(
  my_engine = list(add_params = engine_params)
)

apply_tunable_spec(base_result, "my_engine", spec_add)
#> # A tibble: 3 x 5
#>   name       call_info        source     component  component_id
#>   <chr>      <list>           <chr>      <chr>      <chr>       
#> 1 penalty    <named list [2]> model_spec linear_reg main        
#> 2 mixture    <named list [2]> model_spec linear_reg main        
#> 3 num_leaves <named list [2]> model_spec linear_reg engine
```

Using `apply_tunable_spec()` with `updates`:


``` r
spec_update <- list(
  my_engine = list(
    updates = list(
      mixture = list(pkg = "dials", fun = "mixture", range = c(0.05, 1.0))
    )
  )
)

apply_tunable_spec(base_result, "my_engine", spec_update)
#> # A tibble: 2 x 5
#>   name    call_info        source     component  component_id
#>   <chr>   <list>           <chr>      <chr>      <chr>       
#> 1 penalty <named list [2]> model_spec linear_reg main        
#> 2 mixture <named list [3]> model_spec linear_reg main
```

Using `apply_tunable_spec()` with `replace_fn`:


``` r
spec_replace <- list(
  my_engine = list(
    replace_fn = function(base, component) {
      tibble::tibble(
        name = "custom_param",
        call_info = list(list(pkg = "dials", fun = "custom")),
        source = "model_spec",
        component = component,
        component_id = "engine"
      )
    }
  )
)

apply_tunable_spec(base_result, "my_engine", spec_replace)
#> # A tibble: 1 x 5
#>   name         call_info        source     component  component_id
#>   <chr>        <list>           <chr>      <chr>      <chr>       
#> 1 custom_param <named list [2]> model_spec linear_reg engine
```

Unknown engines return `base_result` unchanged:


``` r
apply_tunable_spec(base_result, "unknown_engine", spec_add)
#> # A tibble: 2 x 5
#>   name    call_info        source     component  component_id
#>   <chr>   <list>           <chr>      <chr>      <chr>       
#> 1 penalty <named list [2]> model_spec linear_reg main        
#> 2 mixture <named list [2]> model_spec linear_reg main
```

# Functions required for parsnip-adjacent packages

These functions are helpful when creating new packages that will
register new model specifications.

## Usage

``` r
null_value(x)

show_fit(model, eng)

check_args(object, call = rlang::caller_env())

update_dot_check(...)

new_model_spec(
  cls,
  args,
  eng_args,
  mode,
  user_specified_mode = TRUE,
  method,
  engine,
  user_specified_engine = TRUE
)

check_final_param(x, call = rlang::caller_env())

update_main_parameters(args, param, call = rlang::caller_env())

update_engine_parameters(eng_args, fresh, ...)

print_model_spec(x, cls = class(x)[1], desc = get_model_desc(cls), ...)

update_spec(
  object,
  parameters,
  args_enquo_list,
  fresh,
  cls,
  ...,
  call = caller_env()
)

is_varying(x)
```

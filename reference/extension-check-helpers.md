# Model Specification Checking:

The helpers `spec_is_possible()`, `spec_is_loaded()`, and
`prompt_missing_implementation()` provide tooling for checking model
specifications. In addition to the `spec`, `engine`, and `mode`
arguments, the functions take arguments `user_specified_engine` and
`user_specified_mode`, denoting whether the user themselves has
specified the engine or mode, respectively.

## Usage

``` r
spec_is_possible(
  spec,
  engine = spec$engine,
  user_specified_engine = spec$user_specified_engine,
  mode = spec$mode,
  user_specified_mode = spec$user_specified_mode
)

spec_is_loaded(
  spec,
  engine = spec$engine,
  user_specified_engine = spec$user_specified_engine,
  mode = spec$mode,
  user_specified_mode = spec$user_specified_mode
)

prompt_missing_implementation(
  spec,
  engine = spec$engine,
  user_specified_engine = spec$user_specified_engine,
  mode = spec$mode,
  user_specified_mode = spec$user_specified_mode,
  prompt,
  ...
)
```

## Details

`spec_is_possible()` checks against the union of

- the current parsnip model environment and

- the `model_info_table` of "pre-registered" model specifications

to determine whether a model is well-specified. See
`parsnip:::model_info_table` for this table.

`spec_is_loaded()` checks only against the current parsnip model
environment.

`spec_is_possible()` is executed automatically on
[`new_model_spec()`](https://parsnip.tidymodels.org/reference/add_on_exports.md),
[`set_mode()`](https://parsnip.tidymodels.org/reference/set_args.md),
and
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md),
and `spec_is_loaded()` is executed automatically in
`print.model_spec()`, among other places. `spec_is_possible()` should be
used when a model specification is still "in progress" of being
specified, while `spec_is_loaded` should only be called when parsnip or
an extension receives some indication that the user is "done" specifying
a model specification: at print, fit, addition to a workflow, or
`extract_*()`, for example.

When `spec_is_loaded()` is `FALSE`, the
`prompt_missing_implementation()` helper will construct an informative
message to prompt users to load or install needed packages. It's
`prompt` argument refers to the prompting function to use, usually
[cli::cli_inform](https://cli.r-lib.org/reference/cli_abort.html) or
[cli::cli_abort](https://cli.r-lib.org/reference/cli_abort.html), and
the ellipses are passed to that function.

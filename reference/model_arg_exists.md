# Check if a model argument is already registered

This function checks whether a specific argument has already been
registered for a model-engine combination. This is useful for extension
packages that want to avoid re-registering arguments that parsnip has
already registered.

## Usage

``` r
model_arg_exists(model, eng, parsnip, original)
```

## Arguments

- model:

  A character string for the model type (e.g., "rand_forest").

- eng:

  A character string for the engine.

- parsnip:

  A character string for the parsnip argument name.

- original:

  A character string for the original engine argument name.

## Value

A logical value indicating whether the argument is already registered.

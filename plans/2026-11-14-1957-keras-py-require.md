# Declare Python requirements for the legacy keras engine

## Overview

CRAN asked us to fix how parsnip's keras code interacts with Python so the package can stay safely on CRAN. Two problems:

1. `tests/testthat/test-aaaa.R` called `keras:::backend()` unconditionally, which made reticulate try to find/provision Python (running `uv`) on CRAN machines and print "To use legacy Keras via py_require(), call py_require_legacy_keras() ...".
2. The legacy `"keras"` engine never declares its Python requirements. Since keras 2.16.0, users are expected to call `keras::py_require_legacy_keras()` before Python starts; parsnip users never call `library(keras)` so they cannot know to do this. The `keras3` engine is fine because keras3 calls `py_require()` in its own `.onLoad()`.

Constraint: parsnip must not take a new dependency on keras, reticulate, or tensorflow. New code builds those calls as expressions (`rlang::call2(.ns = )`) and evaluates them through a single internal evaluator.

## Checklist

- [x] Remove the unguarded `keras:::backend()` call from `tests/testthat/test-aaaa.R`
- [x] Add internal `keras_py_require()` helper (expression-based, no new deps) in `R/mlp.R`
- [x] Call it from `keras_mlp()`, `set_tf_seed()`, and `keras_predict_classes()`
- [x] Tests with mocked bindings in `tests/testthat/test-mlp.R`
- [x] NEWS bullet
- [x] `air format .` and `R CMD check`

## Details

- `keras_py_require()` does nothing unless keras >= 2.16.0 is installed (that is where `py_require_legacy_keras()` was introduced).
- It also does nothing once Python is initialized (`reticulate::py_available(initialize = FALSE)`), because `py_require()` cannot change the environment after that point.
- It calls `keras::py_require_legacy_keras(extra_packages = FALSE)` to declare only what parsnip needs (`tf-keras`, `tensorflow`, `numpy<2`) rather than the hub/datasets/etc. extras.
- reticulate is a hard dependency of keras, so if keras is installed reticulate is too; no separate install check is needed.
- Existing `keras::`/`tensorflow::` calls in `R/mlp.R` are left as they are (out of scope).

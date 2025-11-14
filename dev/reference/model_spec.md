# Model Specifications

The parsnip package splits the process of fitting models into two steps:

1.  Specify how a model will be fit using a *model specification*

2.  Fit a model using the model specification

This is a different approach to many other model interfaces in R, like
[`lm()`](https://rdrr.io/r/stats/lm.html), where both the specification
of the model and the fitting happens in one function call. Splitting the
process into two steps allows users to iteratively define model
specifications throughout the model development process.

This intermediate object that defines how the model will be fit is
called a *model specification* and has class `model_spec`. Model type
functions, like
[`linear_reg()`](https://parsnip.tidymodels.org/dev/reference/linear_reg.md)
or
[`boost_tree()`](https://parsnip.tidymodels.org/dev/reference/boost_tree.md),
return `model_spec` objects.

Fitted model objects, resulting from passing a `model_spec` to
[fit()](https://parsnip.tidymodels.org/dev/reference/fit.md) or
[fit_xy](https://parsnip.tidymodels.org/dev/reference/fit.md), have
class `model_fit`, and contain the original `model_spec` objects inside
them. See
[?model_fit](https://parsnip.tidymodels.org/dev/reference/model_fit.md)
for more on that object type, and
[?extract_spec_parsnip](https://parsnip.tidymodels.org/dev/reference/extract-parsnip.md)
to extract `model_spec`s from `model_fit`s.

## Details

An object with class `"model_spec"` is a container for information about
a model that will be fit.

The main elements of the object are:

- `args`: A vector of the main arguments for the model. The names of
  these arguments may be different from their counterparts n the
  underlying model function. For example, for a `glmnet` model, the
  argument name for the amount of the penalty is called "penalty"
  instead of "lambda" to make it more general and usable across
  different types of models (and to not be specific to a particular
  model function). The elements of `args` can
  [`tune()`](https://hardhat.tidymodels.org/reference/tune.html) with
  the use of the [tune package](https://tune.tidymodels.org/). For more
  information see <https://www.tidymodels.org/start/tuning/>. If left to
  their defaults (`NULL`), the arguments will use the underlying model
  functions default value. As discussed below, the arguments in `args`
  are captured as quosures and are not immediately executed.

- `...`: Optional model-function-specific parameters. As with `args`,
  these will be quosures and can be
  [`tune()`](https://hardhat.tidymodels.org/reference/tune.html).

- `mode`: The type of model, such as "regression" or "classification".
  Other modes will be added once the package adds more functionality.

- `method`: This is a slot that is filled in later by the model's
  constructor function. It generally contains lists of information that
  are used to create the fit and prediction code as well as required
  packages and similar data.

- `engine`: This character string declares exactly what software will be
  used. It can be a package name or a technology type.

This class and structure is the basis for how parsnip stores model
objects prior to seeing the data.

## Argument Details

An important detail to understand when creating model specifications is
that they are intended to be functionally independent of the data. While
it is true that some tuning parameters are *data dependent*, the model
specification does not interact with the data at all.

For example, most R functions immediately evaluate their arguments. For
example, when calling `mean(dat_vec)`, the object `dat_vec` is
immediately evaluated inside of the function.

parsnip model functions do not do this. For example, using

     rand_forest(mtry = ncol(mtcars) - 1)

**does not** execute `ncol(mtcars) - 1` when creating the specification.
This can be seen in the output:

     > rand_forest(mtry = ncol(mtcars) - 1)
     Random Forest Model Specification (unknown)

     Main Arguments:
       mtry = ncol(mtcars) - 1

The model functions save the argument *expressions* and their associated
environments (a.k.a. a quosure) to be evaluated later when either
[`fit.model_spec()`](https://parsnip.tidymodels.org/dev/reference/fit.md)
or
[`fit_xy.model_spec()`](https://parsnip.tidymodels.org/dev/reference/fit.md)
are called with the actual data.

The consequence of this strategy is that any data required to get the
parameter values must be available when the model is fit. The two main
ways that this can fail is if:

1.  The data have been modified between the creation of the model
    specification and when the model fit function is invoked.

2.  If the model specification is saved and loaded into a new session
    where those same data objects do not exist.

The best way to avoid these issues is to not reference any data objects
in the global environment but to use data descriptors such as
[`.cols()`](https://parsnip.tidymodels.org/dev/reference/descriptors.md).
Another way of writing the previous specification is

     rand_forest(mtry = .cols() - 1)

This is not dependent on any specific data object and is evaluated
immediately before the model fitting process begins.

One less advantageous approach to solving this issue is to use
quasiquotation. This would insert the actual R object into the model
specification and might be the best idea when the data object is small.
For example, using

     rand_forest(mtry = ncol(!!mtcars) - 1)

would work (and be reproducible between sessions) but embeds the entire
mtcars data set into the `mtry` expression:

     > rand_forest(mtry = ncol(!!mtcars) - 1)
     Random Forest Model Specification (unknown)

     Main Arguments:
       mtry = ncol(structure(list(Sepal.Length = c(5.1, 4.9, 4.7, 4.6, 5, <snip>

However, if there were an object with the number of columns in it, this
wouldn't be too bad:

     > mtry_val <- ncol(mtcars) - 1
     > mtry_val
     [1] 10
     > rand_forest(mtry = !!mtry_val)
     Random Forest Model Specification (unknown)

     Main Arguments:
       mtry = 10

More information on quosures and quasiquotation can be found at
<https://adv-r.hadley.nz/quasiquotation.html>.

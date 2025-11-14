# Save information about models

This function writes a tab delimited file to the package to capture
information about the known models. This information includes packages
in the tidymodels GitHub repository as well as packages that are known
to work well with tidymodels packages (e.g. not only parsnip but also
tune, etc.). There may be more model definitions in other extension
packages that are not included here.

These data are used to document engines for each model function man
page.

## Usage

``` r
update_model_info_file(path = "inst/models.tsv")
```

## Arguments

- path:

  A character string for the location of the tab delimited file.

## Details

See our [model implementation
guidelines](https://tidymodels.github.io/model-implementation-principles/)
on best practices for modeling and modeling packages.

It is highly recommended that the known parsnip extension packages are
loaded. The unexported parsnip function `extensions()` will list these.

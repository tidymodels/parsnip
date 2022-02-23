# About the parsnip documentation system

## Background

parsnip uses three concepts to describe models: 

 - The _model type_ specifies its mathematical structure.
    - Example: `linear_reg()` is for models that predict a numeric outcome using a linear combination of predictors and coefficients. 

 - The _model mode_ reflects the type of prediction outcome. 
    - Values: `"classification"`, `"regression"`, and `"censored regression"`.  

 - The _model engine_ is a designation of _how_ the model should be fit. 
    - This is often an R package or function name (e.g. `"ranger"` for the ranger package). 

There are parsnip extension packages that use the parsnip model functions to define _new engines_. For example, the poissonreg package has engines for the `poission_reg()` function. 

There are many combinations of type/engine/mode available in parsnip. We keep track of these values for packages that have their model definitions in parsnip and fully adhere to the tidymodels APIs.  A tab-delimited file with these values is in the package (called `models.tsv`). 

## Main function and engine documentation

Each modeling function defined in parsnip has a documentation file (with extension `.Rd`). 

Also, each combination of engine and model type has a corresponding `.Rd` file (a.k.a the "engine-specific" documentation files). The list of known engines is also shown in the `.Rd` file for the main function. 

If you are creating a new engine documentation file, the steps are:

1. Use roxygen comments to generate the main `.Rd` file that R's help system uses.
2. Create an `.Rmd` file that has the details of the engine. 
3. Knit the `.Rmd` file to `.md` so the `.Rd` file from step 1 can link it. 
4. Once you are done, run `devtools::document()` (or use the RStudio IDE) to build the help files. 

**Before you read the rest**, you probably don't have to generate the help files in this way yourself: 

* This process only applies for engines whose model function is in parsnip. If your R package defines a new model, then you don't have to do all of this (unless you want to). 

* If you make a PR with a new engine, you can work on the `.Rmd` file and the tidymodels folks can go through the process of merging it in (but let us know if you want us to do that in the PR). 

## Creating the engine-specific `.Rd` files

Once create by `document()`, the `.Rd` files live in the `man` directory. They are not handwritten but are automatically generated from `.R` files in the `R` directory. 

How do we generate these `.Rd` files? We'll use an example with `poisson_reg()` and the `"zeroinfl"` engine. 

Each model/engine combination has its own `.R` file with a naming convention reflecting the contents (`poisson_reg_zeroinfl.R`). This file has a description of the type of model and the underlying function that is used for that engine: 

> `[pscl::zeroinfl()]` uses maximum likelihood estimation to fit a model for count data that has separate model terms for predicting the counts and for predicting the probability of a zero count.

Next comes a roxygen comment including a specific _markdown_ file (notice we use `@includeRmd` but we actually include markdown): 

> `@includeRmd man/rmd/poisson_reg_zeroinfl.md details`
 
as well as a directive for the `.Rd` file name to be created: 

> `@name details_poisson_reg_zeroinfl` 
 
The engine markdown file (`poisson_reg_zeroinfl.md`) is made by the developer offline.

Take a look at the [actual file](https://github.com/tidymodels/parsnip/blob/main/R/poisson_reg_zeroinfl.R) if you want to see it all. 

To summarize: the relationship between these `.R` files and the `.md` files that they include is:

```
:
├── R
│   ├── :
│   ├── :
│   ├── bag_tree.R.            <- model definition code that has the Sexprs
│   ├── bag_tree_C5.0.R        <- wrapper around bag_tree_C5.0.md  (below)
│   ├── bag_tree_rpart.R       <- wrapper around bag_tree_rpart.md (below)
:   :
:   :
├── man
│   ├── rmd
│   │   ├──:
│   │   ├── :
│   │   ├── aaa.Rmd            <- sourced by all other Rmd files
│   │   ├── :
│   │   ├── :
│   │   ├── bag_tree_C5.0.Rmd  <- engine details for the C5.0 engine only
│   │   ├── bag_tree_C5.0.md
│   │   ├── bag_tree_rpart.Rmd
│   │   ├── bag_tree_rpart.md
:   :   :
```


## Creating the engine-specific `.md` files for `.Rmd` files

How do we make these markdown files? These are created by corresponding `.Rmd` files contained in `parsnip/man/rmd/`. There are `.Rmd` files for the engines defined in parsnip as well as the extension packages listed by `parsnip:::extensions()`.

Each `.Rmd` file uses `parsnip/man/rmd/aaa.Rmd` as a child document. This file defines helper functions for the engine-specific documentation and loads some specific packages. 

The `.Rmd` files use packages that are not formally parsnip dependencies (these are listed in `aaa.Rmd`). It also requires the parsnip extension packages defined in `parsnip:::extensions()`. 

There is an internal helper function (`parsnip:::install_engine_packages()`) that will install the extension packages as well as a few others that are required to build all of the documentation files. 

The `.Rmd` files have a consistent structure and there are [numerous examples](https://github.com/tidymodels/parsnip/tree/main/man/rmd) of these files in the package. The main sections are: 

 - The list of possible engines.
 - The list of tuning parameters, if any, and other arguments of interest. 
 - Details about how parsnip translates the function call to the call for the underlying model function. 
 - Other details (e.g. preprocessing requirements, etc.)

To convert the `.Rmd` files to `.md`, use the function `knit_engine_docs()`. After this, use `devtools::document()` to create the engine specific `.Rd` files. 

**Please** look at the output of the knitting; if there are errors, they will show up there. 

To test the results, do a hard restart of the R session (i.e., do not use `load_all()`). 

## The main function `.Rd` files

_This section has information that is probably only needed by the core developers._

Recall that the `.Rd` files are created from the `.R` files. There are some fancy things that we do in the `.R` files to list the engines. For example, `poisson_reg.R` has the line: 

```r
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("poisson_reg")}
```

This finds the relevant engine `.Rd` files and creates the corresponding `.Rd` markup: 

```
There are different ways to fit this model, and the method of estimation is  
chosen by setting the model \emph{engine}. The engine-specific pages  for this 
model are listed  below.


\itemize{
  \item \code{\link[parsnip:details_poisson_reg_glm]{glm}¹²}
  \item \code{\link[parsnip:details_poisson_reg_gee]{gee}²}
  \item \code{\link[parsnip:details_poisson_reg_glmer]{glmer}²}
  \item \code{\link[parsnip:details_poisson_reg_glmnet]{glmnet}²}
  \item \code{\link[parsnip:details_poisson_reg_hurdle]{hurdle}²}
  \item \code{\link[parsnip:details_poisson_reg_stan]{stan}²}
  \item \code{\link[parsnip:details_poisson_reg_stan_glmer]{stan_glmer}²}
  \item \code{\link[parsnip:details_poisson_reg_zeroinfl]{zeroinfl}²}
}

¹ The default engine. ² Requires a parsnip extension package.
```

There is a similar line at the bottom of the files that creates the _See Also_ list:

```r
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("poisson_reg")}
```

## Generating the model flat file

_This is also for core developers_. 

As previously mentioned, the parsnip package contains a file `models.tsv`. To create this file:

1. Load the packages listed in `parsnip:::extensions()`.
2. Run `parsnip::update_model_info_file()`. 

Note that the file should never have fewer lines that the current version. 

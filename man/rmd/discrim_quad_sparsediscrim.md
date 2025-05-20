


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 1 tuning parameter:

- `regularization_method`: Regularization Method (type: character, default: 'diagonal')

The possible values of this parameter, and the functions that they execute, are:

* `"diagonal"`: [sparsediscrim::qda_diag()]
* `"shrink_mean"`: [sparsediscrim::qda_shrink_mean()]
* `"shrink_cov"`: [sparsediscrim::qda_shrink_cov()]

## Translation from parsnip to the original package

The **discrim** extension package is required to fit this model.


``` r
library(discrim)

discrim_quad(regularization_method = character(0)) |> 
  set_engine("sparsediscrim") |> 
  translate()
```

```
## Quadratic Discriminant Model Specification (classification)
## 
## Main Arguments:
##   regularization_method = character(0)
## 
## Computational engine: sparsediscrim 
## 
## Model fit template:
## discrim::fit_regularized_quad(x = missing_arg(), y = missing_arg(), 
##     method = character(0))
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Variance calculations are used in these computations within each outcome class. For this reason,  _zero-variance_ predictors (i.e., with a single unique value) within each class should be eliminated before fitting the model. 



## Case weights


The underlying model implementation does not allow for case weights. 

## References

 - `qda_diag()`: Dudoit, Fridlyand and Speed (2002) Comparison of Discrimination Methods for the Classification of Tumors Using Gene Expression Data, _Journal of the American Statistical Association_, 97:457, 77-87. 
 
 - `qda_shrink_mean()`: Tong, Chen, Zhao, Improved mean estimation and its application to diagonal discriminant analysis, _Bioinformatics_, Volume 28, Issue 4, 15 February 2012, Pages 531-537.
 
 - `qda_shrink_cov()`: Pang, Tong and Zhao (2009), Shrinkage-based Diagonal Discriminant Analysis and Its Applications in High-Dimensional Data. _Biometrics_, 65, 1021-1029.

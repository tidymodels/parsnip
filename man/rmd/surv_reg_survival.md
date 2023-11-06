


For this engine, there is a single mode: regression

## Tuning Parameters



This model has 1 tuning parameters:

- `dist`: Distribution (type: character, default: 'weibull')

## Translation from parsnip to the original package


```r
surv_reg(dist = character(1)) %>% 
  set_engine("survival") %>% 
  set_mode("regression") %>% 
  translate()
```

```
## Parametric Survival Regression Model Specification (regression)
## 
## Main Arguments:
##   dist = character(1)
## 
## Computational engine: survival 
## 
## Model fit template:
## survival::survreg(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), dist = character(1), model = TRUE)
```

## Other details

Note that `model = TRUE` is needed to produce quantile predictions when there is a stratification variable and can be overridden in other cases.

The main interface for this model uses the formula method since the model specification typically involved the use of [survival::Surv()]. 

The model formula can include _special_ terms, such as [survival::strata()]. The allows the model scale parameter to differ between groups contained in the function. The column used inside `strata()` is treated as qualitative no matter its type. To learn more about using special terms in formulas with tidymodels, see [`?model_formula`][parsnip::model_formula].

For example, in this model, the numeric column `rx` is used to estimate two different scale parameters for each value of the column:


```r
library(survival)

surv_reg() %>% 
  fit(Surv(futime, fustat) ~ age + strata(rx), data = ovarian) %>% 
  extract_fit_engine()
```

```
## Call:
## survival::survreg(formula = Surv(futime, fustat) ~ age + strata(rx), 
##     data = data, model = TRUE)
## 
## Coefficients:
## (Intercept)         age 
##  12.8734120  -0.1033569 
## 
## Scale:
##      rx=1      rx=2 
## 0.7695509 0.4703602 
## 
## Loglik(model)= -89.4   Loglik(intercept only)= -97.1
## 	Chisq= 15.36 on 1 degrees of freedom, p= 8.88e-05 
## n= 26
```

## References

-  Kalbfleisch, J. D. and Prentice, R. L. 2002 _The statistical analysis of failure time data_, Wiley.



By default, `model.matrix()` generates binary indicator variables for factor predictors. When the formula does not remove an intercept, an incomplete set of indicators are created; no indicator is made for the first level of the factor.

For example, `species` and `island` both have three levels but `model.matrix()` creates two indicator variables for each:


``` r
library(dplyr)
library(modeldata)
data(penguins)

levels(penguins$species)
```

```
## [1] "Adelie"    "Chinstrap" "Gentoo"
```

``` r
levels(penguins$island)
```

```
## [1] "Biscoe"    "Dream"     "Torgersen"
```

``` r
model.matrix(~ species + island, data = penguins) %>% 
  colnames()
```

```
## [1] "(Intercept)"      "speciesChinstrap" "speciesGentoo"    "islandDream"     
## [5] "islandTorgersen"
```

For a formula with no intercept, the first factor is expanded to indicators for _all_ factor levels but all other factors are expanded to all but one (as above):


``` r
model.matrix(~ 0 + species + island, data = penguins) %>% 
  colnames()
```

```
## [1] "speciesAdelie"    "speciesChinstrap" "speciesGentoo"    "islandDream"     
## [5] "islandTorgersen"
```

For inference, this hybrid encoding can be problematic. 

To generate all indicators, use this contrast: 


``` r
# Switch out the contrast method
old_contr <- options("contrasts")$contrasts
new_contr <- old_contr
new_contr["unordered"] <- "contr_one_hot"
options(contrasts = new_contr)

model.matrix(~ species + island, data = penguins) %>% 
  colnames()
```

```
## [1] "(Intercept)"      "speciesAdelie"    "speciesChinstrap" "speciesGentoo"   
## [5] "islandBiscoe"     "islandDream"      "islandTorgersen"
```

``` r
options(contrasts = old_contr)
```

Removing the intercept here does not affect the factor encodings. 

 

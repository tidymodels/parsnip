#' We'll model the relationship between the cost of a house in Sacramento CA and the square footage of a property.

#' A few rows were randomly held out for illustrating prediction.

#+ results = "hide", messages = FALSE
library(tidymodels)
tidymodels_prefer()

sac_holdout <- c(90L, 203L, 264L, 733L, 771L)
sac_train <- Sacramento[-sac_holdout, ]
sac_test  <- Sacramento[ sac_holdout, ]

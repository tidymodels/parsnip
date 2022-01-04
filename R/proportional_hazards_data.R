
# parsnip just contains the model specification, the engines are the censored package.

set_new_model("proportional_hazards")
set_model_mode("proportional_hazards", "censored regression")

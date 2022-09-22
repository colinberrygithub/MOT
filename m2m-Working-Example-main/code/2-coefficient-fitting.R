source("libraries.R")

# Load rds ----------------------------------------------------------------

load(file = './intermediate_steps/1-data-processing/output.rds')

save_dir <- './intermediate_steps/2-coefficient-fitting/'


# About -------------------------------------------------------------------

#' In this section we'll take the our nicely formatted data and try and 
#' calculate the coefficients that most closely match what's in your decomp
#' Note we'll park nesting for now. First we need to fit the coefficients for the unnested models
#' So that when we nest the nested contributions are in the right ballpark
#' If we nested first and the calculate the coefficients it won't work well if say a coefficient is missing from one of the mapping tables


# Fit the coefficients to the models --------------------------------------

model_params <- calibrateResponseCurves(
  models = all_model_decomps, 
  media = media, 
  parameters = all_media_parameters, 
  model_parameters = all_model_parameters, 
  calibration_period = calibration_period
)

#' use our plotting function to see if the curve fitting has worked
#' a good fit is one where our fitted - our attempt at simplifying your model effects down to response_curve * base - matches closely
#' your - modelled - possibly log-linear model decomp. 
#' linear models - commstracker for example - should usually be pretty much exact
#' log-linear models we want them to be close
#' this can also be useful to spot if something has gone wrong - a media variable is wrong for example

#' note it's not necessary to wrap the plotting in map, but this gives us a nice way of interact with all the models
# for example calling decomp_plots$awareness will show us the awareness model
decomp_plots <- map(unique(model_params$parameters$id), function(model){
  
  plotCalibratedCurves(model_params, model_nm = model, facet_by_variable = TRUE)
})
names(decomp_plots) <- unique(model_params$parameters$id)

decomp_plots$awareness
decomp_plots$`_app_downloads`
decomp_plots$`_exp_london`  
decomp_plots$`_exp_south`

#' let's update all_media_parameters and save that
all_media_parameters <- model_params$parameters

# Saving ------------------------------------------------------------------

if(!dir.exists(save_dir)) dir.create(save_dir)
save(all_media_parameters, file = file.path(save_dir, 'output.rds'))

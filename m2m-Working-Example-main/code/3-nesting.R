source("libraries.R")

# Load rds ----------------------------------------------------------------

load(file = './intermediate_steps/1-data-processing/output.rds')
load(file = './intermediate_steps/2-coefficient-fitting/output.rds')

save_dir <- './intermediate_steps/3-nesting/'


# About -------------------------------------------------------------------
#' In this script we take care of nesting
#' The syntax is pretty straight-forward
#' You take a model - *from* - and insert it *into* another model
#' *by* splitting a variable within that second model

#' We support two methods of nesting, 
#' 'linear conversion'
#' by working out the conversion from one model to the other, e.g. a point of BP is worth £XXX 
#' and then multiplying the effects found in the *from* model by that conversion

#' 'pro-rated'
#' working out the % share a variable takes up in your *from* model and taking that share from the *by* variable

#' we default to the 'linear conversion' method

#' Note we call calibrateCurves again once we've taken care of the nesting
#' Not strictly speaking necessary
#' But we can handly re-plot our decomps after nesting


# _exp_london ------

#' Note the change in object from the first call
#' our nesting function is passing back a list, of parameters and decomped - model - effects
#' In the first call we use as inputs decomps - all_model_decomps - and our current parameters - all_model_parameters
#' Both are dataframes
#' The function returns back the effects of that nesting into a list with two dataframes

all_models <- insertNestedModel(
  models = all_model_decomps, 
  parameters = all_media_parameters, 
  model_parameters = all_model_parameters, 
  from = 'awareness', 
  into = '_app_downloads', 
  by = 'vol_bm_ret_bp_movavc')

#' in our second call
#' we pass our updated - from the previous step - decomps all_models$models
#' and our updated parameters all_models$parameters

#' this is important to remember
#' especially as here where we chain nesting 

all_models <- insertNestedModel(
  models = all_models$models, 
  parameters = all_models$parameters, 
  model_parameters = all_model_parameters, 
  from = '_app_downloads', 
  into = '_exp_london', 
  by = 'vol_s_appd_tot_tot_dn')

all_models <- insertNestedModel(
  models = all_models$models, 
  parameters = all_models$parameters, 
  model_parameters = all_model_parameters, 
  from = 'brand_preference', 
  into = '_exp_london', 
  by = 'vol_bm_exp_sa_tot_pt')

# _exp_south ------

all_models <- insertNestedModel(
  models = all_models$models,
  parameters = all_models$parameters,
  model_parameters = all_model_parameters,
  from = '_app_downloads',
  into = '_exp_south',
  by = 'vol_s_appd_tot_tot_dn')

all_models <- insertNestedModel(
  models = all_models$models,
  parameters = all_models$parameters,
  model_parameters = all_model_parameters,
  from = 'brand_preference',
  into = '_exp_south',
  by = 'vol_bm_exp_sa_tot_pt')

# _exp_north ------

all_models <- insertNestedModel(
  models = all_models$models,
  parameters = all_models$parameters,
  model_parameters = all_model_parameters,
  from = '_app_downloads',
  into = '_exp_north',
  by = 'vol_s_appd_tot_tot_dn')

all_models <- insertNestedModel(
  models = all_models$models,
  parameters = all_models$parameters,
  model_parameters = all_model_parameters,
  from = 'brand_preference',
  into = '_exp_north',
  by = 'vol_bm_exp_sa_tot_pt')

# _rtd ------

all_models <- insertNestedModel(
  models = all_models$models,
  parameters = all_models$parameters,
  model_parameters = all_model_parameters,
  from = 'brand_preference',
  into = '_rtd',
  by = 'vol_bm_ret_bp_tot_pt_v2')

# _retail_drink ------

all_models <- insertNestedModel(
  models = all_models$models,
  parameters = all_models$parameters,
  model_parameters = all_model_parameters,
  from = 'brand_preference',
  into = '_retail_drink',
  by = 'vol_bm_ret_bp_tot_pt_v2')

# _retail_other ------

all_models <- insertNestedModel(
  models = all_models$models,
  parameters = all_models$parameters,
  model_parameters = all_model_parameters,
  from = 'brand_preference',
  into = '_retail_other',
  by = 'vol_bm_ret_bp_tot_pt_v2')

# _retail_food ------

all_models <- insertNestedModel(
  models = all_models$models,
  parameters = all_models$parameters,
  model_parameters = all_model_parameters,
  from = 'brand_preference',
  into = '_retail_food',
  by = 'vol_bm_ret_bp_tot_pt_v2_5lag')


#' Now that nesting is done we calibrate the response curves
#' Note that there will be warnings, but this is fine as they say there are no coefficents to estimate for some media variables
model_params <- calibrateResponseCurves(
  models = all_models$models, 
  media = media, 
  parameters = all_models$parameters, 
  model_parameters = all_model_parameters, 
  calibration_period = calibration_period
)

#' Re-plot and view in the same way as before

decomp_plots <- map(unique(model_params$parameters$id), function(model){
  plotCalibratedCurves(model_params, model_nm = model, facet_by_variable = TRUE)
})
names(decomp_plots) <- unique(model_params$parameters$id)


#' let's update the dataframes and save those
all_media_parameters <- model_params$parameters
all_model_decomps <- all_models$models
all_model_parameters <- all_model_parameters 

# Saving ------------------------------------------------------------------

if(!dir.exists(save_dir)) dir.create(save_dir)
save(all_model_decomps, all_media_parameters, all_model_parameters, file = file.path(save_dir, 'output.rds'))

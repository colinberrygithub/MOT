source("libraries.R")

# About -------------------------------------------------------------------

#' In this script we'll pull together all of the data from our models needed to
#' begin processing the results

commstracker_decomps_loc <- './raw_data/commstracker_decomps/'
commstracker_mapping_loc <- './raw_data/commstracker_mapping/'
model_decomps_loc <- './raw_data/model_decomps/'
model_mapping_loc <- './raw_data/model_mapping/'
media_loc <- './raw_data/media/'

#' we'll save our working for this script in this location
save_dir <- './intermediate_steps/1-data-processing/'

#' We want to end up with weekly MOT, so we need 52 weeks to take the results from our models
#' We'll re-use this a few times, but you'll see later we use other time periods
#' Think of this as we want to get to an MOT that is representative of our most recent results
#' So typically we'll use the most recent year

calibration_period <- c(start = "2020-01-03", end = "2020-12-25")

# Format Data -------------------------------------------------------------

#' Load & clean media
raw_media <- readCsvsFromFolder(media_loc)
media <- cleanInput(raw_media)

# Load & clean decomps
raw_models <- readCsvsFromFolder(model_decomps_loc)
decomps <- cleanInput(raw_models)

#' Load & clean decomp parameters
raw_maps <- readCsvsFromFolder(model_mapping_loc)
parameters <- cleanParameters(raw_maps)
media_parameters <- parameters$media_parameters
model_parameters <- parameters$model_parameters

#' Load & clean commstrackers
raw_coms <- readCsvsFromFolder(commstracker_decomps_loc)
commstrackers <- cleanInput(raw_coms)

#' Load & clean commstrackers parameters
raw_commstracker_maps <- readCsvsFromFolder(commstracker_mapping_loc)
commstracker_parameters <- cleanCommstrackerParameters(raw_commstracker_maps)
commstracker_media_parameters <- commstracker_parameters$media_parameters
commstracker_model_parameters <- commstracker_parameters$model_parameters

# these variable names are wrong, so we fix them now
media_parameters <- media_parameters %>%
  mutate(input_nm = case_when(input_nm == "m_tot_exp2020_sp" ~ "m_tot_exp20_sp", TRUE ~ input_nm)) %>% 
  mutate(input_spend_nm = case_when(input_spend_nm  == "m_tot_exp2020_sp" ~ "m_tot_exp20_sp", TRUE ~ input_spend_nm))

# there's a dummy for free coffee in 2019
# because of covid we'll use the base from 2019 so let's remove the dummy now

free_coffee_variables <- c('vol_d_free_24_hour', 'vol_d_270919')
decomps %>% 
  filter(variable %in% free_coffee_variables) %>% 
  group_by(date, id) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(date, value)) + geom_line() + facet_wrap(~id, scales = 'free')

decomps <- decomps %>% 
  filter(!(variable %in% free_coffee_variables))

#' bind our commstracker and eviews models together
all_model_decomps <- bind_rows(decomps, commstrackers)
all_media_parameters <- bind_rows(media_parameters, commstracker_media_parameters)
all_model_parameters <- bind_rows(model_parameters, commstracker_model_parameters)

# we can remove these variables - they get put in the base
# they won't make it into MOT and they are missing from media so we'll do this now
all_media_parameters <- all_media_parameters %>% 
  filter(!(input_nm %in% c('m_eml_foou_vol', 'm_eml_delroll_vol', 'm_eml_deliv_vol')))

# In most cases, the input used in the model is spend, so we can do a batch assignment and then fix the two exceptions
all_media_parameters <- all_media_parameters %>%
  mutate(input_spend_nm = input_nm) %>% 
  mutate(input_spend_nm = case_when(input_spend_nm  == "m_tvvspons_ret_sec" ~ "m_tvvspons_ret_sp", TRUE ~ input_spend_nm)) %>% 
  mutate(input_spend_nm = case_when(input_spend_nm  == "m_tvvspons_ret_ppval" ~ "m_tvvspons_ret_sp", TRUE ~ input_spend_nm))

#' check that the data is valid and there are no missing variables
#' if there are then a warning message will pre printed in your console
validateInputs(media = media,
               models = all_model_decomps,
               parameters = all_media_parameters)

# Saving ------------------------------------------------------------------

if(!dir.exists(save_dir)) dir.create(save_dir)
save(all_media_parameters, all_model_parameters, all_model_decomps, media, calibration_period, file = file.path(save_dir, 'output.rds'))

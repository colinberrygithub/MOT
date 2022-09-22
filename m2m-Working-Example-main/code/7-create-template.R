source("libraries.R")

# Load rds ----------------------------------------------------------------

load(file = './intermediate_steps/1-data-processing/output.rds')
load(file = './intermediate_steps/2-coefficient-fitting/output.rds')
load(file = './intermediate_steps/3-nesting/output.rds')
load(file = './intermediate_steps/4-curve-splitting/output.rds')
load(file = './intermediate_steps/5-curve-combining/output.rds')
load(file = './intermediate_steps/6-curve-inferring/output.rds')


# where our blank template creator is saved
metadata_loc <- './MOT_Templates/metadata_info.xlsx'

save_dir <- './intermediate_steps/7-create-template/'


# About -------------------------------------------------------------------
#' In this script we'll create the template to go into the MOT
#' We'll give things client friendly names
#' And take all our previous calculations and make sure they get where they need to be

# Create and Populate MOT Templates -----------------------------------------

#' First we need to create our template
#' this function will take our parameters table and give us a skeleton to fill out
#' So once filled out we don't want to then overwrite that so we comment it out here
#' You can always re-run it if needs be by highlighting the part of the line - excluding the # symbol and then pressing ctrl-return
#' updateMetadataFile(metadata_loc, all_media_parameters, calibration_period)

#' this pulls in the mapping tables we need to align everything given how you've filled out the metadatafile saved in here metadata_loc
MOT_templates <- getMotTemplates(filepath = metadata_loc)
metadata_mapping <- getMetadataMapping(filepath = metadata_loc)

#' Populate last_media_plan
#' there will be a warning message which will tell you which model is being used to filter the media
MOT_templates$last_media_plan <- populateMediaPlan(media = media, 
                                                    calibration_period = calibration_period,
                                                    parameters = all_media_parameters,
                                                    mot_template = MOT_templates,
                                                    metadata_mapping = metadata_mapping,
                                                    model_to_use = NULL,
                                                    media_plan_phase = "last")

#' Another method is to populate via a list
#' this is useful if you want to pull the media from different models
#' we'll call the function summarise_media() to see which media_channels each model contains

summarise_media(parameters = all_media_parameters,
                media = media,
                calibration_period = calibration_period) %>% 
  View()

#' the primary model is the model from which the majority of our media_channels will be populated from
#' for the remainder channels you state the model thet are coming from
#' for example if there is no single model which contains every single media_channel
#' or if you want to pull the media spend for that channel from a different model to the primary one
populateMediaPlan(media = media,
                  calibration_period = calibration_period,
                  parameters = all_media_parameters,
                  mot_template = MOT_templates,
                  metadata_mapping = metadata_mapping,
                  model_to_use = list("primary" = "Costa-App_Total_LongTerm", "Costa-Express_Total_LongTerm" = "Retail_VOD"),
                  media_plan_phase = "last")

# we want to make sure the rois for media that were missing in 2020 are also correctly set
# so pull the spend for then for that activity
# we will revert this later
missing_media <- populateMediaPlan(media = media, 
                                       calibration_period = c(start = "2019-01-04", end = "2019-12-27"),
                                       parameters = all_media_parameters,
                                       mot_template = MOT_templates,
                                       metadata_mapping = metadata_mapping,
                                       model_to_use = NULL,
                                       media_plan_phase = "last")

missing_media <- missing_media %>% 
  filter(campaign %in% c('Express', 'RTD Media'), media_channel %in% c('OOH', 'Press')) %>% 
  rename(new_value = value)

MOT_templates$last_media_plan <- MOT_templates$last_media_plan %>% 
  left_join(., missing_media, by = c("client", "build", "country", "campaign", "media_channel", "message", "cost_per", "period", "sub_period")) %>% 
  mutate(value = case_when(!is.na(new_value) ~ new_value, TRUE ~ value)) %>% 
  select(-new_value)

#' In most cases current_media_plan is the same as last_media_plan
#' Also need to change the period back to 1 for current_media_plan
MOT_templates$current_media_plan <- MOT_templates$last_media_plan
MOT_templates$current_media_plan$period <- as.numeric(1)

#' However in the case that it is not, you can use the function populateMediaPlan and set media_plan_phase = "current"

#' For now we'll set phasings to always on
# But turn FOOU off as requested
MOT_templates$channel_phasings <- MOT_templates$channel_phasings %>% 
  mutate(value = case_when(campaign == 'FOOU' ~ 0, TRUE ~ value))

#' Populate base_coefs and base_table
# a bit clunky but because of Covid, we'll use 2019 to capture the base
# except for RTD which doesn't go back that far
MOT_templates_excl_rtd <- populateBase(models = all_model_decomps, 
                                       parameters = all_media_parameters,
                                       calibration_period = c(start = "2019-01-04", end = "2019-12-27"), 
                                       metadata_mapping = metadata_mapping, 
                                       mot_template = MOT_templates)

MOT_templates_rtd <- populateBase(models = all_model_decomps, 
                                  parameters = all_media_parameters,
                                  calibration_period = calibration_period,
                                  metadata_mapping = metadata_mapping, 
                                  mot_template = MOT_templates)

MOT_templates$base_table <- MOT_templates_excl_rtd$base_table %>% 
  filter(base_element != 'Base RTD') %>% 
  bind_rows(MOT_templates_rtd$base_table %>% filter(base_element == 'Base RTD'))

MOT_templates$base_coefs <- MOT_templates_excl_rtd$base_coefs %>% 
  filter(base_element != 'Base RTD') %>% 
  bind_rows(MOT_templates_rtd$base_coefs %>% filter(base_element == 'Base RTD'))

MOT_templates$base_coefs$value <- ifelse(is.na(MOT_templates$base_coefs$value), 1, MOT_templates$base_coefs$value)
MOT_templates$base_table$value <- ifelse(is.na(MOT_templates$base_table$value), 1, MOT_templates$base_table$value)


#' Populate spend_thresholds
MOT_templates$spend_thresholds <- populateSpendThresholds(mot_template = MOT_templates,
                                                          max_multiplier = 1.5,
                                                          min_multiplier = 0,
                                                          use_max_min = TRUE,
                                                          intersect_phasing = TRUE)
#' Populate parameters
MOT_templates$parameters <- populateParameters(parameters = all_media_parameters, 
                                               metadata_mapping = metadata_mapping, 
                                               mot_template = MOT_templates)

# Saving ------------------------------------------------------------------

if(!dir.exists(save_dir)) dir.create(save_dir)
save(MOT_templates, metadata_mapping, file = file.path(save_dir, 'output.rds'))
writeCsvs(MOT_templates, file.path(save_dir, 'template'))

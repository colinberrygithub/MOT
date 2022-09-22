source("libraries.R")

# Load rds ----------------------------------------------------------------

load(file = './intermediate_steps/1-data-processing/output.rds')
load(file = './intermediate_steps/2-coefficient-fitting/output.rds')
load(file = './intermediate_steps/3-nesting/output.rds')
load(file = './intermediate_steps/4-curve-splitting/output.rds')
load(file = './intermediate_steps/5-curve-combining/output.rds')
load(file = './intermediate_steps/6-curve-inferring/output.rds')
load(file = './intermediate_steps/7-create-template/output.rds')

save_dir <- './intermediate_steps/8-roi-checking/'


# About -------------------------------------------------------------------

#' Now we check to see if our coefficients tally up to the reported ROI
#' And make any final adjustments
#' I need to create an upload from the previous step
#' upload into the tool
#' Goto Visualise > Current plan
#' Filter the data to be campaign, media channel, kpi group, product and sub-product
#' download that file and save it somewhere, for us it's this file './Current Plan - Project 1 - Core.csv'
#' IF I have to re-do some earlier steps I have to re-generate that file
#' It contains the quantificaitions that are implied by the previous unadjusted step
#' We've said for a while we'll finally wrap-up the coefficients
#' Now... we do it


# Final adjustments to coefficients ---------------------------------------

#' compare the ROIs implied by all our work to do it
current_rois <- read.csv(file = './current_plan/Current Plan - Project 1 - Core.csv', stringsAsFactors = FALSE)

returnSubSection <- function(mot_out, string){
  x <- mot_out$Spend
  
  appears <- stringr::str_detect(x, pattern = string)
  if(any(appears)){
    idx <- which(appears == TRUE)
    spaces_ <- stringr::str_detect(x[idx:length(x)], pattern = '')
    
    # why does it return the statement negated ?!?
    spaces_ <- !spaces_
    if(!any(spaces_)){
      end <- length(x)
    } else{
      end <- idx + min(which(spaces_))-2
    }
  } else{
    stop('string not found')
  }
  
  points <- c('start'= idx+2, 'end' = end)
  
  data_ <- mot_out[points[['start']]:points[['end']], ]
  
  return(data_)
  
}

metrics <- c(
  "All Revenue - ROI"
)
out_tidy <- map(metrics, function(x){
  
  returnSubSection(current_rois, x)
  
})

names(out_tidy) <- metrics


out_tidy <- bind_rows(out_tidy, .id ='metric_id') %>% 
  set_names(c('metric_id', 'campaign', 'media_channel', 'group', 'product', 'sub_product', 'out')) %>% 
  mutate(campaign = stringr::str_remove_all(campaign, '[ -]'),
         media_channel = stringr::str_remove_all(media_channel, '[ -]'),
         group = stringr::str_remove_all(group, '[ -]'),
         product = stringr::str_remove_all(product, '[ -]'),
         sub_product = stringr::str_remove_all(sub_product, '[ -]'))


# with a nicely formatted template of our actuals provided by Nick
actual_rois <- read.csv(file = './current_plan/Current Plan - Actual.csv', stringsAsFactors = FALSE)

actual_tidy <- map(metrics, function(x){
  
  returnSubSection(actual_rois, x)
  
})

names(actual_tidy) <- metrics


actual_tidy <- bind_rows(actual_tidy, .id ='metric_id') %>% 
  set_names(c('metric_id', 'campaign', 'media_channel', 'group', 'product', 'sub_product', 'out', 'actual')) %>% 
  select(-out)

#' work out the adjustments necessary 
#' note because we only check ROIs the adjustment is increasing the coefficient in proportion to actual/out
#' if it was also on CPA's we'd need an identifier to invert that adjustment
adjustments <- left_join(out_tidy, actual_tidy, by = c("metric_id", "campaign", "media_channel", "group", "product", "sub_product")) %>% 
  mutate(actual = as.numeric(actual)) %>% 
  mutate(adjustment = case_when(out != 0 ~ actual/out, TRUE ~ 0)) %>% 
  select(campaign, media_channel, group, product, sub_product, adjustment) %>% 
  rename(campaign_ns = campaign, media_channel_ns = media_channel, group_ns = group, product_ns = product, sub_product_ns = sub_product) %>% 
  mutate(parameter = 'coefficient')


cols_ <- colnames(MOT_templates$parameters)

MOT_templates$parameters %>% 
  nrow()

insertion <- MOT_templates$parameters %>% 
  mutate(
    campaign_ns = stringr::str_remove_all(campaign, '[ -]'),
    media_channel_ns = str_remove_all(media_channel, pattern = '[ -]'),
    group_ns = str_remove_all(group, pattern = '[ -]'),
    product_ns = str_remove_all(product, pattern = '[ -]'),
    sub_product_ns = str_remove_all(sub_product, pattern = '[ -]')
  ) %>% 
  left_join(adjustments, by = c("parameter", "campaign_ns", "media_channel_ns", "group_ns", "product_ns", "sub_product_ns")) %>% 
  mutate(value = case_when(!is.na(adjustment) ~ value * adjustment, TRUE ~ value)) %>% 
  select(cols_)

nrow(insertion)

anti_join(insertion, MOT_templates$parameters) %>% 
  filter(parameter !='coefficient') %>% 
  nrow()


MOT_templates$parameters <- insertion

if(!dir.exists(save_dir)) dir.create(save_dir)
writeCsvs(MOT_templates, file.path(save_dir, '/template_after_calibration/'))
save(MOT_templates, file = file.path(save_dir, 'output.rds'))


# Put back media to actual ------------------------------------------------
#' in checking the ROIs we pulled media from 2019 for a few strands
#' now we put that change back
MOT_templates$last_media_plan <- populateMediaPlan(media = media, 
                                                   calibration_period = calibration_period,
                                                   parameters = all_media_parameters,
                                                   mot_template = MOT_templates,
                                                   metadata_mapping = metadata_mapping,
                                                   model_to_use = NULL,
                                                   media_plan_phase = "last")

MOT_templates$current_media_plan <- MOT_templates$last_media_plan
MOT_templates$current_media_plan$period <- as.numeric(1)

# there's an outlier in the base app variable in this week
# so lets smooth it
MOT_templates$base_table <- MOT_templates$base_table %>% 
  mutate(value = case_when(base_element == "Base App" & sub_period == 39 ~ mean(c(0.9739408,1.2418861)), TRUE ~ value))

# Saving ------------------------------------------------------------------

writeCsvs(MOT_templates, file.path(save_dir, '/template_final/'))

save(MOT_templates, file = file.path(save_dir, 'output_final.rds'))

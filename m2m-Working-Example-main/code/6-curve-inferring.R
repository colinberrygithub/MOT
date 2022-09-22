source("libraries.R")

# Load rds ----------------------------------------------------------------

load(file = './intermediate_steps/1-data-processing/output.rds')
load(file = './intermediate_steps/2-coefficient-fitting/output.rds')
load(file = './intermediate_steps/3-nesting/output.rds')
load(file = './intermediate_steps/4-curve-splitting/output.rds')
load(file = './intermediate_steps/5-curve-combining/output.rds')

save_dir <- './intermediate_steps/6-curve-inferring/'

# About -------------------------------------------------------------------

#' In this script we want to deal with the fact that we don't fit diminishing returns in commstracker
#' we want to find some reference curve that we can mimic or infer from. Because having linear curves will give us skewy optimisations
#' So we first say, let's go model by model and take the divisor found in the short-term for a given variable
#' and find the long-term divisor that best fits that same shape of diminishing returns
#' Note it's important here that our function is flexible, we first use it to match variables within a model
#' It warns us that within a given model not all agg_variables have a short-term effect to infer from
#' so later we have to individually identify which models to take from


# Inferring the long-term curves ------------------------------------------

#' first the easy bit
#' let's identify where our gaps are
#' a gap is any agg_variable, that has a long-term effect but no short-term effect in a given model

inference_mapping <- all_media_parameters %>% 
  filter(coefficient != 0) %>% 
  select(agg_id, agg_variable) %>%
  unique() %>%
  mutate(value = 1) %>%
  spread(agg_id, value, fill = 0)

#' we can see from this table some agg_variables have a long-term effect - indicated by a 1 - and some have no short-term effect indicated by a zero
#' so we know there will be gaps we need to fill in
inference_mapping %>% View()

#' first we do the easy ones, infer any curves where a long and short-term exist
inferred_curves <- inferDiminishingReturns(all_media_parameters, media, from_model = 'Costa-App_Total_ShortTerm', to_model = 'Costa-App_Total_LongTerm')
inferred_curves <- inferDiminishingReturns(inferred_curves$parameters, media, from_model = 'Costa-Express_Total_ShortTerm', to_model = 'Costa-Express_Total_LongTerm')
inferred_curves <- inferDiminishingReturns(inferred_curves$parameters, media, from_model = 'Costa-Retail_Drinks_ShortTerm', to_model = 'Costa-Retail_Drinks_LongTerm')
inferred_curves <- inferDiminishingReturns(inferred_curves$parameters, media, from_model = 'Costa-Retail_Food_ShortTerm', to_model = 'Costa-Retail_Food_LongTerm')
inferred_curves <- inferDiminishingReturns(inferred_curves$parameters, media, from_model = 'Costa-Retail_Other_ShortTerm', to_model = 'Costa-Retail_Other_LongTerm')
inferred_curves <- inferDiminishingReturns(inferred_curves$parameters, media, from_model = 'Costa-RTD_Total_ShortTerm', to_model = 'Costa-RTD_Total_LongTerm')

#' now identify where the gaps are
fill_lt_effects <- inference_mapping %>% 
  gather(key = 'agg_id', value = 'modification', -agg_variable) %>% 
  mutate(st_lt = stringr::str_extract(string = agg_id, pattern = '[A-Za-z]+$'), kpi = stringr::str_remove(agg_id, pattern = '[A-Za-z]+$')) %>% 
  select(-agg_id) %>% 
  spread(key = 'st_lt', value = 'modification') %>% 
  filter(LongTerm == 1, ShortTerm == 0)

# we don't want to infer for At Home so we exclude it now
fill_lt_effects <- fill_lt_effects %>% 
  filter(agg_variable != 'At Home_Total')

# using Nick's help we know that we can fill in gaps, by using these models where no short-term exists in a given model
# e.g. if trying to fit a LT divisor for Express in some non-express model, use the Express model which does have a short-term effect we can mimic
fill_lt_effects <- fill_lt_effects %>% 
  mutate(to_ = paste0(kpi, 'LongTerm')) %>% 
  mutate(from_ = case_when(
    grepl('Express', agg_variable) ~ 'Costa-Express_Total_ShortTerm', 
    grepl('Retail', agg_variable) ~ 'Costa-Retail_Drinks_ShortTerm',
    grepl('RTD', agg_variable) ~ 'Costa-RTD_Total_ShortTerm'
  ))


#' do this in a loop
for( row in 1:nrow(fill_lt_effects)){
  print(paste0('from_model: ', fill_lt_effects$from_[row], 
               ' from_variable: ', fill_lt_effects$agg_variable[row], 
               ' to_model: ', fill_lt_effects$to_[row], 
               ' to_variable: ', fill_lt_effects$agg_variable[row]))
  
  inferred_curves <- inferDiminishingReturns(inferred_curves$parameters, media, 
                                             from_model = fill_lt_effects$from_[row], to_model = fill_lt_effects$to_[row],
                                             from_variable = fill_lt_effects$agg_variable[row], to = fill_lt_effects$agg_variable[row])
}

#' and now plot
plotInferredCurves(inferred_curves, all_media_parameters, media, 'Costa-App_Total_LongTerm')
plotInferredCurves(inferred_curves, all_media_parameters, media, 'Costa-Express_Total_LongTerm')
plotInferredCurves(inferred_curves, all_media_parameters, media, 'Costa-Retail_Drinks_LongTerm')
plotInferredCurves(inferred_curves, all_media_parameters, media, 'Costa-Retail_Food_LongTerm')
plotInferredCurves(inferred_curves, all_media_parameters, media, 'Costa-Retail_Other_LongTerm')
plotInferredCurves(inferred_curves, all_media_parameters, media, 'Costa-RTD_Total_LongTerm')

#' let's update all_media_parameters and save that
all_media_parameters <- inferred_curves$parameters

# Saving ------------------------------------------------------------------

if(!dir.exists(save_dir)) dir.create(save_dir)
save(all_media_parameters, file = file.path(save_dir, 'output.rds'))

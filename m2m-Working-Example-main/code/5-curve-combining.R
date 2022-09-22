source("libraries.R")

# Load rds ----------------------------------------------------------------

load(file = './intermediate_steps/1-data-processing/output.rds')
load(file = './intermediate_steps/2-coefficient-fitting/output.rds')
load(file = './intermediate_steps/3-nesting/output.rds')
load(file = './intermediate_steps/4-curve-splitting/output.rds')

save_dir <- './intermediate_steps/5-curve-combining/'


# About -------------------------------------------------------------------

#' In this script we are going to combine our response curves
# For costa this isn't so difficult as generally we don't have that many effects
#' But because of nesting, aggregating the express models and because commstracker variables have two adstocks
#' we still need to do some. I take it back we need to do about as much as usual just for different reasons.

#' We start by taking the previous step media_parameters_split and create a mapping table of how we want to aggregate accross 
#' variables and models - ids - 
#' Once we have that mapping we aggregate the curves to find the best single adstock, coefficient, divisor 
#' that fits the combination of some number of curves

# Create a mapping --------------------------------------------------------

#' Let's take our previous step and create a mapping table to fill out
#' for each combination of model - id - and variable we need to provide the agg_id and agg_variable that we want to combine

# let's select a small example and think it through
# we want to combine all the long-term effects  in the express models into one one long-term curve per variable
# so for m_disst_exp_sp there's upto 12 effects, 4 per model and 3 regional models
# so the relevant section of our completed agg_mapping, might look like this
# here we're saying combine all of these variables into the aggregate variable "Express_Display" in the aggregate model "Costa-Express_Total_LongTerm"
# it doesn't matter what we use as a name in agg_id and agg_variable as long as we're consistent
# if for example I didn't include all of these variables under the same agg_variable I wouldn't get one combined curve, 
# I'd get two or more - dependent on how inconsistent I was

# id          variable                                input_nm       agg_id                       agg_variable   
# <chr>       <chr>                                   <chr>          <chr>                        <chr>          
# 1 _exp_london _app_downloads_awareness_m_disst_exp_sp m_disst_exp_sp Costa-Express_Total_LongTerm Express_Display
# 2 _exp_london brand_preference_m_disst_exp_sp         m_disst_exp_sp Costa-Express_Total_LongTerm Express_Display
# 3 _exp_south  _app_downloads_awareness_m_disst_exp_sp m_disst_exp_sp Costa-Express_Total_LongTerm Express_Display
# 4 _exp_south  brand_preference_m_disst_exp_sp         m_disst_exp_sp Costa-Express_Total_LongTerm Express_Display
# 5 _exp_north  _app_downloads_awareness_m_disst_exp_sp m_disst_exp_sp Costa-Express_Total_LongTerm Express_Display
# 6 _exp_north  brand_preference_m_disst_exp_sp         m_disst_exp_sp Costa-Express_Total_LongTerm Express_Display



#' because we don't want to overwrite a filled out mapping we've included an if statement to stop us overwirting
#' you can always highlight the code inside the if statement
#' or just do as we've done and create a blank version combined_mapping.csv
#' and a filled out version - combined_mapping_complete.csv'

if(!file.exists('combined_mapping.csv')){
  all_media_parameters %>% 
    filter(!(id %in% c('awareness', 'brand_preference'))) %>% 
    select(id, variable, input_nm) %>% 
    unique() %>% 
    mutate(agg_id ='', agg_variable ='') %>% 
    write_csv(x = .,path = 'combined_mapping.csv')
}

#' ok we go away and fill out our mapping
#' being sure that if we changed something higher up that would change the names or number of combos, we'd need to re-do this
#' I personally don't like creating mapping tables in csv
#' It's easy to make a change and then forget to update it

agg_mapping <- read_csv('./combined_mapping_complete.csv')

# in this mapping we don't want to include the two commstrackers and we have excluded some media variables
# as labelled with 'Exclude' so we can easily filter them out
mapped_curves <- all_media_parameters %>% 
  left_join(., agg_mapping, by = c('id', 'variable', 'input_nm')) %>% 
  filter(!(id %in% c('awareness', 'brand_preference'))) %>% 
  filter(agg_variable != 'Exclude')

#' now we combine our curves
combined_curves <- combineResponseCurves(parameters = mapped_curves, media = media)

#' let's be selective about which plots we look at
#' picking the worst in terms of rmse - our measure of how well our combined curve matches the sum of its parts
rmse_ <- summary(combined_curves$parameters$rmse)
rmse_3rdq <- rmse_[['3rd Qu.']]

idx <- which(combined_curves$parameters$rmse > rmse_3rdq)
plots_to_inspect <- combined_curves$parameters[idx, ] %>% 
  select(agg_id, agg_variable, rmse) %>% 
  unique()


combined_plots <- map2(plots_to_inspect$agg_id, plots_to_inspect$agg_variable, function(model, variable){
  plots <- plotCombinedCurves(combined_data = combined_curves, media = media, model_nm = model, variable_nm = variable)
})
names(combined_plots) <- paste0('model: ', plots_to_inspect$agg_id, ' variable: ', plots_to_inspect$agg_variable)

#' Generally pretty good
#' Some long-term ones are not so good. But in reality we're often solving for a single adstock which kind of don't need to be doing.
combined_plots$`model: Costa-Express_Total_LongTerm variable: Retail_TV Sponsorship`
#' even this which has the highest rmse is still really good
combined_plots$`model: Costa-Express_Total_ShortTerm variable: Express_Display`

#' let's update all_media_parameters and save that
all_media_parameters <- combined_curves$parameters

# Saving ------------------------------------------------------------------

if(!dir.exists(save_dir)) dir.create(save_dir)
save(all_media_parameters, file = file.path(save_dir, 'output.rds'))

source("libraries.R")

# Load rds ----------------------------------------------------------------

load(file = './intermediate_steps/1-data-processing/output.rds')
load(file = './intermediate_steps/2-coefficient-fitting/output.rds')
load(file = './intermediate_steps/3-nesting/output.rds')

save_dir <- './intermediate_steps/4-curve-splitting/'

# About -------------------------------------------------------------------

#' In the models we have total spend
#' we want seperate curves in the mot
#' so we need to split the combined curve
#' We'll take each total and find the divisor that best fits each component to give each component the same shape of returns
#' IMPORTANTLY the coefficients will be wrong here. 
#' Because we split, by first assuming that all components start with the same divisor and then modifiying that divisor so it fits that curve
# I did try doing this earlier up and then finding the coefficients that best fit the total found in the decomps. 
# This doesn't work for dis_ret because it's 80% correlated with soc_ret and tvvspons_sp becuase it doesn't vary that much
#' The hope here is that we can combine the curves after this and resolve any differences in the coefficients in the MOT. 
# IT IS possible that in combining we get some funky results


# Convert all divisors to spend -------------------------------------------

#' we do this now because we start to disaggregate curves and then combine, so they need to be in a consistent unit

converted_divisors <- convertDivisors(parameters = all_media_parameters, media = media, calibration_period = NULL)


# Disaggregate curve ------------------------------------------------------

#' To understand what is happening here
#' You need to think about the outcome of the previous step
#' we take converted_divisors we identify from that which variables need to be split
#' we then insert records - copies at this point of the combined curve where the input for each copy is some variable that's a part of the whole
#' we then use our code infer inferDiminishingReturns to find the divisor for each constitutent part of the combined variable
#' keeping the contribution constant
#' we've already covered this won't match what was reported
#' but we know we can tidy that up - much - later on

# Nick kindly provided a mapping of how to split the variables
media_disaggregation <- readr::read_csv(file ='./raw_data/received/mediaMapping.csv')

media_disaggregation <- media_disaggregation %>% 
  select(Spend, Grouped_Var) %>% 
  filter(Grouped_Var != 'Remove') %>% 
  filter(Spend != Grouped_Var) %>% 
  unique() %>% 
  select('input_nm' = 'Grouped_Var', 'agg_variable' = 'Spend')

#' identify which variables need to be split, and join on the splits
media_parameters_aggregated <- converted_divisors %>% 
  filter(input_nm %in% media_disaggregation$input_nm)

media_parameters_disaggregated <- media_parameters_aggregated %>%  
  left_join(., media_disaggregation, by = 'input_nm') %>% 
  mutate(input_nm = agg_variable, input_spend_nm = agg_variable, agg_id = paste0(id, '_', variable))

media_parameters_for_splitting <- media_parameters_aggregated %>% 
  mutate(agg_variable = input_nm, agg_id = paste0(id, '_', variable)) %>% 
  bind_rows(., media_parameters_disaggregated)

#' here we work out the combinations we need to loop through 
#' and pass those combos through map2_df to loop through them
#' rather than type out the exact combos and have 17 calls to inferDiminishingReturns

combos_ <- media_parameters_aggregated %>% 
  mutate(agg_id = paste0(id, '_', variable)) %>% 
  select(agg_id, input_nm) %>% 
  unique()

parameters_split <- map2_df(combos_$agg_id, combos_$input_nm, function(from_model, from_variable){
  
  media_combos <- media_disaggregation %>% 
    filter(input_nm == from_variable)
  
  map_df(media_combos$agg_variable, function(to_variable){
    print(paste0('from_model: ', from_model, ' from_variable: ', from_variable, ' to_model: ', from_model, ' to_variable: ', to_variable))
    curves_ <- inferDiminishingReturns(media_parameters_for_splitting, media = media, from_model = from_model, from_variable = from_variable, to_model = from_model, to_variable = to_variable)
    
    # important to know that inferDiminishingReturns will pass back a list 
    # with parameters
    # parameters will contain the curve we modified AND the ones we didn't
    # so we need to filter to that one modification here and then map2_df will bind that into a single dataframe for us
    curve <- curves_$parameters %>% 
      filter(agg_variable == to_variable, agg_id == from_model)
    
    return(curve)
  })
  
})

parameters_split <- parameters_split %>% 
  select(-inferred_from_id, -inferred_from_variable, -agg_id, -agg_variable)

#' we need to take converted_divisors, remove the aggregate records
#' and put back the disaggregated versions

media_parameters_split <- converted_divisors %>% 
  filter(!(input_nm %in% media_disaggregation$input_nm)) %>% 
  bind_rows(., parameters_split)

#' let's update all_media_parameters and save that
all_media_parameters <- media_parameters_split

# Saving ------------------------------------------------------------------

if(!dir.exists(save_dir)) dir.create(save_dir)
save(all_media_parameters, file = file.path(save_dir, 'output.rds'))

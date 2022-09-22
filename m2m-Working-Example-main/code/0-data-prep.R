source("libraries.R")

#' Take the data in this location and split it into the raw_data directories
received_folder <- './raw_data/received/'

# About -------------------------------------------------------------------

# This script takes the output of the JEET tool and formats the files to be the same as an Eviews output
# this is very specific to the Costa dataset we're using,
# this step may be skipped if your data is already in the correct format. In this script we:
#   tidy up any mistakes
#   convert to csv
#   fill in missing values
#   get to format we want for our code

# Media data --------------------------------------------------------------

file_ <- here::here(received_folder, 'media_data.csv')

media_data <- read_csv(file_)
dupe_cols <- colnames(media_data) %>% 
  stringr::str_detect(., pattern = '_[0-9]$')

media_data <- media_data[ , !dupe_cols]

media_data <- media_data %>% 
  mutate(date = dmy(date))

readr::write_csv(x = media_data, path = './raw_data/media/media.csv')

# Awareness ---------------------------------------------------------------

file_ <- here::here(received_folder, 'awareness decomp.xlsx')
sht_names <- readxl::excel_sheets(file_)
coefficients <- readxl::read_xlsx(file_, sheet = sht_names[1])
coefficients <- coefficients %>% 
  mutate(Input = Campaign) %>% 
  select(Campaign, Input, AI, `Short-term Retention`, `Base Uplift`, `Base Retention`, Lag, Curvature)
readr::write_csv(x = coefficients, path = './raw_data/commstracker_mapping/awareness.csv')


decomp <- readxl::read_xlsx(file_, sheet = sht_names[2])

decomp <- decomp %>% 
  set_names(., str_remove_all(colnames(.), '\\.{3}[0-9]+$')) %>% 
  select(-`Actual Awareness`, -`Model`) %>% 
  mutate(wcSun = ymd(wcSun))

readr::write_csv(x = decomp, path = './raw_data/commstracker_decomps/awareness.csv')

# Brand preference --------------------------------------------------------

file_ <- here::here(received_folder, 'Brand preference decomp.xlsx')
sht_names <- readxl::excel_sheets(file_)
coefficients <- readxl::read_xlsx(file_, sheet = sht_names[1])
coefficients <- coefficients %>% 
  mutate(Input = Campaign) %>% 
  select(Campaign, Input, AI, `Short-term Retention`, `Base Uplift`, `Base Retention`, Lag, Curvature)
readr::write_csv(x = coefficients, path = './raw_data/commstracker_mapping/brand_preference.csv')

decomp <- readxl::read_xlsx(file_, sheet = sht_names[2])

decomp <- decomp %>% 
  select(-`Actual Brand Preference`, -`Model`) %>% 
  mutate(wcSun = ymd(wcSun))

readr::write_csv(x = decomp, path = './raw_data/commstracker_decomps/brand_preference.csv')

# Decomps -----------------------------------------------------------------

file_ <- here::here(received_folder, 'decomp_file.xlsx')
sht_names <- readxl::excel_sheets(file_)
decomp <- readxl::read_xlsx(file_, sheet = sht_names[1])

decomp <- decomp %>% 
  mutate(clean.vars = case_when(is.na(clean.vars) & model == '_exp_london' ~ "competitors",
                                TRUE ~ clean.vars))

# there's a gap in some models due to covid. During this time period, the kpi and the decomp is essentially zero
decomp <- decomp %>% 
  unite(col = 'identifier', c('modelled.vars', 'clean.vars', 'Group', 'Decomp.Group', 'model'), sep = '#') %>% 
  spread(key = 'identifier', 'contributions', fill = 0) %>% 
  gather(key = 'identifier', value = 'contributions', -date) %>% 
  separate(col = 'identifier', into = c('modelled.vars', 'clean.vars', 'Group', 'Decomp.Group', 'model'), sep = '#')

split_decomps <- decomp %>% 
  mutate(date = ymd(date)) %>% 
  mutate(clean.vars = tolower(clean.vars), model = tolower(model)) %>% 
  select(date, clean.vars, model, contributions) %>% 
  mutate(clean.vars = paste0('vol_', clean.vars)) %>% 
  group_by(clean.vars, model, date) %>% 
  summarise(contributions = sum(contributions, na.rm = TRUE)) %>% 
  ungroup() %>% 
  split(., .$model)

# in the "_exp_london model" competitors is unmapped
# Given our usage it's fine to categorise as competitors

tidy_split_decomps <- map(split_decomps, function(model){
  
  out <- model %>% 
    select(-model) %>% 
    spread(key = 'clean.vars', value = 'contributions') %>% 
    arrange(date)
  
  return(out)
})

writeCsvs(tidy_split_decomps, './raw_data/model_decomps/')

# Mapping tables ----------------------------------------------------------

file_ <- here::here(received_folder, 'MediaMap.csv')
mapping <- read_csv(file =file_)

split_mappings <- mapping %>% 
  rename("Carryover" = "adstock_new",
         "CleanVar" = "clean_vars",
         "Transformation Parameter" = "atan_new",
         "Var_Eqn" = "variable_name") %>% 
  mutate(Transformation = case_when(!is.na(`Transformation Parameter`) ~ "ATAN", TRUE ~ "")) %>% 
  mutate(Price = "", Margin = "", `ROI Output` = "", 
         Lag = 0, Coeff = 1, Var = CleanVar, 
         Group = CleanVar, RefPt = NA, SpendVar = CleanVar,
         `Transformation Parameter2` = "", `ST/LT` = "", NestedVariableEquation = "") %>% 
  select(Var_Eqn, Coeff, Var, Group, RefPt, 
         SpendVar, Price, Margin, `ROI Output`, Lag, 
         Transformation, `Transformation Parameter`, Carryover, 
         CleanVar, `Transformation Parameter2`, `ST/LT`, NestedVariableEquation, model
  ) %>% 
  split(., .$model)

tidy_split_mappings <- map(split_mappings, function(model){
  
  out <- model
  # insert the dependant variable, am told they are all logged
  
  kpi_line <- out[1, ]
  model_name <- kpi_line$model
  kpi_line <- kpi_line %>% 
  mutate(Var_Eqn = paste0('log(', model_name, ')'), SpendVar = "", Coeff = NA, 
         Var = model_name, Group = 'KPI', RefPt = NA, SpendVar = "", 
         Price = NA, Margin = NA, `ROI Output` = "", Lag = NA, 
         Transformation = 'LOG',`Transformation Parameter` = NA, 
         Carryover = NA, CleanVar = model_name, 
         `Transformation Parameter2` = "", `ST/LT` = "", NestedVariableEquation = "") 

  out <- bind_rows(kpi_line, out) %>% 
    select(-model)

  return(out)
  
})


# Saving ------------------------------------------------------------------

writeCsvs(tidy_split_mappings, './raw_data/model_mapping/')

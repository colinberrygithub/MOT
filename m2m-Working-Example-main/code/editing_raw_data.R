library(tidyverse)
library(readxl)
library(lubridate)

source('./Code/functions.R')



# Take the data in this location and split it into the raw_data directories
received_folder <- './working/costa/raw_data/received/'

file_ <- here::here(received_folder, 'media_data_orig.csv')
media_data <- read_csv(file_)

## multiply each media val by a random number
# set.seed as we want the same 'random' numbers each time
set.seed(123)
media_data <- media_data %>% 
  mutate_if(., is.numeric, ~ . * runif(1, min = 0.5, max = 1.5))

# write back
readr::write_csv(x = media_data, path = './working/costa/raw_data/received/media_data.csv')

# # old
# 
# file_ <- here::here(received_folder, 'awareness decomp.xlsx')
# sht_names <- readxl::excel_sheets(file_)
# coefficients_aware <- readxl::read_xlsx(file_, sheet = sht_names[1])
# decomp_aware <- readxl::read_xlsx(file_, sheet = sht_names[2])
# 
# file_ <- here::here(received_folder, 'Brand preference decomp.xlsx')
# sht_names <- readxl::excel_sheets(file_)
# coefficients_brand <- readxl::read_xlsx(file_, sheet = sht_names[1])
# decomp_brand <- readxl::read_xlsx(file_, sheet = sht_names[2])
# 
# # remove at home
# media_data <- media_data %>% 
#   select(-contains("home"))
# 
# coefficients_aware <- coefficients_aware %>% 
#   filter(!str_detect(Campaign, 'home'))
# 
# decomp_aware <- decomp_aware %>% 
#   select(-contains("home"))
# 
# coefficients_brand <- coefficients_brand %>% 
#   filter(!str_detect(Campaign, 'home'))
# 
# decomp_brand <- decomp_brand %>% 
#   select(-contains("home"))
# 
# xlsx::write.xlsx(x = as.data.frame(coefficients_aware),
#                  file = './working/costa/raw_data/received_edited/awareness decomp.xlsx',
#                  sheetName = "Coeffiecients",
#                  row.names = FALSE)
# xlsx::write.xlsx(x = as.data.frame(decomp_aware),
#                  file = './working/costa/raw_data/received_edited/awareness decomp.xlsx',
#                  sheetName = "Decomp",
#                  row.names = FALSE,
#                  append = TRUE)
# 
# xlsx::write.xlsx(x = as.data.frame(coefficients_brand),
#                  file = './working/costa/raw_data/received_edited/Brand preference decomp.xlsx',
#                  sheetName = "Coeffiecients",
#                  row.names = FALSE)
# xlsx::write.xlsx(x = as.data.frame(decomp_brand),
#                  file = './working/costa/raw_data/received_edited/Brand preference decomp.xlsx',
#                  sheetName = "Decomp",
#                  row.names = FALSE,
#                  append = TRUE)

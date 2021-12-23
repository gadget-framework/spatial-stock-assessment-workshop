library(mfdb)
library(tidyverse)

## Load data objects for both species
yft_version <- 2
species_codes <- c('YFT', 'TOA')
n_sims <- 100


## Lookup table for time, month is the mid-point of each quarter.. see data$surveytiming
time_df <- tibble(year = 1:256, 
                  nyear = rep(1952:2015, each = 4), 
                  month = rep(c(2, 5, 8, 11), 256/4))

## Gear lookup table
gear_lookup <- 
  tibble(noaa_gear = c('ps', 'trol', 'bb', 'gi', 'll', 'other', 'hd'),
         gear = c('PSE', 'TROL', 'BB', 'GIL', 'LLN', 'OTHER', 'HLN'))


## YFT data was updated, create suffix for version
yft_suffix <- ifelse(yft_version == 2, '_v2', '')

## Load the 100 simulations for the four areas
## YFT
load(file = file.path('data', paste0('YFT_4area_observations_1_100', yft_suffix, '.RData')))

## -----------------------------------------------------------------------------

mdb <- mfdb('noaa_spatial.duckdb', destroy_schema = FALSE)

## -----------------------------------------------------------------------------
## Load taxonomies
## -----------------------------------------------------------------------------

## Area and division
mfdb_import_area(mdb, 
                 tibble(id = 1:4,
                             name = as.character(1:4),
                             size = rep(1,4),
                             division = rep(1, 4)))

## Species
species_map <- 
  tibble(id = c(172423),
         name = c('YFT'),
         description = c('Yellowfin tuna (Thunnus albacares)'),
         species = c('yellowfin'))

# species_map <- 
#   tibble(id = c(172423, 642808),
#          name = c('YFT', 'TOA'),
#          description = c('Yellowfin tuna (Thunnus albacares)',
#                          'Antarctic toothfish (Dissostichus mawsoni)'),
#          species = c('yellowfin', 'antarctic toothfish'))

mfdb_import_species_taxonomy(mdb, species_map)

## -----------------------------------------------------------------------------

## Data sources - landings, length distributions and cpue - not including tagging yet
mfdb_import_cs_taxonomy(mdb,
                        'data_source',
                        tibble(id = 1:5,
                               name = c('landings', 'ldist', 'index', 'index_cv', 'tagging'),
                               description = c('Landings', 'Length distribution', 'Cpue', 'Cpue_cv', 'tagging')))

## -----------------------------------------------------------------------------

## Sampling type - used to separate separate simulations
mfdb_import_sampling_type(mdb, 
                          tibble(id = 1:n_sims,
                                 name = paste0('SIM_', 1:n_sims),
                                 description = paste0('Simulation ', 1:n_sims)))

## -----------------------------------------------------------------------------

## Gear
mfdb_import_cs_taxonomy(mdb, 
                        'gear', 
                        tibble(id = 1:nrow(gear_lookup), 
                               name = gear_lookup$gear,
                               description = c('Purse seine', 'Trol', 'Bait boat',
                                               'Gillnet', 'Longline', 'Other', 'Handline')))

## -----------------------------------------------------------------------------

## Index type
mfdb_import_cs_taxonomy(mdb,
                        'index_type',
                        tibble(id = 1,
                               name = 'cpue',
                               description = 'Catch-per-unit-effort'))

## -----------------------------------------------------------------------------
## Total catch - YFT
## -----------------------------------------------------------------------------

catch_data <- NULL
index_data <- NULL
len_data <- NULL

for (i in 1:n_sims){
  
  sim_data <- get(paste0('dat_4A_', i))
  fleet_df <- 
    sim_data$fleetinfo %>% 
    mutate(name = rownames(.)) %>% 
    mutate(noaa_gear = gsub('(fishing)_([A-Za-z]+)_(.+$)', '\\2', .$name)) %>% 
    mutate(noaa_gear = gsub("(cpue[0-9])", "", .$noaa_gear)) %>% 
    left_join(gear_lookup, by = 'noaa_gear') %>% 
    mutate(index = as.character(1:nrow(.))) %>% 
    select(-noaa_gear)
  
  tmp_catch_data <- 
    sim_data$catch %>% 
    pivot_longer(cols = starts_with('fishing_')) %>% 
    left_join(fleet_df, by = 'name') %>% 
    left_join(time_df, by = 'year') %>%
    select(-year) %>% 
    mutate(species = 'YFT',
           year = nyear) %>% 
    mutate(sampling_type = paste0('SIM_', i)) %>% 
    mutate(count = value * 1e3) %>% 
    select(year, month, sampling_type, gear, areacell = areas, count, species)
  
  tmp_index_data <- 
    sim_data$CPUE %>%
    mutate(year = as.numeric(levels(.$year)[.$year])) %>% 
    left_join(fleet_df, by = 'index') %>% 
    left_join(time_df, by = 'year') %>% 
    mutate(species = 'YFT',
           index_type = 'cpue',
           sampling_type = paste0('SIM_', i)) %>%
    select(index_type, year = nyear, month, areacell = areas, value = cpu, species, cv)
  
  
  tmp_len_data <- 
    sim_data$lencomp %>% 
    pivot_longer(cols = starts_with('l')) %>%
    mutate(index = as.character(FltSvy),
           species = 'YFT',
           sampling_type = paste0('SIM_', i)) %>% 
    rename(len = name, year = Yr) %>% 
    left_join(fleet_df, by = 'index') %>% 
    left_join(time_df, by = 'year') %>% 
    mutate(len = gsub('l', '', len) %>% as.numeric()) %>% 
    mutate(length_min = len - 5) %>% # Lengths in simulated data appear to be upper bounds
    select(gear, sampling_type, year = nyear, month, areacell = areas,
           species, length_min, count = value)
    
  catch_data <- rbind(catch_data, tmp_catch_data)
  index_data <- rbind(index_data, tmp_index_data)
  len_data <- rbind(len_data, tmp_len_data)

}
  
## Load into MFDB
mfdb_import_survey(mdb, catch_data, data_source = 'landings')

## Index and cv
mfdb_import_survey_index(mdb, 
                         index_data %>% select(index_type, species, year, month, areacell, value), 
                         data_source = 'index')

mfdb_import_survey_index(mdb, 
                         index_data %>% select(index_type, species, year, month, areacell, value = cv), 
                         data_source = 'index_cv')

## Length dists
mfdb_import_survey(mdb,
                   len_data, 
                   data_source = 'ldist')


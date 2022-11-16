library(mfdb)
library(tidyverse)
source('R/src.R')

## Load data objects for both species
species_code <- c('YFT')#, 'TOA')
n_sims <- 100

## -----------------------------------------------------------------------------
## Lookup tables
## -----------------------------------------------------------------------------

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

## Lookup table for time, month is the mid-point of each quarter.. see data$surveytiming
time_df <- tibble(year = 1:256, 
                  nyear = rep(1952:2015, each = 4), 
                  month = rep(c(2, 5, 8, 11), 256/4))

## Gear lookup table
gear_lookup <- 
  tibble(noaa_gear = c('ps', 'trol', 'bb', 'gi', 'll', 'other', 'hd', 'hand', 
                       'll1', 'll2', 'll3', 'll4'),
         gear = c('PSE', 'TROL', 'BB', 'GIL', 'LLN', 'OTHER', 'HLN', 'HLN', rep('LLN', 4)))

## -----------------------------------------------------------------------------

#mdb <- mfdb('noaa_spatial.duckdb', destroy_schema = TRUE)
mdb <- mfdb('noaa_spatial.duckdb', destroy_schema = FALSE)


## -----------------------------------------------------------------------------
## Load taxonomies
## -----------------------------------------------------------------------------

## NOTE, we will upload the single and four area aggregations separately rather 
## than aggregating the four area data to one data

## Area and division
mfdb_import_area(mdb, 
                 tibble(id = 1:4,
                        name = as.character(1:4),
                        size = rep(1, 4),
                        division = rep(1, 4)))

## Species
mfdb_import_species_taxonomy(mdb, species_map)

## -----------------------------------------------------------------------------

## Data sources - landings, length distributions and cpue
mfdb_import_cs_taxonomy(mdb,
                        'data_source',
                        tibble(id = 1:14,
                               name = c('landings', 'ldist', 'index', 'index_cv', 'tagging', 'cpue', 'cpue_cv',
                                        'landings_fourarea', 'ldist_fourarea', 'index_fourarea', 'index_cv_fourarea', 'tagging_fourarea', 'cpue_fourarea', 'cpue_cv_fourarea'),
                               description = rep(c('Landings', 'Length distribution', 'Index', 'Index_cv', 'tagging', 'Cpue', 'Cpue_cv'),2)))

## -----------------------------------------------------------------------------

## Sampling type - used to separate separate simulations
mfdb_import_cs_taxonomy(mdb, 
                        'sampling_type',
                        tibble(id = 1:n_sims,
                               name = paste0('sim_', 1:n_sims),
                               description = paste0('Simulation ', 1:n_sims)))

## -----------------------------------------------------------------------------

## Gear
mfdb_import_cs_taxonomy(mdb, 
                        'gear', 
                        tibble(id = 1:nrow(gear_lookup[1:7,]), 
                               name = gear_lookup$gear[1:7],
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
## Single area data
## -----------------------------------------------------------------------------

single_area <- list(catch = NULL, index = NULL, ldist = NULL)

load(file = file.path('../Spatial-Assessment-Modeling-Workshop-MASTER/data', paste0('YFT_1area_observations_1_100_ESS_05.RData')))

for (i in 1:n_sims){
  
  sim_data <- get(paste0('dat_1A_', i))
  fleet_df <- 
    sim_data$fleetinfo %>% 
    mutate(name = rownames(.)) %>% 
    mutate(noaa_gear = gsub('(fishing_|cpue)', '', .$name)) %>%  
    mutate(noaa_gear = gsub('(.+)_[0-9]', '\\1', .$noaa_gear)) %>% 
    left_join(gear_lookup, by = 'noaa_gear') %>% 
    mutate(index = as.character(1:nrow(.))) %>% 
    mutate(index = ifelse(grepl('cpue', .$name), 'R1', index)) %>% 
    select(-noaa_gear)
  
  single_area$catch <- rbind(single_area$catch,
                             collate_catch(sim_data, fleet_df, species_code, paste0('sim_', i)))
  
  single_area$index <- rbind(single_area$index, 
                             collate_index(sim_data, fleet_df, species_code, paste0('sim_', i)))
  
  single_area$index_num <- rbind(single_area$index_num,
                                 collate_index_as_num(sim_data, fleet_df, species_code, paste0('sim_', i)))
  
  single_area$ldist <- rbind(single_area$ldist,
                             collate_ldist(sim_data, fleet_df, species_code, paste0('sim_', i)))
  
}

## Load into MFDB
## Catches
mfdb_import_survey(mdb, single_area$catch, data_source = 'landings')
## Index 
mfdb_import_survey_index(mdb, single_area$index %>% select(-cv), data_source = 'index')

## Index cv
mfdb_import_survey_index(mdb, 
                         single_area$index %>% 
                           select(-value) %>% 
                           rename(value = cv), 
                         data_source = 'index_cv')

## Index as numbers
mfdb_import_survey(mdb, single_area$index_num %>% select(-cv), data_source = 'cpue')

## Index cv as numbers
mfdb_import_survey(mdb, 
                   single_area$index_num %>% 
                     select(-count) %>%
                     rename(count = cv), 
                   data_source = 'cpue_cv')


## Length dist
mfdb_import_survey(mdb, single_area$ldist, data_source = 'ldist')

################################################################################

four_area <- list(catch = NULL, index = NULL, ldist = NULL)

load(file = file.path('../Spatial-Assessment-Modeling-Workshop-MASTER/data', paste0('YFT_4area_observations_1_100_ESS_05.RData')))

for (i in 1:n_sims){
  
  sim_data <- get(paste0('dat_4A_', i))
  fleet_df <- 
    sim_data$fleetinfo %>% 
    mutate(name = rownames(.)) %>% 
    mutate(noaa_gear = gsub('(fishing_|cpue)', '', .$name)) %>%  
    mutate(noaa_gear = gsub('(.+)_[0-9]', '\\1', .$noaa_gear)) %>% 
    left_join(gear_lookup, by = 'noaa_gear') %>% 
    mutate(index = as.character(1:nrow(.))) %>% 
    mutate(index = ifelse(grepl('cpue', .$name), 'R1', index)) %>% 
    select(-noaa_gear)
  
  four_area$catch <- rbind(four_area$catch,
                           collate_catch(sim_data, fleet_df, species_code, paste0('sim_', i)))
  
  four_area$index <- rbind(four_area$index,
                           collate_index(sim_data, fleet_df, species_code, paste0('sim_', i)))
  
  four_area$index_num <- rbind(four_area$index_num,
                                 collate_index_as_num(sim_data, fleet_df, species_code, paste0('sim_', i)))
  
  four_area$ldist <- rbind(four_area$ldist,
                           collate_ldist(sim_data, fleet_df, species_code, paste0('sim_', i)))
  
}

## Load into MFDB
## Catches
mfdb_import_survey(mdb, four_area$catch, data_source = 'landings_fourarea')
## Index 
mfdb_import_survey_index(mdb, four_area$index %>% select(-cv), data_source = 'index_fourarea')
## Index cv
mfdb_import_survey_index(mdb, 
                         four_area$index %>% 
                           select(-value) %>% 
                           rename(value = cv), 
                         data_source = 'index_cv_fourarea')

## Index as numbers
mfdb_import_survey(mdb, four_area$index_num %>% select(-cv), data_source = 'cpue_fourarea')

## Index cv as numbers
mfdb_import_survey(mdb, 
                         four_area$index_num %>% 
                           select(-count) %>% 
                           rename(count = cv), 
                         data_source = 'cpue_cv_fourarea')

## Length dist
mfdb_import_survey(mdb, four_area$ldist, data_source = 'ldist_fourarea')

mfdb_disconnect(mdb)





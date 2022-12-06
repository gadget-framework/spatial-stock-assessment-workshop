## -----------------------------------------------------------------------------
##
## Catches by fleet
##
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Surveys
## -----------------------------------------------------------------------------

survey_landings <- 
  structure(expand.grid(year = 1972:2015, step = 1:4) %>% 
              mutate(area = 1, total_weight = 1),
            area_group = mfdb_group(`1` = 1)) %>%
  arrange(year)

## -----------------------------------------------------------------------------
## Fisheries
## -----------------------------------------------------------------------------

trol_landings <- 
  mfdb_sample_count(mdb,
                    NULL, 
                    c(list(data_source = paste0('landings', data_source_suffix), 
                           gear = 'TROL'),
                      defaults))

pse_landings <- 
  mfdb_sample_count(mdb,
                    NULL, 
                    c(list(data_source = paste0('landings', data_source_suffix), 
                           gear = 'PSE'),
                      defaults))
                  
bb_landings <- 
  mfdb_sample_count(mdb,
                    NULL, 
                    c(list(data_source = paste0('landings', data_source_suffix), 
                           gear = 'BB'),
                      defaults))

gil_landings <- 
  mfdb_sample_count(mdb,
                    NULL, 
                    c(list(data_source = paste0('landings', data_source_suffix), 
                           gear = 'GIL'),
                      defaults))

lln_landings <- 
  mfdb_sample_count(mdb,
                    NULL, 
                    c(list(data_source = paste0('landings', data_source_suffix), 
                           gear = 'LLN'),
                      defaults))

other_landings <- 
  mfdb_sample_count(mdb,
                    NULL, 
                    c(list(data_source = paste0('landings', data_source_suffix), 
                           gear = 'OTHER'),
                      defaults))

hln_landings <- 
  mfdb_sample_count(mdb,
                    NULL, 
                    c(list(data_source = paste0('landings', data_source_suffix), 
                           gear = 'HLN'),
                      defaults))

all_landings <- 
  mfdb_sample_count(mdb,
                    NULL, 
                    c(list(data_source = paste0('landings', data_source_suffix)),
                      defaults))

if (TRUE){
  save(lln_landings, hln_landings, other_landings, pse_landings, 
       gil_landings, bb_landings, trol_landings, survey_landings, all_landings,
       file = file.path('MODEL', 'data', 'fleet_data.Rdata'))
}

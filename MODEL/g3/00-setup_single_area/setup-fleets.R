## -----------------------------------------------------------------------------
##
## Fleet actions:
##
## -----------------------------------------------------------------------------

############ Configure fleets ##################################################

pse <-
  g3_fleet('pse') %>%
  g3s_livesonareas(areas[c('1')])

trol <-
  g3_fleet('trol') %>%
  g3s_livesonareas(areas[c('1')])

bb <-
  g3_fleet('bb') %>%
  g3s_livesonareas(areas[c('1')])

gil <-
  g3_fleet('gil') %>%
  g3s_livesonareas(areas[c('1')])

lln <-
  g3_fleet('lln') %>%
  g3s_livesonareas(areas[c('1')])

other <-
  g3_fleet('other') %>%
  g3s_livesonareas(areas[c('1')])

hln <-
  g3_fleet('hln') %>%
  g3s_livesonareas(areas[c('1')])

survey <- 
  g3_fleet('survey') %>% 
  g3s_livesonareas(areas[c('1')])

## -----------------------------------------------------------------------------
## Create fleet actions

fleet_actions <-
  
  list(
    pse %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('comm.alpha', by_stock = 'species'),
                                                                        g3_parameterized('comm.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('pse_landings', pse_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number'))),
    
    trol %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('comm.alpha', by_stock = 'species'),
                                                                        g3_parameterized('comm.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('trol_landings', trol_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number'))),
    bb %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('comm.alpha', by_stock = 'species'),
                                                                        g3_parameterized('comm.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('bb_landings', bb_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number'))),
    
    gil %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('comm.alpha', by_stock = 'species'),
                                                                        g3_parameterized('comm.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('gil_landings', gil_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number'))),
    
    lln %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('comm.alpha', by_stock = 'species'),
                                                                        g3_parameterized('comm.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('lln_landings', lln_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number'))),
    
    other %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('comm.alpha', by_stock = 'species'),
                                                                        g3_parameterized('comm.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('other_landings', other_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number'))),
    
    hln %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('comm.alpha', by_stock = 'species'),
                                                                        g3_parameterized('comm.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('hln_landings', hln_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number'))),
    list()
  )

survey_actions <- 
  list(
    survey %>%
      g3a_predate_fleet(stocks,
                        suitabilities =
                          stocks %>%
                          set_names(.,map(.,'name')) %>%
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('survey.alpha', by_stock = 'species'),
                                                                        g3_parameterized('survey.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('survey_landings', survey_landings %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    list()
  )

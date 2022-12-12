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
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('pse.alpha', by_stock = 'species'),
                                                                        g3_parameterized('pse.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('pse_landings', pse_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number')),
                        run_f = ~cur_year > 1951),
    
    trol %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('trol.alpha', by_stock = 'species'),
                                                                        g3_parameterized('trol.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('trol_landings', trol_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number')),
                        run_f = ~cur_year > 1951),
    bb %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('bb.alpha', by_stock = 'species'),
                                                                        g3_parameterized('bb.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('bb_landings', bb_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number')),
                        run_f = ~cur_year > 1951),
    
    gil %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('gil.alpha', by_stock = 'species'),
                                                                        g3_parameterized('gil.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('gil_landings', gil_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number')),
                        run_f = ~cur_year > 1951),
    
    lln %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('lln.alpha', by_stock = 'species'),
                                                                        g3_parameterized('lln.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('lln_landings', lln_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number')),
                        run_f = ~cur_year > 1951),
    
    other %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('other.alpha', by_stock = 'species'),
                                                                        g3_parameterized('other.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('other_landings', other_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number')),
                        run_f = ~cur_year > 1951),
    
    hln %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('hln.alpha', by_stock = 'species'),
                                                                        g3_parameterized('hln.l50', by_stock = 'species'))),
                        catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('hln_landings', hln_landings[[1]] %>%
                                                                                                mutate(area = as.numeric(area),
                                                                                                       step = as.numeric(step),
                                                                                                       year = as.numeric(year)), value_field = 'number')),
                        run_f = ~cur_year > 1951),
    list()
  )

survey_actions <- 
  list(
    
    ## Trying to mimic CPUE with effort fleet here
    survey %>% 
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(g3_parameterized('lln.alpha', by_stock = 'species', exponentiate = FALSE),
                                                                        g3_parameterized('lln.l50', by_stock = 'species', exponentiate = FALSE))),
                        catchability_f = g3a_predate_catchability_effortfleet(
                          catchability_fs = g3_parameterized('catchability', optimise = TRUE),
                          E = ~1e-6)
                        ,
                        run_f = ~cur_year > 1951),
    
    list()
  )

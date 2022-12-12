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
                          map(function(x) g3_suitability_andersen(
                            p0 = g3_parameterized('ander.p0', by_stock = 'species'),
                            p1 = gadget3:::f_substitute(~log(x/y), 
                                                        list(x = g3_parameterized('ander.p5', by_stock = 'species'),
                                                             y = g3_parameterized('pse.Lmode', by_stock = 'species'))),
                            p2 = g3_parameterized('ander.p2', by_stock = 'species'),
                            p3 = g3_parameterized('pse.p3', by_stock = 'species'),
                            p4 = g3_parameterized('pse.p4', by_stock = 'species'),
                            p5 = g3_parameterized('ander.p5', by_stock = 'species')
                          )),
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
                          map(function(x) g3_suitability_andersen(
                            p0 = g3_parameterized('ander.p0', by_stock = 'species'),
                            p1 = gadget3:::f_substitute(~log(x/y), 
                                                        list(x = g3_parameterized('ander.p5', by_stock = 'species'),
                                                             y = g3_parameterized('trol.Lmode', by_stock = 'species'))),
                            p2 = g3_parameterized('ander.p2', by_stock = 'species'),
                            p3 = g3_parameterized('trol.p3', by_stock = 'species'),
                            p4 = g3_parameterized('trol.p4', by_stock = 'species'),
                            p5 = g3_parameterized('ander.p5', by_stock = 'species')
                          )),
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
                          map(function(x) g3_suitability_andersen(
                            p0 = g3_parameterized('ander.p0', by_stock = 'species'),
                            p1 = gadget3:::f_substitute(~log(x/y), 
                                                        list(x = g3_parameterized('ander.p5', by_stock = 'species'),
                                                             y = g3_parameterized('bb.Lmode', by_stock = 'species'))),
                            p2 = g3_parameterized('ander.p2', by_stock = 'species'),
                            p3 = g3_parameterized('bb.p3', by_stock = 'species'),
                            p4 = g3_parameterized('bb.p4', by_stock = 'species'),
                            p5 = g3_parameterized('ander.p5', by_stock = 'species')
                          )),
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
                          map(function(x) g3_suitability_andersen(
                            p0 = g3_parameterized('ander.p0', by_stock = 'species'),
                            p1 = gadget3:::f_substitute(~log(x/y), 
                                                        list(x = g3_parameterized('ander.p5', by_stock = 'species'),
                                                             y = g3_parameterized('gil.Lmode', by_stock = 'species'))),
                            p2 = g3_parameterized('ander.p2', by_stock = 'species'),
                            p3 = g3_parameterized('gil.p3', by_stock = 'species'),
                            p4 = g3_parameterized('gil.p4', by_stock = 'species'),
                            p5 = g3_parameterized('ander.p5', by_stock = 'species')
                          )),
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
                          map(function(x) g3_suitability_andersen(
                            p0 = g3_parameterized('ander.p0', by_stock = 'species'),
                            p1 = gadget3:::f_substitute(~log(x/y), 
                                                        list(x = g3_parameterized('ander.p5', by_stock = 'species'),
                                                             y = g3_parameterized('other.Lmode', by_stock = 'species'))),
                            p2 = g3_parameterized('ander.p2', by_stock = 'species'),
                            p3 = g3_parameterized('other.p3', by_stock = 'species'),
                            p4 = g3_parameterized('other.p4', by_stock = 'species'),
                            p5 = g3_parameterized('ander.p5', by_stock = 'species')
                          )),
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
                          map(function(x) g3_suitability_andersen(
                            p0 = g3_parameterized('ander.p0', by_stock = 'species'),
                            p1 = gadget3:::f_substitute(~log(x/y), 
                                                        list(x = g3_parameterized('ander.p5', by_stock = 'species'),
                                                             y = g3_parameterized('hln.Lmode', by_stock = 'species'))),
                            p2 = g3_parameterized('ander.p2', by_stock = 'species'),
                            p3 = g3_parameterized('hln.p3', by_stock = 'species'),
                            p4 = g3_parameterized('hln.p4', by_stock = 'species'),
                            p5 = g3_parameterized('ander.p5', by_stock = 'species')
                          )),
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
                          E = ~1e-9)
                        ,
                        run_f = ~cur_year > 1951),
    
    list()
  )

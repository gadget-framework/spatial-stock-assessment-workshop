## -----------------------------------------------------------------------------
##
## Set up tagging action
##
## -----------------------------------------------------------------------------

## Purse seine fleet only
## Suitability age-based in the operating model with the proportation tagged split
## between stocks depending on their relative proportions.
##
## We'll have it length based using  a flat suitability in the instance '_flat' 
## and length based in another

tag_pse <- g3_fleet('tag_pse') %>% g3s_livesonareas(areas[c('1')])

tagging_actions_flat <- 
  list(
    g3a_predate_tagrelease(tag_pse,
                           stocks,
                           suitabilities = list('yft_imm' = 1, 'yft_mat' = 1),
                           catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata('pse_tag_numbers',
                                                                                                 tagrel %>% 
                                                                                                   group_by(year, step, area) %>% 
                                                                                                   summarise(value = sum(nrel)) %>% 
                                                                                                   ungroup(),
                                                                                                 value_field = 'value')),
                           mortality_f = ~0.1,
                           output_tag_f = g3_timeareadata('pse_tagrel',
                                                           tagrel %>% select(-nrel), 
                                                           value_field = 'tg')
    ),
    
    g3a_tag_shedding(
      stocks,
      tagshed = 0.91,
      run_f = ~cur_year >= g3_param('tagshed_start', optimise = FALSE, value = 2005)),
    list()
  )


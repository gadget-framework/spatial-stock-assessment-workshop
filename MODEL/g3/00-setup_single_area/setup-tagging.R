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

tag_pse <-
  g3_fleet('tag_pse') %>%
  g3s_livesonareas(areas[c('1')])

tagging_actions_flat <- 
  list(
    g3a_predate_tagrelease(tag_pse,
                           stocks,
                           suitabilities = list(yft_imm = 1, yft_mat = 1),
                           catchability_f = g3a_predate_catchability_numberfleet(
                             gadget3:::f_substitute(~x, list(x = round(sum(tagrel$nrel), 0)))
                           ),
                           mortality_f = ~0.1,
                           output_tag_f = g3_timeareadata('pse_tagrel',
                                                           tagrel %>% select(-nrel), 
                                                           value_field = 'tg')
    ),
    
    g3a_tag_shedding(
      stocks,
      tagshed = 0.91,
      run_f = ~cur_year >= g3_param('tagshed_start')),
    list()
  )

tagging_actions_test <- 
  list(
    g3a_predate_tagrelease(tag_pse,
                           list(imm_stock),
                           suitabilities = list(yft_imm = 1),
                           catchability_f = g3a_predate_catchability_numberfleet(~100),
                           mortality_f = ~0.1,
                           output_tag_f = g3_timeareadata('pse_tagrel',
                                                          tagrel %>% select(-nrel), 
                                                          value_field = 'tg')
    ),
    
    g3a_tag_shedding(
      list(imm_stock),
      tagshed = 0.91,
      run_f = ~cur_year >= g3_param('tagshed_start')),
    list()
  )



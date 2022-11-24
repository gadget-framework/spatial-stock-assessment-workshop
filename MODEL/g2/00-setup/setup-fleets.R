

tmp <-
  gadgetfleet('Modelfiles/fleet',gd,missingOkay = TRUE) %>% 
  
  # gadget_update('totalfleet',
  #               name = 'survey',
  #               suitability = paste0('\n',
  #                                    paste(stock_names,
  #                                          'function','exponentiall50',
  #                                          '#lln.alpha','#lln.l50',
  #                                          collapse='\n')),
  #               data = survey_landings) %>%
  
  gadget_update('numberfleet',
                name = 'pse',
                suitability = paste0('\n',
                                     paste(stock_names,
                                           'function','andersenfleet',
                                           '#yft.pse.p0',to.gadget.formulae(quote(log(195/yft.pse.lmode))),'#yft.pse.p2',
                                           '#yft.pse.p3','#yft.pse.p4','195',
                                           collapse='\n')),
                
                data = pse_landings[[1]]) %>%
  
  gadget_update('numberfleet',
                name = 'trol',
                suitability = paste0('\n',
                                     paste(stock_names,
                                           'function','andersenfleet',
                                           '#yft.trol.p0',to.gadget.formulae(quote(log(195/yft.trol.lmode))),'#yft.trol.p2',
                                           '#yft.trol.p3','#yft.trol.p4','195',
                                           collapse='\n')),
                data = trol_landings[[1]]) %>%
  
  gadget_update('numberfleet',
                name = 'bb',
                suitability = paste0('\n',
                                     paste(stock_names,
                                           'function','andersenfleet',
                                           '#yft.bb.p0',to.gadget.formulae(quote(log(195/yft.bb.lmode))),'#yft.bb.p2',
                                           '#yft.bb.p3','#yft.bb.p4','195',
                                           collapse='\n')),
                data = bb_landings[[1]]) %>% 
  
  gadget_update('numberfleet',
                name = 'gil',
                suitability = paste0('\n',
                                     paste(stock_names,
                                           'function','andersenfleet',
                                           '#yft.gil.p0',to.gadget.formulae(quote(log(195/yft.gil.lmode))),'#yft.gil.p2',
                                           '#yft.gil.p3','#yft.gil.p4','195',
                                           collapse='\n')),
                data = gil_landings[[1]]) %>%
  
  gadget_update('numberfleet',
                name = 'lln',
                suitability = paste0('\n',
                                     paste(stock_names,
                                           'function','exponentiall50',
                                           '#lln.alpha','#lln.l50',
                                           collapse='\n')),
                data = lln_landings[[1]]) %>%
  
  gadget_update('numberfleet',
                name = 'other',
                suitability = paste0('\n',
                                     paste(stock_names,
                                           'function','andersenfleet',
                                           '#yft.other.p0',to.gadget.formulae(quote(log(195/yft.other.lmode))),'#yft.other.p2',
                                           '#yft.other.p3','#yft.other.p4','195',
                                           collapse='\n')),
                data = other_landings[[1]]) %>%
  
  gadget_update('numberfleet',
                name = 'hln',
                suitability = paste0('\n',
                                     paste(stock_names,
                                           'function','andersenfleet',
                                           '#yft.hln.p0',to.gadget.formulae(quote(log(195/yft.hln.lmode))),'#yft.hln.p2',
                                           '#yft.hln.p3','#yft.hln.p4','195',
                                           collapse='\n')),
                data = hln_landings[[1]]) 

  tmp %>% 
   write.gadget.file(gd)

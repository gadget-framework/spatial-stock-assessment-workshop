collate_catch <- function(data, fleet_df, species, sampling_type){
  
  return(
    data$catch %>% 
      pivot_longer(cols = starts_with('fishing_')) %>% 
      mutate(name = ifelse(name == 'fishing_hand', 'fishing_hd', name)) %>% 
      left_join(fleet_df, by = 'name') %>% 
      mutate(month = 1,
             species = species,
             sampling_type = sampling_type,
             count = value*1000) %>% 
      select(year, month, areacell = areas, 
             sampling_type, gear, count, species)
    )
  
}

collate_index <- function(data, fleet_df, species, sampling_type){
  
  return(
    data$CPUE %>%
      mutate(year = as.numeric(levels(.$year)[.$year])) %>% 
      left_join(fleet_df, by = 'index') %>% 
      mutate(month = 1, 
             species = species,
             index_type = 'cpue',
             sampling_type = sampling_type) %>%
      select(year, month, areacell = areas, 
             species, sampling_type,  
             index_type, value = cpu, cv)
  )
  
}

collate_index_as_num <- function(data, fleet_df, species, sampling_type){
  
  return(
    data$CPUE %>%
      mutate(year = as.numeric(levels(.$year)[.$year])) %>% 
      left_join(fleet_df, by = 'index') %>% 
      mutate(month = 1, 
             species = species,
             #index_type = 'cpue',
             sampling_type = sampling_type,
             length = 12.5,
             length_min = 10) %>%
      select(year, month, areacell = areas, 
             species, sampling_type, length, length_min,  
             count = cpu, cv)
  )
  
}

collate_ldist <- function(data, fleet_df, species, sampling_type){
  
  return(
    data$lencomp %>% 
      pivot_longer(cols = starts_with('l')) %>%
      mutate(index = as.character(FltSvy),
             species = species,
             sampling_type = sampling_type,
             month = 1) %>% 
      rename(len = name, year = Yr) %>% 
      left_join(fleet_df, by = 'index') %>% 
      mutate(len = gsub('l', '', len) %>% as.numeric()) %>% 
      mutate(length = len - 2.5, 
             length_min = len - 5) %>% # Lengths in simulated data appear to be upper bounds
      select(year, month, areacell = areas, 
             sampling_type, gear, species, 
             length, length_min, count = value)
  )
  
}

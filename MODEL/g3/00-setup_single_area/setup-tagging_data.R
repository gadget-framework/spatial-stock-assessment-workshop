load(file = '../Spatial-Assessment-Modeling-Workshop-MASTER/data/YFT_SRD_1A_4.RData')

## Lookup table for time, month is the mid-point of each quarter.. see data$surveytiming
time_df <- tibble(year = 1:256, 
                  nyear = rep(1952:2015, each = 4), 
                  nmonth = rep(c(2, 5, 8, 11), 256/4),
                  step = rep(1:4, 256/4))

## Releases 
tmp <- dat_1A_4$tag_releases
tagrel <- 
  tmp %>% 
  rename(year = yr) %>% 
  left_join(time_df, by = 'year') %>% 
  select(-year) %>% 
  rename(year = nyear, month = nmonth, area = reg) %>% 
  select(year, step, area, tg, nrel) 

attributes(tagrel)$area <- defaults$area
attributes(tagrel)$step <- defaults$timestep
attributes(tagrel)$year <- attr(trol_landings$`0.0.0`, 'year')


## Recaptures
tmp2 <- dat_1A_4$tag_recaps
rm(biol_dat, dat_1A_4)

tags <- structure(tagrel$tg, names = tagrel$tg)

tagrecap <- 
  tmp2 %>% 
  rename(year = yr) %>% 
  left_join(time_df, by = 'year') %>% 
  select(-year, year = nyear, month = nmonth) %>% 
  mutate(area = 1) %>% 
  select(year, step, area, recaps) %>% 
  rename(number = recaps)
  








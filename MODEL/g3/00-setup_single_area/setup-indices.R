## -----------------------------------------------------------------------------
## Survey indices
## -----------------------------------------------------------------------------

cpue1 <- 
  mfdb_sample_count(mdb,
                    'length', 
                    c(list(
                      data_source = paste0('cpue', data_source_suffix)
                    ),
                    defaults))

cpue.all <- cpue1
cpue.all[[1]] <- bind_rows(expand.grid(year = 1952:1971, step = as.character(1:4)) %>% 
                             mutate(area = as.character(1), length = 'all', number = 1) %>% 
                             arrange(year),
                           cpue1$`0.0.0.0`)

## Add age and length attributes
attributes(cpue1[[1]])$age$all <- minage:maxage
attributes(cpue1[[1]])$length$all <- seq(minlength, maxlength, by = dl)

## Add age and length attributes
attributes(cpue.all[[1]])$age$all <- minage:maxage
attributes(cpue.all[[1]])$length$all <- seq(minlength, maxlength, by = dl)


if (TRUE){
  save(cpue1, cpue.all, file = file.path('MODEL', 'data', 'indices.Rdata'))
}

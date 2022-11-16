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

## Add age and length attributes
attributes(cpue1$`0.0.0`)$age$all <- minage:maxage


if (TRUE){
  save(cpue1, file = file.path('MODEL', 'data', 'indices.Rdata'))
}

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
attributes(cpue1[[1]])$age$all <- minage:maxage
attributes(cpue1[[1]])$length$all <- seq(minlength, maxlength, by = dl)


if (TRUE){
  save(cpue1, file = file.path('MODEL', 'data', 'indices.Rdata'))
}

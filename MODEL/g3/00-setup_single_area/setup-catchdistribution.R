## -----------------------------------------------------------------------------
## Catch age and length distributions:
## -----------------------------------------------------------------------------

minage <- g3_stock_def(imm_stock, 'minage')
maxage <- g3_stock_def(mat_stock, 'maxage')
minlength <- g3_stock_def(imm_stock, 'minlen') %>% min()
maxlength <- g3_stock_def(mat_stock, 'minlen') %>% max()
dl <- g3_stock_def(mat_stock, 'stock__dl') %>% min()

Ls <- seq(10, 200, by = 5)

## Query length data 
## Commercial fleets
ldist.trol <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      gear = 'TROL',
                      data_source = paste0('ldist', data_source_suffix),
                      age = mfdb_interval("all",c(minage, maxage),
                                          open_ended = c("upper", "lower")),
                      length = mfdb_interval("len", 
                                             Ls,
                                             open_ended = c("upper","lower"))),
                      defaults))

ldist.pse <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      gear = 'PSE',
                      data_source = paste0('ldist', data_source_suffix),
                      age = mfdb_interval("all",c(minage, maxage),
                                          open_ended = c("upper", "lower")),
                      length = mfdb_interval("len", 
                                             Ls,
                                             open_ended = c("upper","lower"))),
                      defaults))

ldist.bb <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      gear = 'BB',
                      data_source = paste0('ldist', data_source_suffix),
                      age = mfdb_interval("all",c(minage, maxage),
                                          open_ended = c("upper", "lower")),
                      length = mfdb_interval("len", 
                                             Ls,
                                             open_ended = c("upper","lower"))),
                      defaults))

ldist.gil <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      gear = 'GIL',
                      data_source = paste0('ldist', data_source_suffix),
                      age = mfdb_interval("all",c(minage, maxage),
                                          open_ended = c("upper", "lower")),
                      length = mfdb_interval("len", 
                                             Ls,
                                             open_ended = c("upper","lower"))),
                      defaults))

ldist.lln <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      gear = 'LLN',
                      data_source = paste0('ldist', data_source_suffix),
                      age = mfdb_interval("all",c(minage, maxage),
                                          open_ended = c("upper", "lower")),
                      length = mfdb_interval("len", 
                                             Ls,
                                             open_ended = c("upper","lower"))),
                      defaults))

ldist.other <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      gear = 'OTHER',
                      data_source = paste0('ldist', data_source_suffix),
                      age = mfdb_interval("all",c(minage, maxage),
                                          open_ended = c("upper", "lower")),
                      length = mfdb_interval("len", 
                                             Ls,
                                             open_ended = c("upper","lower"))),
                      defaults))

ldist.hln <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      gear = 'HLN',
                      data_source = paste0('ldist', data_source_suffix),
                      age = mfdb_interval("all",c(minage, maxage),
                                          open_ended = c("upper", "lower")),
                      length = mfdb_interval("len", 
                                             Ls,
                                             open_ended = c("upper","lower"))),
                      defaults))



if (TRUE){
  save(ldist.pse,
       ldist.trol,
       ldist.bb,
       ldist.gil, ldist.lln,
       ldist.other, ldist.hln,
       file = file.path('MODEL', 'data', 'catchdistribution_data.Rdata'))
}

## -----------------------------------------------------------------------------
##
## Useful parameters and constants
##
## -----------------------------------------------------------------------------

## Natural mortality, length-at-age, maturity, and K (?)
params_age <- read.csv(file = file.path(base_dir, 'data/parameters_age.csv'))

params_bio <- list(
  
  walpha = 2.459e-5,
  wbeta = 2.9667,
  R0 = 97.2506e6,
  Linf = 145,
  Lmin = 22,
  Kbase = 0.455,
  mat.a50 = 9,
  mat.l50 = 74.721
  
)

## Check length-weight
plot(10:195, params_bio$walpha*(10:195)^params_bio$wbeta, type = 'l', xlab = 'length', ylab = 'weight')

## initial conditions sigma
# init.sigma <- 
#   mfdb_dplyr_sample(mdb) %>% 
#   dplyr::filter(species == local(defaults$species),age >0,!is.na(length))  %>% 
#   dplyr::select(age,length) %>% 
#   dplyr::collect(n=Inf) %>% 
#   dplyr::group_by(age) %>% 
#   dplyr::summarise(ml=mean(length,na.rm=TRUE),ms=sd(length,na.rm=TRUE))



if (TRUE){
  save(params_age, 
       params_bio,
       file = file.path('MODEL', 'data', 'init_param.Rdata'))
}

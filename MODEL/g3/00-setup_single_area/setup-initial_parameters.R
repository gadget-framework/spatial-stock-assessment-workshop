## -----------------------------------------------------------------------------
##
## Useful parameters and constants
##
## -----------------------------------------------------------------------------

params_bio <- list(
  
  walpha = 2.459e-5,
  wbeta = 2.9667,
  R0 = 97.2506e6,
  Linf = 145,
  Lmin = 22,
  Kbase = 0.455,
  mat.a50 = 9,
  mat.l50 = 74.721,
  lenage_cv = 0.1
  
)

## Read table of age varying information
tmp <- 
  read.csv2(file = 'MODEL/data/params_age.csv') %>% 
  bind_rows(do.call("rbind", replicate(4, tail(., n = 1), simplify = FALSE))) %>% 
  mutate(sd = L * params_bio$lenage_cv,
         age = rep(0:7, each = 4),
         step = rep(1:4, times = 8),
         area = 1) %>%
  mutate(K = ifelse(is.na(K), 1, K),
         Kbase = params_bio$Kbase,
         K_gadget = K * Kbase,
         K_om = K + Kbase, 
         M = M * 4) %>% 
  rename(Age_OM = Age) %>% 
  select(-X)

params_age <- 
  tmp %>% left_join(
    tmp %>% 
      group_by(age) %>% 
      summarise(Kyr = mean(K_gadget), 
                Kyr_om = mean(K_om),
                Myr = mean(M))
  , by = 'age')

rm(tmp)

## Check length-weight
plot(10:195, params_bio$walpha*(10:195)^params_bio$wbeta, type = 'l', xlab = 'length', ylab = 'weight')

if (TRUE){
  save(params_age, 
       params_bio,
       file = file.path('MODEL', 'data', 'init_param.Rdata'))
}



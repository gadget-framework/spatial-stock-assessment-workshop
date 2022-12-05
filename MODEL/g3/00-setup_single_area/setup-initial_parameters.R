## -----------------------------------------------------------------------------
##
## Useful parameters and constants
##
## -----------------------------------------------------------------------------

vonb <- function(age, Linf, K, recl){
  Linf * (1 - exp(-1 * K * (age - (1 + log(1 - 
                                             recl/Linf)/K))))  
}

find_recl <- function(data){
  
  age <- data$age
  K <- data$K_gadget
  Linf <- 145
  score <- data$L
  
  Ls <- seq(-200,200,by=0.1)
  
  out <- vonb(age, Linf, K, Ls)
  
  return(Ls[which.min(abs(score - out))])
  
  
}


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
dat <- read_table(file = 'MODEL/data/params_age')

## Standard deviation for len at age
dat$sd <- dat$L * params_bio$lenage_cv

## Extend the lookup table for an extra year
dat2 <- rbind(
  dat, 
  do.call("rbind", replicate(4, tail(dat, n = 1), simplify = FALSE))
)

## Add steps gadget year and step
dat2$age <- rep(0:7, each = 4)
dat2$step <- rep(1:4, times = 8)
dat2$area <- 1

## Add new K column, based on these multipliers
dat2$K <- ifelse(is.na(dat2$K), 1, dat2$K)
dat2$Kbase <- params_bio$Kbase
dat2$K_gadget <- dat2$K * dat2$Kbase

tmp <- lapply(split(dat2, 1:nrow(dat2)), function(x) return(find_recl(x)))
dat2$recl <- do.call('c', tmp)
dat2$initl <- vonb(dat2$age, 145, dat2$K_gadget, dat2$recl)

params_age <- dat2

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

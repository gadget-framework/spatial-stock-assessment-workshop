
g3a_bevholt_b0 <- function(h, R0, B0){
  # NB: bevertonholt is calculated over an entire area, so divide up so each age/length spawn equally.
  list(
    s = ~sum(stock_ss(stock__wgt) * stock_ss(stock__spawningnum)),
    r = gadget3:::f_substitute(~4L * h * R0 * s / ((B0 * (1L - h) + s * (5L * h - 1))), list(
      h = h,
      R0 = R0,
      B0 = B0)))
}

g3a_bevholt_casal <- function(R0, B0, h, ysc){
  
  list(
    s = ~sum(stock_ss(stock__wgt) * stock_ss(stock__spawningnum)),
    r = gadget3:::f_substitute(~ R0 * ysc * ((s/B0) / (1L - (5L*h - 1L)/(4L * h) * (1L - s/B0))),
      list(h = h,
           R0 = R0,
           B0 = B0,
           ysc = ysc)))
  
}

g3a_spawn_recruitment_pars <- function(spawnfun, method, mu = TRUE){
  
  if (!(spawnfun %in% c('bevertonholt', 'ricker'))){
    stop("The only valid arguments for this function are 'bevertonholt' and 'ricker'")
  }
  
  if (!(method %in% c('ss3', 'sv'))){
    stop("The only valid arguments for this function are 'ss3' and 'sv'")
  }
  
  if (method == 'ss3'){
    
    if (spawnfun == 'bevertonholt' ){
      
      if (mu){
        out <- gadget3:::f_substitute(~(4L * h * R0),
                                      list(h = g3_parameterized('srr_h', by_stock = 'species'),
                                           R0 = g3_parameterized('R0', by_stock = 'species')))
      }
      else{
        out <- gadget3:::f_substitute(~B0 * (1L - h) + s * (5L * h - 1) - s,
                                      list(B0 = g3_parameterized('B0', by_stock = 'species'),
                                           h = g3_parameterized('srr_h', by_stock = 'species'),
                                           s = ~sum(stock_ss(stock__wgt) * stock_ss(stock__spawningnum))))
      }
    }
    else{
      stop('not supported')
    }
    
  }
  else{
    
    if (spawnfun == 'bevertonholt' ){
      
      if (mu){
        out <- gadget3:::f_substitute(~(4L * B0 * h) / (R0 * (5L * h - 1L)),
                                      list(B0 = g3_parameterized('B0', by_stock = 'species'),
                                           h = g3_parameterized('srr_h', by_stock = 'species'),
                                           R0 = g3_parameterized('R0', by_stock = 'species')))
      }
      else{
        out <- gadget3:::f_substitute(~(B0 * (1L - h)) / (5L * h - 1L),
                                      list(B0 = g3_parameterized('B0', by_stock = 'species'),
                                           h = g3_parameterized('srr_h', by_stock = 'species')))
      }
    }
    else{
      
      if (mu){
        out <- gadget3:::f_substitute(~exp((b - B0) / R0),
                                      list(B0 = g3_parameterized('B0', by_stock = 'species'),
                                           b = g3_parameterized('srr_b', by_stock = 'species'),
                                           R0 = g3_parameterized('R0', by_stock = 'species')))
      }
      else{
        out <- gadget3:::f_substitute(~log(5L * h) / (0.8 * B0),
                                      list(B0 = g3_parameterized('B0', by_stock = 'species'),
                                           h = g3_parameterized('srr_h', by_stock = 'species')))
      }
    }
    
  }
  
  return(out)
}

g3a_spawn_bevholt_ss3_mu <- g3a_spawn_recruitment_pars(spawnfun = 'bevertonholt', method = 'ss3', mu = TRUE)
g3a_spawn_bevholt_ss3_lambda <- g3a_spawn_recruitment_pars(spawnfun = 'bevertonholt', method = 'ss3', mu = FALSE)
g3a_spawn_bevholt_sv_mu <- g3a_spawn_recruitment_pars(spawnfun = 'bevertonholt', method = 'sv', mu = TRUE)
g3a_spawn_bevholt_sv_lambda <- g3a_spawn_recruitment_pars(spawnfun = 'bevertonholt', method = 'sv', mu = FALSE)
g3a_spawn_ricker_sv_mu <- g3a_spawn_recruitment_pars(spawnfun = 'ricker', method = 'sv', mu = TRUE)
g3a_spawn_ricker_sv_lambda <- g3a_spawn_recruitment_pars(spawnfun = 'ricker', method = 'sv', mu = FALSE)


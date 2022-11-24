
## setup the immature stock first
yft.imm <- 
  gadgetstock(sprintf('%simm',species_name),gd,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 0,
                maxage = 2,
                minlength = 10,
                maxlength = 195,
                dl = 5,
                livesonareas = 1) %>%
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf=sprintf('#%s.Linf', species_name), 
                                   k=to.gadget.formulae(quote(0.001*yft.k)),
                                   alpha = sprintf('#%s.walpha',.[[1]]$stockname),
                                   beta = sprintf('#%s.wbeta',.[[1]]$stockname)),
                beta = to.gadget.formulae(quote(10*yft.bbin)),
                maxlengthgroupgrowth = 3) %>% 
  gadget_update('initialconditions',
                normalparam = tibble(age = .[[1]]$minage:.[[1]]$maxage,
                                         area = 1,
                                         age.factor = parse(text=sprintf('exp(-1*(%4$s+%3$s.init.F)*%1$s)*%2$s.init.%1$s',
                                                                         age,
                                                                         .[[1]]$stockname,
                                                                         species_name,
                                                                         params_age[params_age$age == .[[1]]$minage, 'M'][1])) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),   
                                         area.factor = sprintf('#%s.init.scalar',.[[1]]$stockname),
                                         mean = von_b_formula(age,linf=sprintf('%s.Linf',species_name),
                                                                              k=sprintf('%s.k',species_name),
                                                                              recl=sprintf('%s.recl',species_name)),
                                         stddev = params_age$sd[age+1],
                                         alpha = sprintf('#%s.walpha',.[[1]]$stockname),
                                         beta = sprintf('#%s.wbeta',.[[1]]$stockname))) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('refweight',
                data=tibble(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=params_bio$walpha*length^params_bio$wbeta)) %>% 
  gadget_update('iseaten',1) %>% 
  gadget_update('doesmature', 
                maturityfunction = 'continuous',
                maturestocksandratios = sprintf('%smat 1',species_name),
                coefficients = '( * 0.001 #yft.mat1) #yft.mat2 0 0')%>% 
  gadget_update('doesmove',
                transitionstocksandratios = sprintf('%smat 1',species_name),
                transitionstep = 4) %>% 
  gadget_update('doesrenew',
                normalparam = 
                  tibble(year = rep(year_range, each = 4),
                         step = rep(1:4, length(year_range)),
                         area = 1,
                         age = .[[1]]$minage,
                         number = parse(text=sprintf('%1$s.rec.scalar*%1$s.rec.%2$s',species_name,year)) %>% 
                           map(to.gadget.formulae) %>% 
                           unlist(),
                         mean = von_b_formula(age,linf=sprintf('%s.Linf',species_name),
                                              k=sprintf('%s.k',species_name),
                                              recl=sprintf('%s.recl',species_name)),
                         stddev = sprintf('#%s.rec.sd',species_name),
                         alpha = sprintf('#%s.walpha',.[[1]]$stockname),
                         beta = sprintf('#%s.wbeta',.[[1]]$stockname))) %>% 
  gadget_update('naturalmortality', params_age[seq(1,9,by=4),'M'])


yft.mat <-
  gadgetstock(sprintf('%smat',species_name),
              gd,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 1,
                maxage = 7,
                minlength = 10,
                maxlength = 195,
                dl = 5,
                livesonareas = 1) %>%
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf=sprintf('#%s.Linf',species_name), 
                                   k=to.gadget.formulae(quote(0.001*yft.k)),
                                   alpha = sprintf('#%s.walpha',.[[1]]$stockname),
                                   beta = sprintf('#%s.wbeta',.[[1]]$stockname)),
                beta = to.gadget.formulae(quote(10*yft.bbin)),
                maxlengthgroupgrowth = 3) %>% 
  gadget_update('initialconditions',
                normalparam = tibble(age = .[[1]]$minage:.[[1]]$maxage,
                                         area = 1,
                                         age.factor = parse(text=sprintf('exp(-1*(%4$s+%3$s.init.F)*%1$s)*%2$s.init.%1$s',
                                                                         age,
                                                                         .[[1]]$stockname,
                                                                         species_name,
                                                                         params_age[params_age$age == .[[1]]$minage, 'M'][1])) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),   
                                         area.factor = sprintf('#%s.init.scalar',.[[1]]$stockname),
                                         mean = von_b_formula(age,linf=sprintf('%s.Linf',species_name),
                                                              k=sprintf('%s.k',species_name),
                                                              recl=sprintf('%s.recl',species_name)),
                                         stddev = params_age$sd[age+1],
                                         alpha = sprintf('#%s.walpha',.[[1]]$stockname),
                                         beta = sprintf('#%s.wbeta',.[[1]]$stockname))) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('refweight',
                data=tibble(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=params_bio$walpha*length^params_bio$wbeta)) %>% 
  gadget_update('iseaten',1) %>% 
  gadget_update('naturalmortality', params_age[seq(5,29,by=4),'M'])


## write to file
yft.imm %>% 
  write.gadget.file(gd)

yft.mat %>% 
  write.gadget.file(gd)

Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd))
callGadget(s=1,log = 'init.log') #ignore.stderr = FALSE,

## update the input parameters with sane initial guesses
read.gadget.parameters(sprintf('%s/params.out',gd)) %>% 
  init_guess('init.[0-9]',50,0.001,100,1) %>%
  init_guess('rec.[0-9]',10,0.001,25,1) %>%
  init_guess('recl',22,5,30,0) %>% 
  init_guess('rec.sd',5,4,20,1) %>% 
  init_guess('Linf',params_bio$Linf, floor(params_bio$Linf*0.75), ceiling(params_bio$Linf*1.25),1) %>% 
  init_guess('k$',50, 40, 60,1) %>% 
  init_guess('bbin',6, 1e-08, 100, 1) %>% 
  init_guess('alpha', 0.5, 0.005, 3, 1) %>% 
  init_guess('l50',100,10,200,1) %>% 
  init_guess('walpha',params_bio$walpha, 1e-10, 1,0) %>% 
  init_guess('wbeta',params_bio$wbeta, 2, 4,0) %>% 
  #init_guess('M$',0.15,0.001,1,0) %>% 
  init_guess('rec.scalar', 5, 1, 10, 1) %>% 
  init_guess('init.scalar', 5, 1, 10, 1) %>% 
  init_guess('mat2',params_bio$mat.l50,0.5*params_bio$mat.l50,1.5*params_bio$mat.l50,1) %>% 
  init_guess('mat1', 70, 10, 200, 1) %>% 
  init_guess('init.F', 0.1, 0.01, 1, 1) %>% 
  init_guess('p0', 0, 0, 1, 0) %>% 
  init_guess('p2', 1, 0, 1, 0) %>% 
  init_guess('p3', 50, 0.01, 100, 1) %>% 
  init_guess('p4', 50, 0.01, 100, 1) %>% 
  init_guess('mode', 195/2, 50, 150, 1) %>% 
  write.gadget.parameters(.,file=sprintf('%s/params.in',gd))


## Useful constansts
minage <- yft.imm[[1]]$minage
maxage <- yft.mat[[1]]$maxage
maxlength <- yft.mat[[1]]$maxlength 
minlength <- yft.imm[[1]]$minlength
dl <- yft.imm[[1]]$dl

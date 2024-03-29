## -----------------------------------------------------------------------------
##
## Runner to set up stocks and associated actions
##
## -----------------------------------------------------------------------------

## Immature stock
imm_stock <- 
  #g3_stock(c(species = 'yft', 'imm'), seq(10, 195, 5)) %>%
  g3_stock(c(species = 'yft', 'imm'), seq(10, 200, 5)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(minage = 0, maxage = 2)

## Mature stock
mat_stock <- 
  g3_stock(c(species = 'yft', 'mat'), seq(10, 200, 5)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(minage = 1, maxage = 7)


if (include_tagging){
  
  source(file.path(base_dir, '00-setup_single_area', 'setup-tagging_data.R'))
  
  imm_stock <- 
    g3_stock(c(species = 'yft', 'imm'), seq(10, 195, 5)) %>%
    g3s_livesonareas(areas[c('1')]) %>%
    g3s_tag(tags) %>% 
    g3s_age(minage = 0, maxage = 2)
  
  mat_stock <- 
    g3_stock(c(species = 'yft', 'mat'), seq(10, 195, 5)) %>%
    g3s_livesonareas(areas[c('1')]) %>%
    g3s_tag(tags) %>% 
    g3s_age(minage = 1, maxage = 7)
  
}

stocks <- list(imm_stock, mat_stock)

## -----------------------------------------------------------------------------

## How do set up the initial population
## Options:
## 0 - population is initialised at equilibrium
## 1 - parameter for each age group (across stocks)
## 2 - parameter for each age group of each stock
init_abund_mode <- 2

## comp_id - stock id used to specify parameter names
comp_id <- 'species'

## -----------------------------------------------------------------------------
##
## Setup model actions
## Doing this separately for each component rather than using the wrapper in gadgetutils
##
## -----------------------------------------------------------------------------

load(file = file.path('MODEL', 'data/init_param.Rdata'))

comp_id <- 'species'

exp_linf <- FALSE
exp_k <- FALSE
exp_recl <- FALSE
exp_init <- FALSE
exp_rec <- FALSE
exp_bbin <- TRUE

## -----------------------------------------------------------------------------
## DEFINE SOME PARAMETERS PRIOR TO SETTING UP THE ACTIONS
## -----------------------------------------------------------------------------

## Weight-length
wl_par <- list(walpha = g3_parameterized('walpha', by_stock = 'species'),
               wbeta = g3_parameterized('wbeta', by_stock = 'species'))

## Growth
grow_par <- list(Linf = g3_parameterized('Linf', by_stock = 'species'),
                 K = g3_parameterized('K', by_stock = TRUE, scale = 1),
                 #bbin = g3_parameterized('bbin', by_stock = 'species', scale = 10, offset = 1, exponentiate = TRUE),
                 bbin = g3_parameterized('bbin', by_stock = 'species', scale = 10, value = 100, optimise = FALSE),
                 K_table = g3_timeareadata('K', params_age %>% select(age, step, K), value_field = 'K'),
                 mlgg = 10)

## Maturity
mat_par <- list(mat_alpha = g3_parameterized('mat_alpha', by_stock = TRUE, scale = 0.001),
                mat_l50 = g3_parameterized('mat_l50', by_stock = TRUE),
                mat_beta = g3_parameterized('mat_beta', by_stock = TRUE),
                mat_a50 = g3_parameterized('mat_a50', by_stock = TRUE))

## Renewal and initialisation
init_par <- list(recl = g3_parameterized('recl', by_stock = 'species'),
                 recsd = g3_parameterized('rec.sd', by_stock = 'species'))

## Spawning
spawn_par <- list(bh_mu = g3_parameterized('bh_mu', by_stock = TRUE),#, scale = 1e6), # Max possible recruits in millions
                  bh_lambda = g3_parameterized('bh_lambda', by_stock = TRUE))#, scale = 1e6)) # SSB to produce half max possible recruits (scaled to 000s T)

## Initial vonb
init_vonb <- g3a_renewal_vonb(Linf = grow_par$Linf,
                              K = grow_par$K,
                              recl = init_par$recl)

## -----------------------------------------------------------------------------
## SET UP DATASETS FOR SOME PARAMETER TABLES
##
## We do this for natural mortality and initial sd's because they are fixed 
## parameters (ie they will not be optimised) that vary with year and step
## 
## -----------------------------------------------------------------------------

params_age <- params_age %>% left_join(aggregate(list(Mtest=params_age$M), by = list(age = params_age$age), FUN=max)) 

maturity_init_data <- params_age[seq(1,29,by = 4),] %>% select(age, area, Mat)
mean_len_init_data <- params_age[seq(1,29,by=4),] %>% select(age, area, L)
init_sd_init_data <- params_age[seq(1,29,by=4),] %>% select(age, area, sd)# %>% mutate(sd = 1)


naturalmortality_data <- 
  params_age %>%
  #mutate(Mtest = ifelse(age %in% 6:7, 1.5*Mtest, Mtest)) %>% 
  select(age, step, area, M = Mtest) 


init_sd_data <- params_age %>% select(age, step, area ,sd)# %>% mutate(sd = 1)
mean_len_data <- params_age %>% select(age, step, area, L)
growth_data <- params_age %>% select(age, step, area, K = Kyr)

## Recruitment deviations
ycs <- rlnorm(256, 0, 0.6)

recdev <-
  data.frame(
    year = rep(year_range, each = 4),
    step = rep(1:4, length(year_range)),
    dev = c(rep(1, length(year_range)*4-256), ycs/mean(ycs))
  )
  
recdev$dev <- 1



## IF YOU WANT TO COMPARE THE G2 AND G3 MODELS, YOU WILL WANT TO USE THE FOLLOWING DATASETS
## AS M AND INIT.SD VARY BY YEAR BUT NOT STEP IN G2
#naturalmortality_data <- params_age[seq(1,29,by=4),] %>% select(age, area, M) 
#init_sd_data <- params_age[seq(1,29,by=4),] %>% select(age, area ,sd)

## INITIAL ABUNDANCE SET UP
init_param <- gadget3:::f_substitute(~R0 * (1-exp(-1*M))/(1-exp(-1*maxage*M)),
                                     list(R0 = g3_parameterized('carrycap', by_stock = FALSE),
                                          M = mean(naturalmortality_data$M),# g3_timeareadata('natmort', naturalmortality_data, value_field = 'M'),
                                          maxage = g3_stock_def(mat_stock, 'maxage')+1))

initial_conditions_imm_short <- 
  list(
    g3a_initialconditions_normalparam(imm_stock,
                                      factor_f = gadget3::g3a_renewal_initabund(scalar = g3_timeareadata('imm_prop', 
                                                                                                         maturity_init_data %>% 
                                                                                                           mutate(Mat = 1 - Mat), 
                                                                                                         value_field = 'Mat'),
                                                                                init = init_param,
                                                                                M = mean(naturalmortality_data$M)),# g3_timeareadata('natmort', naturalmortality_data, value_field = 'M')),
                                      mean_f = gadget3::g3_timeareadata('stock_agelen', mean_len_init_data, value_field = 'L'),
                                      stddev_f = g3_timeareadata('stock_init_sd', init_sd_init_data, value_field = 'sd'),
                                      alpha_f = wl_par$walpha,
                                      beta_f = wl_par$wbeta)
  )

initial_conditions_mat_short <- 
  list(
    g3a_initialconditions_normalparam(mat_stock,
                                      factor_f = gadget3::g3a_renewal_initabund(scalar = g3_timeareadata('mat_prop', 
                                                                                                         maturity_init_data, 
                                                                                                         value_field = 'Mat'),
                                                                                init = init_param,
                                                                                M = mean(naturalmortality_data$M)),# g3_timeareadata('stock_M', naturalmortality_data, value_field = 'M')),
                                      mean_f = gadget3::g3_timeareadata('stock_agelen', mean_len_init_data, value_field = 'L'),
                                      stddev_f = g3_timeareadata('stock_init_sd', init_sd_init_data, value_field = 'sd'),
                                      alpha_f = wl_par$walpha,
                                      beta_f = wl_par$wbeta)
  )


initial_renewal <-
  list(
    g3a_renewal_normalparam(imm_stock,
                          factor_f = gadget3:::f_substitute(~ (if (cur_year < 1952) x else y),
                                                  list(x = g3_formula(rr0, list(rr0 = params_bio$R0/10000)),
                                                       y = 0L)),
                          mean_f = g3_formula(~L0, list(L0 = params_age$L[1])),
                          stddev_f = g3_formula(~L0.sd, list(L0.sd = params_age$sd[1])),
                          alpha_f = wl_par$walpha,
                          beta_f = wl_par$wbeta,
                          run_f = gadget3:::f_substitute(~ age == minage,# && cur_year < 1952,
                                                         list(minage = g3_stock_def(imm_stock, 'stock__minage'))))
  )

initial_renewal_time0 <-
  list(
    g3a_renewal_normalparam(imm_stock,
                            factor_f = g3_formula(rr0, list(rr0 = params_bio$R0/10000)),
                            mean_f = g3_formula(~L0, list(L0 = params_age$L[1])),
                            stddev_f = g3_formula(~L0.sd, list(L0.sd = params_age$sd[1])),
                            alpha_f = wl_par$walpha,
                            beta_f = wl_par$wbeta,
                            run_f = gadget3:::f_substitute(~ age == minage,# && cur_year < 1952,
                                                           list(minage = g3_stock_def(imm_stock, 'stock__minage'))))
  )

initial_renewal2 <-
 list(
   g3a_initialconditions_normalparam(imm_stock,
                                     factor_f = gadget3:::f_substitute(~(if (age == 0) x else y), list(x = params_bio$R0/10000, y = 0L)),
                                     mean_f = gadget3::g3_timeareadata('stock_agelen', mean_len_init_data, value_field = 'L'),
                                     stddev_f = g3_timeareadata('stock_init_sd', init_sd_init_data, value_field = 'sd'),
                                     alpha_f = wl_par$walpha,
                                     beta_f = wl_par$wbeta))

initial_renewal_mat <- 
  list(
    g3a_initialconditions_normalparam(mat_stock,
                                      factor_f = ~0,
                                      mean_f = gadget3::g3_timeareadata('stock_agelen', mean_len_init_data, value_field = 'L'),
                                      stddev_f = g3_timeareadata('stock_init_sd', init_sd_init_data, value_field = 'sd'),
                                      alpha_f = wl_par$walpha,
                                      beta_f = wl_par$wbeta)
  )

initial_spawn_bevholt <- 
  list(
    g3a_spawn(mat_stock,
              recruitment_f = g3a_bevholt_casal(params_bio$R0,params_bio$R0, 1, 1),
              output_stocks = list(imm_stock),
              mean_f = gadget3::g3_timeareadata('imm_Len', mean_len_data, value_field = 'L'),
              stddev_f = gadget3::g3_timeareadata('imm_init_sd', init_sd_data, value_field = 'sd'),
              alpha_f = wl_par$walpha,
              beta_f = wl_par$wbeta,
              run_f = ~cur_year > 1932 && cur_year < 1952)
  )

load(file = 'MODEL/data/init_num.Rdata')

initial_abun_imm <- 
  list(
    g3a_initialconditions_normalparam(imm_stock, 
                                      factor_f = g3_timeareadata('initimm', 
                                                                 init_num %>% 
                                                                   filter(stock == 'imm') %>% 
                                                                   select(-stock) %>% 
                                                                   arrange(age), value_field = 'n'),
                                      mean_f = gadget3::g3_timeareadata('stock_agelen', mean_len_init_data, value_field = 'L'),
                                      stddev_f = g3_timeareadata('stock_init_sd', init_sd_init_data, value_field = 'sd'),
                                      alpha_f = wl_par$walpha,
                                      beta_f = wl_par$wbeta)
    )

initial_abun_mat <- 
  list(
    g3a_initialconditions_normalparam(mat_stock, 
                                      factor_f = g3_timeareadata('initmat', 
                                                                 init_num %>% 
                                                                   filter(stock == 'mat') %>% 
                                                                   select(-stock) %>% 
                                                                   arrange(age), value_field = 'n'),
                                      mean_f = gadget3::g3_timeareadata('stock_agelen', mean_len_init_data, value_field = 'L'),
                                      stddev_f = g3_timeareadata('stock_init_sd', init_sd_init_data, value_field = 'sd'),
                                      alpha_f = wl_par$walpha,
                                      beta_f = wl_par$wbeta)
  )
  

## -----------------------------------------------------------------------------
## SETUP ACTIONS
## -----------------------------------------------------------------------------

init_abun <- ~bounded(g3_param_table("yft.init", expand.grid(age = seq(0,7))), 0.001, 50)

## INITIAL CONDITIONS
initial_conditions_imm <- 
  list(
    g3a_initialconditions_normalparam(imm_stock,
                                      # factor_f = g3a_renewal_initabund(~1,
                                      #                                  init_abun,
                                      #                                  M = g3_timeareadata('stock_M', naturalmortality_data, value_field = 'M')),
                                      factor_f = gadgetutils::init_abund(imm = imm_stock,
                                                                         mat = mat_stock,
                                                                         comp_id = comp_id,
                                                                         mature = FALSE,
                                                                         init_mode = init_abund_mode,
                                                                         exp_init = exp_init,
                                                                         naturalmortality = g3_timeareadata('stock_M', naturalmortality_data, value_field = 'M')),
                                      # mean_f = g3a_renewal_vonb(Linf = grow_par$Linf, 
                                      #                           K = g3_timeareadata('KK', growth_data, value_field = 'K'),
                                      #                           recl = init_par$recl),# g3_timeareadata('stock_recl', recl_data, value_field = 'recl')),
                                      mean_f = gadget3::g3_timeareadata('stock_agelen', mean_len_init_data, value_field = 'L'),
                                      stddev_f = g3_timeareadata('stock_init_sd', init_sd_init_data, value_field = 'sd'),
                                      alpha_f = wl_par$walpha,
                                      beta_f = wl_par$wbeta)
    )

initial_conditions_mat <- 
  list(
    g3a_initialconditions_normalparam(mat_stock,
                                      # factor_f = g3a_renewal_initabund(~1,
                                      #                                  init_abun,
                                      #                                  M = g3_timeareadata('stock_M', naturalmortality_data, value_field = 'M')),
                                      factor_f = gadgetutils::init_abund(imm = imm_stock,
                                                                         mat = mat_stock,
                                                                         comp_id = comp_id,
                                                                         mature = TRUE,
                                                                         init_mode = init_abund_mode,
                                                                         exp_init = exp_init,
                                                                         naturalmortality = g3_timeareadata('stock_M', naturalmortality_data, value_field = 'M')),
                                      # mean_f = g3a_renewal_vonb(Linf = grow_par$Linf,
                                      #                           K = g3_timeareadata('KK', growth_data, value_field = 'K'),
                                      #                           recl = init_par$recl),# g3_timeareadata('stock_recl', recl_data, value_field = 'recl')),
                                      mean_f = gadget3::g3_timeareadata('stock_agelen', mean_len_init_data, value_field = 'L'),
                                      stddev_f = g3_timeareadata('stock_init_sd', init_sd_init_data, value_field = 'sd'),
                                      alpha_f = wl_par$walpha,
                                      beta_f = wl_par$wbeta)
  )



## -------------------------
## Natural mortality actions
## -------------------------

natural_mortality_imm <- 
  list(
    g3a_naturalmortality(imm_stock, 
                         g3a_naturalmortality_exp(g3_timeareadata('stock_M', naturalmortality_data, value_field = 'M')))
    )

natural_mortality_mat <- 
  list(
    g3a_naturalmortality(mat_stock, 
                         g3a_naturalmortality_exp(g3_timeareadata('stock_M', naturalmortality_data, value_field = 'M')))
    )

## ------------------
## Ageing actions
## ------------------

ageing_imm <- list(g3a_age(imm_stock, list(mat_stock)))
ageing_mat <- list(g3a_age(mat_stock, list()))

## ------------------
## Renewal actions
## ------------------

unbounded_init <- gadget3:::f_substitute(~x*y,
                                         list(x = ~g3_param('yft.rec.scalar'),
                                              y = ~g3_param_table('yft.rec', expand.grid(cur_year = seq(start_year, end_year),
                                                                                         cur_step = 1:4), ifmissing = NaN)))
# factor_f = ~100 * bounded(g3_param_table("yft.rec", expand.grid(
#   cur_year = seq(start_year, end_year),
#   cur_step = 1:4)), 0.001, 150),

# factor_f = g3_parameterized('rec',
#                             by_stock = list(imm_stock, mat_stock),
#                             by_year = TRUE,
#                             scale = g3_parameterized(name = 'rec.scalar',
#                                                      by_stock = list(imm_stock, mat_stock),
#                                                      exponentiate = FALSE,
#                                                      ),
#                             exponentiate = exp_rec,
#                             ifmissing = NaN),


all_renewal <-
  list(
    g3a_renewal_normalparam(imm_stock,
                            factor_f = unbounded_init,
                            mean_f = g3_formula(~L0, list(L0 = params_age$L[1])),
                            stddev_f = g3_formula(~L0.sd, list(L0.sd = params_age$sd[1])),
                            alpha_f = wl_par$walpha,
                            beta_f = wl_par$wbeta,
                            run_f = gadget3:::f_substitute(~ age == minage,
                                                           list(minage = g3_stock_def(imm_stock, 'stock__minage'))))
  )

renewal_imm <- 
  list(
    g3a_renewal_normalparam(imm_stock,
                            factor_f = unbounded_init,
                            mean_f = g3_formula(~L0, list(L0 = params_age$L[1])),
                            stddev_f = g3_formula(~L0.sd, list(L0.sd = params_age$sd[1])),
                            alpha_f = wl_par$walpha,
                            beta_f = wl_par$wbeta,
                            run_f = gadget3:::f_substitute(~ age == minage &&
                                                             #cur_step == 1 &&
                                                             cur_year > 1971 &&
                                                             !cur_year_projection,
                                                           list(minage = g3_stock_def(imm_stock, 'stock__minage'))))
    )


## ----------------------------
## Immature growth and maturity
## ----------------------------

growmature_imm <- 
  list(
    g3a_growmature(imm_stock,
                   impl_f = g3a_grow_impl_bbinom(
                     delta_len_f = g3a_grow_lengthvbsimple(linf_f = grow_par$Linf,
                                                          kappa_f = g3_timeareadata('KK', growth_data, value_field = 'K')),
                                                          #kappa_f = g3_parameterized('K', by_stock = list(imm_stock, mat_stock), by_age = TRUE)),
                     delta_wgt_f = g3a_grow_weightsimple(alpha_f = wl_par$walpha,
                                                         beta_f = wl_par$wbeta),
                     beta_f = grow_par$bbin,
                     maxlengthgroupgrowth = grow_par$mlgg),
                   maturity_f = g3a_mature_continuous(alpha = mat_par$mat_alpha,
                                                      l50 = mat_par$mat_l50),
                   output_stocks = list(mat_stock),
                   transition_f = ~TRUE)
    )

growmature_imm_constant <- 
  list(
    g3a_growmature(imm_stock,
                   impl_f = g3a_grow_impl_bbinom(
                     delta_len_f = g3a_grow_lengthvbsimple(linf_f = grow_par$Linf,
                                                           kappa_f = g3_timeareadata('KK', growth_data, value_field = 'K')),
                                                           #kappa_f = g3_parameterized('K', by_stock = list(imm_stock, mat_stock), by_age = TRUE)),
                     delta_wgt_f = g3a_grow_weightsimple(alpha_f = wl_par$walpha,
                                                         beta_f = wl_par$wbeta),
                     beta_f = grow_par$bbin,
                     maxlengthgroupgrowth = grow_par$mlgg),
                   maturity_f = g3a_mature_constant(#beta = mat_par$mat_beta,
                                                    #a50 = mat_par$mat_a50),
                                                  alpha = mat_par$mat_alpha,
                                                  l50 = mat_par$mat_l50),
                   output_stocks = list(mat_stock),
                   transition_f = ~TRUE)
  )



grow_imm <- 
  list(
    g3a_growmature(imm_stock,
                   impl_f = g3a_grow_impl_bbinom(
                     delta_len_f = g3a_grow_lengthvbsimple(linf_f = grow_par$Linf,
                                                           kappa_f = g3_timeareadata('KK', growth_data, value_field = 'K')),
                     delta_wgt_f = g3a_grow_weightsimple(alpha_f = wl_par$walpha,
                                                         beta_f = wl_par$wbeta),
                     beta_f = grow_par$bbin,
                     maxlengthgroupgrowth = grow_par$mlgg))
    )

grow_mat <- 
  list(
    g3a_growmature(mat_stock,
                   impl_f = g3a_grow_impl_bbinom(
                     delta_len_f = g3a_grow_lengthvbsimple(linf_f = grow_par$Linf,
                                                           kappa_f = g3_timeareadata('KK', growth_data, value_field = 'K')),
                     delta_wgt_f = g3a_grow_weightsimple(alpha_f = wl_par$walpha,
                                                         beta_f = wl_par$wbeta),
                     beta_f = grow_par$bbin,
                     maxlengthgroupgrowth = grow_par$mlgg))
  )

## ----------------------------------------------------------------------------


spawn_bevholt_simple <- 
  list(
    g3a_spawn(mat_stock,
              recruitment_f = g3a_spawn_recruitment_bevertonholt(mu = g3_parameterized('R0', scale = 10e3),
                                                                 lambda = g3_parameterized('Lambda', by_year = TRUE)),
              output_stocks = list(imm_stock),
              mean_f = gadget3::g3_timeareadata('imm_Len', mean_len_data, value_field = 'L'),
              stddev_f = gadget3::g3_timeareadata('imm_init_sd', init_sd_data, value_field = 'sd'),
              alpha_f = wl_par$walpha,
              beta_f = wl_par$wbeta,
              run_f = ~cur_year > 1951)
  )


spawn_bevholt <- 
  list(
    g3a_spawn(mat_stock,
              recruitment_f = g3a_bevholt_casal(R0 = g3_parameterized('R0', by_year = TRUE),
                                                B0 = g3_parameterized('B0'),
                                                h = g3_parameterized('srr_h'),
                                                ysc = g3_timeareadata('recdev', recdev, value_field = 'dev')),
              output_stocks = list(imm_stock),
              mean_f = g3_formula(~L0, list(L0 = params_age$L[1])),
              stddev_f = g3_formula(~L0.sd, list(L0.sd = params_age$sd[1])),
              alpha_f = wl_par$walpha,
              beta_f = wl_par$wbeta,
              run_f = ~cur_time > 0)
    )

spawn_bevholt2 <- 
  list(
    g3a_spawn(mat_stock,
              recruitment_f = g3a_bevholt_b0(g3_parameterized('srr_h'),
                                             g3_parameterized('R0', scale = 10e3),
                                             g3_parameterized('B0', scale = 10e3)),
              output_stocks = list(imm_stock),
              mean_f = gadget3::g3_timeareadata('imm_Len', mean_len_data, value_field = 'L'),
              stddev_f = gadget3::g3_timeareadata('imm_init_sd', init_sd_data, value_field = 'sd'),
              alpha_f = wl_par$walpha,
              beta_f = wl_par$wbeta,
              run_f = ~cur_year > 1951)
  )


## EXPERIMENT
## Using a spawning action to mimic a fixed maturity ogive
## This should be a proportion moving per age group so will not work with simplessb


spawn_as_maturity <- list(g3a_spawn(imm_stock,
                                    recruitment_f = g3a_spawn_recruitment_simplessb(1),
                                    output_stocks = list(mat_stock),
                                    proportion_f = g3_timeareadata('mat_ogive', params_age[,c('age', 'step', 'area', 'Mat')], value_field = 'Mat'),
                                    mean_f = g3a_renewal_vonb(by_stock = 'species'),
                                    stddev_f = g3_timeareadata('imm_init_sd', init_sd_data, value_field = 'sd'),
                                    alpha_f = g3_parameterized('walpha', by_stock = 'species'),
                                    beta_f = g3_parameterized('wbeta', by_stock = 'species'),
                                    run_at = 5,
                                    recruit_at = 7)
)



# spawning <- list(g3a_spawn(mat_stock,
#                            recruitment_f = g3a_spawn_recruitment_ricker(
#                              ~g3_param("ricker.mu"),
#                              ~g3_param("ricker.lambda")),
#                            #proportion_f = g3_suitability_exponentiall50(alpha = ~-g3_param("spawn.prop.alpha"), l50 =  ~g3_param("spawn.prop.l50")),
#                            #mortality_f = g3_suitability_straightline(alpha = ~g3_param("spawn.mort.alpha"), beta =  ~g3_param("spawn.mort.beta")),
#                            #weightloss_f = ~g3_param("spawn.weightloss"),
#                            output_stocks = list(imm_stock),
#                            mean_f = 50,
#                            stddev_f = 0.9,
#                            alpha_f = 1,
#                            beta_f = 1,
#                            run_f = ~cur_step==1))

# spawning <- list(g3a_spawn(mat_stock,
#                            recruitment_f = g3a_spawn_recruitment_simplessb(mu = g3_parameterized('spawn.mu', by_stock = 'species')),
#                            #  recruitment_f = g3a_spawn_recruitment_hockeystick(r0 = 1e8,
#                            #                                                   blim = g3_parameterized(name = 'blim',
#                            #                                                                           by_stock = FALSE,
#                            #                                                                           scale = 1,
#                            #                                                                           value = 100)),
#                            
#                            output_stocks = list(imm_stock),
#                            mean_f = g3a_renewal_vonb(by_stock = 'species'),
#                            stddev_f = g3_parameterized('rec.sd', by_stock = 'species'),
#                            alpha_f = g3_parameterized('walpha', by_stock = 'species'),
#                            beta_f = g3_parameterized('wbeta', by_stock = 'species'),
#                            run_f = ~cur_step == 1 && cur_year_projection))

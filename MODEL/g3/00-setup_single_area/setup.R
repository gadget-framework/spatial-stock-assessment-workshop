## -----------------------------------------------------------------------------
##
## Runner to build a single area YFT model
##
## -----------------------------------------------------------------------------

## See https://github.com/aaronmberger-nwfsc/Spatial-Assessment-Modeling-Workshop
## for information about the project, and vignettes for information regarding the 
## operating model 

library(mfdb)
library(gadget3)
library(tidyverse)
library(gadgetutils)
library(gadgetplots)

source("~/gadget-framework/spatial-stock-assessment-workshop/MODEL/g3/src/spawn_helpers.R")

## Model directory
base_dir <- 'MODEL/g3'

## Model version
vers <- 'models/TAGGING_TEST'

include_tagging <- TRUE

## -----------------------------------------------------------------------------
## OPTIONS 
## -----------------------------------------------------------------------------

## If FALSE, all data files will be read from "../data"
## If TRUE, the mfdb will be queried and data files written to "../data"
read_data <- TRUE

## Whether or not to run iterative re-weighting
run_iterative <- FALSE
run_retro <- FALSE
bootstrap <- FALSE

## -----------------------------------------------------------------------------
## PARAMETERS 
## -----------------------------------------------------------------------------

## This setup script uses the real years as opposed to pseudoyears
year_range <- 1940:2015
species_name <- 'yft' 

## -----------------------------------------------------------------------------

## Leave as '' for the single area model
data_source_suffix <- ''

defaults <- list(area = mfdb_group('1' = 1),
                 timestep = mfdb_timestep_quarterly,
                 year = year_range,
                 species = 'YFT',
                 sampling_type = 'sim_4') ## sim_4 is the baseline

# Map area names to integer area numbers (in this case only "1" ==> 1, but could be anything)
areas <- structure(seq_along(defaults$area), names = names(defaults$area))

# Timekeeping for the model, i.e. how long we run for
time_actions <- list(g3a_time(start_year = min(defaults$year), 
                              end_year = max(defaults$year),
                              defaults$timestep),
                     list())

## Data and model folders
fs::dir_create(file.path(base_dir, vers, 'OPT', 'figs'))
fs::dir_create(file.path(base_dir, vers, 'WGTS', 'figs'))
fs::dir_create(file.path('MODEL/data'))

## ------------------------------------------------------------------------------------

# Sets up the stock objects and generates the stock actions
source(file.path(base_dir, '00-setup_single_area', 'setup-stocks.R')) 

## Load data objects ----------------------------------------------------------
if(read_data){
  mdb <- mfdb("MFDB/noaa_spatial.duckdb")
  source(file.path(base_dir, '00-setup_single_area', 'setup-fleet-data.R'))
  source(file.path(base_dir, '00-setup_single_area', 'setup-catchdistribution.R'))
  source(file.path(base_dir, '00-setup_single_area', 'setup-indices.R'))
  source(file.path(base_dir, '00-setup_single_area', 'setup-initial_parameters.R'))
  mfdb_disconnect(mdb)
} else {
  fs::dir_ls(file.path('MODEL', 'data')) %>%
    stringr::str_subset('.Rdata') %>%
    lapply(load,.GlobalEnv)
}

## Configure model actions ------------------------------------------------------------

## Sets up the stock processes, initial conditions, growth, renewal etc
source(file.path(base_dir, '00-setup_single_area', 'setup-model.R'))  
## Sets up the fleet objects and processes 
source(file.path(base_dir, '00-setup_single_area', 'setup-fleets_andersen2.R'))  # Generates fleet_actions

#source(file.path(base_dir, '00-setup_single_area', 'setup-fleets.R'))  # Generates fleet_actions
if (include_tagging) source(file.path(base_dir, '00-setup_single_area', 'setup-tagging.R'))  # Generates fleet_actions
#source(file.path(base_dir, '00-setup_single_area', 'setup-fleets.R'))  # Generates fleet_actions
source(file.path(base_dir, '00-setup_single_area', 'setup-likelihood.R'))  # Generates likelihood_actions
#source(file.path(base_dir, '00-setup_single_area', 'setup-likelihood_multi.R'))  # Generates likelihood_actions
#source(file.path(base_dir, '00-setup_single_area', 'setup-likelihood_survey.R'))  # Generates likelihood_actions

##### Compile the r- and tmb-based models ######################################

## Collate the stock actions
stock_actions <- c(#all_renewal,
                   #initial_renewal,
                   #initial_abun_imm,
                   initial_conditions_imm,
                   #initial_conditions_imm_short,
                   #initial_renewal,
                   natural_mortality_imm,
                   renewal_imm,
                   #growmature_imm,
                   growmature_imm_constant,
                   #grow_imm,
                   #spawn_as_maturity,
                   ageing_imm,
                   list(),
                   #initial_renewal_mat,
                   #initial_spawn_bevholt,
                   #initial_abun_mat,
                   initial_conditions_mat,
                   #initial_renewal_mat,
                   #initial_conditions_mat_short,
                   natural_mortality_mat,
                   grow_mat,
                   #initial_spawn_bevholt,
                   #spawn_bevholt,
                   ageing_mat,
                   list())

## Collate model actions
actions <- c(
  stock_actions,
  survey_actions,
  fleet_actions,
  likelihood_actions,
  time_actions
)

if (include_tagging){
  actions <- c(actions, tagging_actions_flat, list())
}

## It is possible to add reporting to the model, e.g.
#actions <- c(actions, list(
#  g3a_report_history(actions, '^yft_(imm|mat)__(num|wgt|igfs|lln|bmt|gil|foreign|suit_igfs|renewalnum|renewalwgt|spawnednum)$')))

# Turn actions into an R function
model <- g3_to_r(actions)#, strict = TRUE, trace = TRUE)

# Turn actions into C++ objective function code
tmb_model <- g3_to_tmb(actions)

# Get the parameter template 
tmb_param <- attr(tmb_model, 'parameter_template')

## Fill in the parameter template
## Fill in the parameter template
tmb_param <- 
  tmb_param %>% 
  g3_init_guess('\\.rec', 100, 0.001, 200, 1) %>% 
  g3_init_guess('\\.init', 100, 0.001, 200, 1) %>% 
  g3_init_guess('recl', 22, 5, 30, 0) %>% 
  g3_init_guess('rec.sd', 6, 0, 20, 1) %>% 
  g3_init_guess('rec.scalar', 1e2, 1e1, 1e3, 0) %>% 
  g3_init_guess('\\.rec.sigma', 0.2, -1, 1, 0) %>% 
  g3_init_guess('init.scalar', 1, 1, 10, 0) %>% 
  g3_init_guess('Linf', params_bio$Linf, params_bio$Linf*0.75, params_bio$Linf*1.25, 0) %>% 
  g3_init_guess('\\.K', 0.455, 0.001, 2, 1) %>% #params_bio$Kbase*1e3, params_bio$Kbase*1e3, params_bio$Kbase*1e3, 1) %>%
  # g3_init_guess('bbin', 100, 1e-08, 101, 0) %>% 
  g3_init_guess('\\.alpha', 0.5, 0.005, 3, 1) %>% 
  g3_init_guess('l50', params_bio$mat.l50, 0.75*params_bio$mat.l50, 1.25*params_bio$mat.l50, 1) %>% 
  g3_init_guess('init.F', 0, 0, 1, 0) %>% 
  g3_init_guess('\\.M', 0.15, 0.001, 1, 0) %>%  
  g3_init_guess('prop_mat0', 0.5, 0.1, 0.9, 1) %>%
  # g3_init_guess('B0', 9725, 8000, 10000, 1) %>%
  g3_init_guess('mat_alpha', 95, 10, 110, 0) %>% # g3_init_guess('mat1', 0, 10, 200, 1) %>% 
  g3_init_guess('mat_l50', params_bio$mat.l50, 0.75*params_bio$mat.l50, 1.25*params_bio$mat.l50, 0) %>%  #mat.l50$l50, 0.001,1000,1) %>% #
  g3_init_guess('mat_beta', 3.5, 0.1, 4, 0) %>% # g3_init_guess('mat1', 0, 10, 200, 1) %>% 
  g3_init_guess('mat_a50', 2, 1, 3, 0) %>%  #mat.l50$l50, 0.001,1000,1) %>% #
  g3_init_guess('walpha', params_bio$walpha, 1e-10, 1, 0) %>% 
  g3_init_guess('wbeta', params_bio$wbeta, 2, 4, 0) %>% 
  g3_init_guess('ander.p0$', 0, -1, 1, 0) %>%
  g3_init_guess('ander.p2$', 1, 0, 1,0) %>% 
  g3_init_guess('ander.p5$', max(g3_stock_def(mat_stock, 'minlen')), 10, 200, 0) %>% 
  g3_init_guess('\\.p1', 1, 0.000001, 2, 1) %>% 
  g3_init_guess('\\.p3', 1, 0.000001, 2, 1) %>% 
  g3_init_guess('\\.p4', 1, 0.000001, 2, 1) %>% 
  g3_init_guess('\\.Lmode$', 
                max(g3_stock_def(mat_stock, 'minlen'))/2, 
                max(g3_stock_def(mat_stock, 'minlen'))/2.5, 
                max(g3_stock_def(mat_stock, 'minlen'))/1.5, 1) %>% 
  #  g3_init_guess('bh_mu', 2, 0, 100, 1) %>% ## Maximum recruitment R0
  #  g3_init_guess('bh_lambda', 2, 0, 100, 1) %>% 
  g3_init_guess('srr_h', 0.8, 0, 1, 0) %>% 
  g3_init_guess('R0', params_bio$R0, params_bio$R0*0.75, params_bio$R0*1.25, 0) %>% 
  g3_init_guess('B0', 5773659420, 5773659420*0.9, 5773659420*1.1, 0) %>% 
#  g3_init_guess('Lambda', 5000000/10e3, (5000000/10e3)*0.9, (5000000/10e3)*1.1, 1) %>% 
  g3_init_guess('catchability', 10,0.001,1e3,1)

## --------------------------------------------------------------------------

## R0 to zero when not spawning
#tmb_param$value[paste('R0', c(min(year_range):1951,1972:2015), sep='.')] <- 0L
#tmb_param$value[paste('R0', c(min(year_range):2015), sep='.')] <- 0L

## Init rec to R0 prior to 1951
#tmb_param$value[paste('yft.rec', rep(min(year_range):1951, each=4), rep(1:4, length(min(year_range):1951)), sep='.')] <- params_bio$R0/10000/1e2
#tmb_param[match(paste('yft.rec', rep(min(year_range):1951, each=4), rep(1:4, length(min(year_range):1951)), sep='.'), tmb_param$switch), 'optimise'] <- FALSE

#tmb_param$value[paste('yft.rec', rep(1952:1971, each=4), rep(1:4, length(1952:1971)), sep='.')] <- 0L
#tmb_param[match(paste('yft.rec', rep(1952:1971, each=4), rep(1:4, length(1952:1971)), sep='.'), tmb_param$switch), 'optimise'] <- FALSE




## Run the R-model
result <- model(tmb_param$value)
print(result[[1]])
# 
#test <- g3_fit(tmb_model, tmb_param)
#test$stock.full %>% mutate(t = number*mean_weight) %>% group_by(year, step, stock) %>% summarise(t = sum(t))  %>% mutate(t = t/1000000)
# 
# plot(test)
# 
#tmp <- attributes(result)
#(tmp$hist_yft_mat__num* tmp$hist_yft_mat__wgt) %>% as.data.frame.table() %>% group_by(time) %>% summarise(t = sum(Freq)) %>% view

#tmp$hist_yft_imm__renewalnum %>% as.data.frame.table() %>% group_by(time) %>% summarise(s = sum(Freq)) %>% view()
# # 
#  (tmp$yft_imm__num) %>% as.data.frame.table() %>% group_by(age) %>% summarise(biomass  = sum(Freq)) %>% mutate(stock = 'imm') %>% bind_rows(
#    (tmp$yft_mat__num) %>% as.data.frame.table() %>% group_by(age) %>% summarise(biomass = sum(Freq)) %>% mutate(stock = 'mat')) -> qq
# # 
# sum(print(qq$biomass))
# print(params_bio$R0)
# 
# (tmp$yft_imm__num * tmp$yft_imm__wgt) %>% as.data.frame.table() %>% group_by(age) %>% summarise(biomass  = sum(Freq)) %>% mutate(stock = 'imm') %>% bind_rows(
#   (tmp$yft_mat__num * tmp$yft_mat__wgt) %>% as.data.frame.table() %>% group_by(age) %>% summarise(biomass = sum(Freq)) %>% mutate(stock = 'mat')) -> ww
# 
# print(sum(ww$biomass[4:10]))

# load(file = 'result0.Rdata')
# tmp2 <- attributes(result0)
# 
# tmp2$yft_mat__num %>% as.data.frame.table() %>% bind_cols(tmp$yft_mat__num %>% as.data.frame.table())
# tmp2$yft_imm__num %>% as.data.frame.table() %>% bind_cols(tmp$yft_imm__num %>% as.data.frame.table())

#(attr(result, 'yft_mat__num') %>% sum()) + (attr(result, 'yft_imm__num') %>% sum())
#tmp <- attributes(result)
#qq <- tmp$hist_yft_imm__num %>% as.data.frame.table() %>% group_by(time) %>% summarise(f = sum(Freq))
#ww <- tmp$hist_yft_mat__num %>% as.data.frame.table() %>% group_by(time) %>% summarise(f = sum(Freq))

# (tmp$hist_yft_mat__num*tmp$hist_yft_mat__wgt) %>% 
#   as.data.frame.table() %>% 
#   separate(time, into = c('year', 'step')) %>% 
#   filter(year > 1945) %>% 
#   mutate(time = as.numeric(year) + (as.numeric(step)-1)/4) -> qq
# 
# qq %>% group_by(time) %>% summarise(ssb = sum(Freq)) %>% ggplot(aes(time, ssb)) + geom_line()



#print(qq$f + ww$f)

# List all available reports
print(names(attributes(result)))

## Write out parameters and both models
save(tmb_param, file = file.path(base_dir, vers, 'tmb_param.Rdata'))
save(tmb_model, file = file.path(base_dir, vers, 'tmb_model.Rdata'))

## -----------------------------------------------------------------------------


#if (!run_iterative){
  
  ## Run optimisation without iterative re-weighting
  params_opt <- g3_optim(tmb_model, 
                         tmb_param,
                         use_parscale = TRUE,
                         control = list(#reltol = 1e-10,
                                        maxit = 1000),
                         print_status = TRUE)
  

source("~/gadget-framework/spatial-stock-assessment-workshop/MODEL/g3/g3_fit2.R")

  fit <- g3_fit2(tmb_model, params_opt)
  ## Write to file
  write.g3.param(params_opt, file.path(base_dir, vers, 'OPT'), 'params.final.optim')
  save(params_opt, file = file.path(base_dir, vers, 'OPT', 'params_opt.Rdata'))
  save(fit, file = file.path(base_dir, vers, 'OPT', 'fit.Rdata'))
  gadget_plots(fit, file.path(base_dir, vers, 'OPT/figs'), file_type = 'html')
  
  
#}else{
  
  ## Run iterative re-weighting
  params_out <- g3_iterative(file.path(base_dir, vers),
                             wgts = 'WGTS',
                             tmb_model,
                             tmb_param,
                             grouping = list(),
                             use_parscale = TRUE,
                             control = list(#reltol = 1e-10,
                                            maxit = 1000))
  
  ## Get the model fit
  fit <- g3_fit2(tmb_model, params_out)
  save(fit, file = file.path(base_dir, vers, 'WGTS/fit.Rdata'))
  gadget_plots(fit, file.path(base_dir, vers, 'WGTS/figs'), file_type = 'html')
  
  
  ## Run the retro
  if (run_retro){
    retro <- g3_retro(file.path(base_dir, vers),
                      tmb_model,
                      params.out,
                      num.years = 10)
  }
#}

## Iterative re-weighting step-by-step
if (FALSE){
  res1 <-  g3_lik_out(model, tmb_param) 
  res2 <-  g3_iterative_setup(res1, grouping = list())
  res3 <-  parallel::mclapply(res2$params, function(x) g3_iterative_run(x, tmb_model), mc.cores = parallel::detectCores())
  res4 <-  parallel::mclapply(res3, function(x) g3_lik_out(model,x), mc.cores = parallel::detectCores())
  res5 <-  g3_iterative_final(res4)
  res6 <-  parallel::mclapply(res5, function(x) g3_iterative_run(x, tmb_model), mc.cores = parallel::detectCores())
  res7 <-  parallel::mclapply(res6, function(x) g3_lik_out(model,x), mc.cores = parallel::detectCores())
  
  lapply(res6, function(x) attr(x, 'summary')) %>% dplyr::bind_rows(.id = 'group')
  
}

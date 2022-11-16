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

## Model directory
base_dir <- 'MODEL/g3'

## Model version
vers <- 'models/TEST'

## -----------------------------------------------------------------------------
## OPTIONS 
## -----------------------------------------------------------------------------

## If FALSE, all data files will be read from "../data"
## If TRUE, the mfdb will be queried and data files written to "../data"
read_data <- FALSE

## Whether or not to run iterative re-weighting
run_iterative <- FALSE
run_retro <- FALSE
bootstrap <- FALSE

## -----------------------------------------------------------------------------
## PARAMETERS 
## -----------------------------------------------------------------------------

## This setup script uses the real years as opposed to pseudoyears
year_range <- 1952:2015
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

fs::dir_create(file.path(base_dir, 'QQ', c('jkjkj', 'popop')))

## Data and model folders
fs::dir_create(file.path(base_dir, vers, 'OPT', 'figs'))
fs::dir_create(file.path(base_dir, vers, 'WGTS', 'figs'))
fs::dir_create(file.path('MODEL/data'))

## ------------------------------------------------------------------------------------

# Sets up the stock objects and generates the stock actions
source(file.path(base_dir, '00-setup_single_area', 'setup-model.R'))  

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

source(file.path(base_dir, '00-setup_single_area', 'setup-fleets.R'))  # Generates fleet_actions
source(file.path(base_dir, '00-setup_single_area', 'setup-likelihood.R'))  # Generates likelihood_actions

##### Compile the r- and tmb-based models ######################################

## Collate the stock actions
stock_actions <- c(initial_conditions_imm,
                   natural_mortality_imm,
                   ageing_imm,
                   renewal_imm,
                   growmature_imm,
                   list(),
                   initial_conditions_mat,
                   natural_mortality_mat,
                   ageing_mat,
                   growmature_mat,
                   #spawning,
                   list())

## Collate model actions
actions <- c(
  stock_actions,
  survey_actions,
  fleet_actions,
  likelihood_actions,
  time_actions
)

## It is possible to add reporting to the model, e.g.
#actions <- c(actions, list(
#  g3a_report_history(actions, '^yft_(imm|mat)__(num|wgt|igfs|lln|bmt|gil|foreign|suit_igfs|renewalnum|renewalwgt)$')))

# Turn actions into an R function
model <- g3_to_r(actions)#, strict = TRUE, trace = TRUE)

# Turn actions into C++ objective function code
tmb_model <- g3_to_tmb(actions)

# Get the parameter template 
tmb_param <- attr(tmb_model, 'parameter_template')

## Fill in the parameter template
tmb_param <- 
  tmb_param %>% 
  g3_init_guess('\\.rec',1, 0.001, 1000, 1) %>% 
  g3_init_guess('\\.init', 1, 0.001, 1000, 1) %>% 
  g3_init_guess('recl', 5, 1, 30, 1) %>% 
  g3_init_guess('rec.sd', 5, 4, 20, 1) %>% 
  g3_init_guess('rec.scalar', 1, 1, 500, 1) %>% 
  g3_init_guess('\\.rec.sigma', 0.2, -1, 1, 0) %>% 
  g3_init_guess('init.scalar', 1, 1, 500, 1) %>% 
  g3_init_guess('Linf', 140, 100, 200, 1) %>% 
  g3_init_guess('\\.K', 90, 40, 200, 1) %>%
  g3_init_guess('bbin', 6, 1e-08, 100, 1) %>% 
  g3_init_guess('\\.alpha', 0.5, 0.01, 3, 1) %>% 
  g3_init_guess('l50', 50, 10, 100, 1) %>% 
  g3_init_guess('init.F', 0, 0.1, 1, 0) %>% 
  g3_init_guess('\\.M', 0.15, 0.001, 1, 0) %>%  
  g3_init_guess('prop_mat0', 0.5, 0.1, 0.9, 0) %>%
  g3_init_guess('B0', 100, 1, 5000, 1) %>%
  g3_init_guess('mat_alpha', 70, 10, 200, 1) %>% # g3_init_guess('mat1', 0, 10, 200, 1) %>% 
  g3_init_guess('mat_l50', params_bio$mat.l50, 0.75*params_bio$mat.l50, 1.25*params_bio$mat.l50, 1) %>%  #mat.l50$l50, 0.001,1000,1) %>% #
  g3_init_guess('walpha', params_bio$walpha, 1e-10, 1, 0) %>% 
  g3_init_guess('wbeta', params_bio$wbeta, 2, 4, 0) %>% 
  g3_init_guess('p0', 0.5,0,1,1) %>% 
  g3_init_guess('p1', 0.5,0,1,1) %>% 
  g3_init_guess('p2', 0.5,0,1,1) %>% 
  g3_init_guess('p3', 50,0.01,100,1) %>% 
  g3_init_guess('p4', 50,0.01,100,1) %>% 
  g3_init_guess('LP', 100,10,300,1) %>% 
  g3_init_guess('init.sd', 1, 1, 1, 0)

## Add age-varying natural mortality
## The operating model has natural mortality rates that vary per pseudoyear (i.e. per step)
## We therefore use the mean value for each calender year

m_by_age <- params_age %>% 
  mutate(age = floor(age_in_years)) %>% 
  group_by(age) %>% 
  summarise(M = mean(M)) %>% 
  pull(M)

tmb_param[grepl('\\imm.M.[0-9]', tmb_param$switch), 'value'] <- m_by_age[1:4]
tmb_param[grepl('\\mat.M.[0-9]', tmb_param$switch), 'value'] <- m_by_age[2:8]  

## --------------------------------------------------------------------------

## Run the R-model
result <- model(tmb_param$value)
result[[1]]
attr(result, 'yft_imm__num')

# List all available reports
print(names(attributes(result)))

## Write out parameters and both models
save(tmb_param, file = file.path(base_dir, vers, 'tmb_param.Rdata'))
save(tmb_model, file = file.path(base_dir, vers, 'tmb_model.Rdata'))

## -----------------------------------------------------------------------------

if (!run_iterative){
  
  ## Run optimisation without iterative re-weighting
  params_opt <- g3_optim(tmb_model, 
                         tmb_param,
                         use_parscale = TRUE,
                         control = list(reltol = 1e-10,
                                        maxit = 1000),
                         print_status = TRUE)
  
  ## Gather fit
  fit <- g3_fit(tmb_model, params_opt)
  
  ## Write to file
  write.g3.param(params_opt, file.path(base_dir, vers, 'OPT'), 'params.final.optim')
  save(params_opt, file = file.path(base_dir, vers, 'OPT', 'params_opt.Rdata'))
  save(fit, file = file.path(base_dir, vers, 'OPT', 'fit.Rdata'))
  gadget_plots(fit, file.path(base_dir, vers, 'OPT/figs'), file_type = 'html')
  
  
}else{
  
  ## Run iterative re-weighting
  params_out <- g3_iterative(file.path(base_dir, vers),
                             wgts = 'WGTS',
                             tmb_model,
                             tmb_param,
                             grouping = list(),
                             use_parscale = TRUE,
                             control = list(reltol = 1e-10,
                                            maxit = 1000))
  
  ## Get the model fit
  fit <- g3_fit(tmb_model, params_out)
  save(fit, file = file.path(base_dir, vers, 'WGTS/fit.Rdata'))
  gadget_plots(fit, file.path(base_dir, vers, 'WGTS/figs'), file_type = 'html')
  
  
  ## Run the retro
  if (run_retro){
    retro <- g3_retro(file.path(base_dir, vers),
                      tmb_model,
                      params.out,
                      num.years = 10)
  }
}

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

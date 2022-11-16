## -----------------------------------------------------------------------------
##
## Runner to build a Gadget3 model for cod
##
## Trying to replicate Taylor et als model (2007)
##
## -----------------------------------------------------------------------------

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

## Whether or not to call the setup-data scripts
read_data <- FALSE

## Whether or not to run iterative reweighting
run_iterative <- FALSE
run_retro <- FALSE
bootstrap <- FALSE

## -----------------------------------------------------------------------------
## PARAMETERS 
## -----------------------------------------------------------------------------

year_range <- 1952:2015
species_name <- 'yft' 

## -----------------------------------------------------------------------------

data_source_suffix <- ''

defaults <- list(area = mfdb_group('1' = 1),
                 timestep = mfdb_timestep_quarterly,
                 year = year_range,
                 species = 'YFT',
                 sampling_type = 'sim_4') ## Sim 4 is the baseline

# Map area names to integer area numbers (in this case only "1" ==> 1, but could be anything)
areas <- structure(
  seq_along(defaults$area),
  names = names(defaults$area))

# Timekeeping for the model, i.e. how long we run for
time_actions <- list(
  g3a_time(start_year = min(defaults$year), 
           end_year = max(defaults$year),
           defaults$timestep),
  list())

## Data and model folders
fs::dir_create(file.path(base_dir, c('models', vers)))
fs::dir_create(file.path('MODEL/data'))

## ------------------------------------------------------------------------------------

source(file.path(base_dir, '00-setup_single_area', 'setup-model.R'))  # Generates mat_stock_actions / imm_stock_actions

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
source(file.path(base_dir, '00-setup_single_area', 'setup-likelihood.R'))  # Generates ling_likelihood_actions

##### Compile the r- and tmb-based models ######################################

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

## Collate actions
actions <- c(
  stock_actions,
  survey_actions,
  fleet_actions,
  likelihood_actions,
  time_actions
)

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
## Take the average per 4 pseudoyears for each year

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
save(model, file = file.path(base_dir, vers, 'r_model.Rdata'))
save(tmb_model, file = file.path(base_dir, vers, 'tmb_model.Rdata'))

## -----------------------------------------------------------------------------

if (!run_iterative){
  
  obj_fun <- gadget3::g3_tmb_adfun(tmb_model, tmb_param)
  
  ## Run optim
  params_opt <- g3_optim(tmb_model, 
                         tmb_param,
                         control = list(reltol = 1e-10))
  
  ## Gather fit
  fit <- g3_fit(tmb_model, params_opt)
  gadget_plots(fit, file.path(base_dir, vers, 'figs'), file_type = 'html')
  
  
}else{
  
  ## Run iterative re-weighting
  params.out <- g3_iterative(file.path(base_dir, vers),
                             wgts = 'WGTS',
                             model,
                             tmb_model,
                             tmb_param,
                             grouping = list(igfs_si = c('log_si_igfs_si1',
                                                         'log_si_igfs_si2',
                                                         'log_si_igfs_si3'),
                                             aut_si = c('log_si_aut_si1',
                                                        'log_si_aut_si2',
                                                        'log_si_aut_si3')),
                             opt_method = 'BFGS',
                             use_parscale = TRUE)
  
  
  ## Get the model fit
  fit <- gadget3:::g3_fit(model, params.out)
  save(fit, file = file.path(base_dir, vers, 'fit.Rdata'))
  
  ## Plot gadget2 vs gadget3 comparisons, and get all model plots
  gadget_plots(fit, file.path(base_dir, vers, 'figs'))
  #source(file.path(base_dir, "src/g3_vs_assessmentmodel_graphs.R"))
  
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

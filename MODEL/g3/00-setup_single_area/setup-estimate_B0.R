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

source("~/gadget-framework/spatial-stock-assessment-workshop/MODEL/g3/spawn_helpers.R")


## Model directory
base_dir <- 'MODEL/g3'

## Model version
vers <- 'models/00-B0_estimation'

include_tagging <- FALSE

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
year_range <- 1930:1952
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

source(file.path(base_dir, '00-setup_single_area', 'setup-fleets_andersen2.R'))  # Generates fleet_actions

##### Compile the r- and tmb-based models ######################################

## Collate the stock actions
stock_actions <- c(initial_renewal_time0,
                   natural_mortality_imm,
                   growmature_imm_constant,
                   ageing_imm,
                   list(),
                   initial_renewal_mat,
                   natural_mortality_mat,
                   grow_mat,
                   #initial_spawn_bevholt,
                   ageing_mat)

## Collate model actions
actions <- c(
  stock_actions,
#  survey_actions,
#  fleet_actions,
#  likelihood_actions,
  time_actions
)

## It is possible to add reporting to the model, e.g.
actions <- c(actions, list(
  g3a_report_history(actions, '^yft_(mat|imm)__(num|wgt|igfs|lln|bmt|gil|foreign|suit_igfs|renewalnum|renewalwgt)$')))

# Turn actions into an R function
model <- g3_to_r(actions)#, strict = TRUE, trace = TRUE)

# Turn actions into C++ objective function code
tmb_model <- g3_to_tmb(actions)

# Get the parameter template 
tmb_param <- attr(tmb_model, 'parameter_template')

## Fill in the parameter template
tmb_param <- 
  tmb_param %>% 
  g3_init_guess('\\.rec', 100, 0.001, 200, 1) %>% 
  g3_init_guess('\\.init', 100, 0.001, 200, 1) %>% 
  g3_init_guess('recl', 22, 5, 30, 0) %>% 
  g3_init_guess('rec.sd', 6, 0, 20, 1) %>% 
  g3_init_guess('rec.scalar', 1, 1, 100, 0) %>% 
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
  g3_init_guess('mat_alpha', 95, 10, 110, 1) %>% # g3_init_guess('mat1', 0, 10, 200, 1) %>% 
  g3_init_guess('mat_l50', params_bio$mat.l50, 0.75*params_bio$mat.l50, 1.25*params_bio$mat.l50, 1) %>%  #mat.l50$l50, 0.001,1000,1) %>% #
  g3_init_guess('mat_beta', 3.5, 0.1, 4, 0) %>% # g3_init_guess('mat1', 0, 10, 200, 1) %>% 
  g3_init_guess('mat_a50', 2, 1, 3, 0) %>%  #mat.l50$l50, 0.001,1000,1) %>% #
  g3_init_guess('walpha', params_bio$walpha, 1e-10, 1, 0) %>% 
  g3_init_guess('wbeta', params_bio$wbeta, 2, 4, 0) %>% 
  g3_init_guess('ander.p0$', 0, -1, 1, 0) %>%
  g3_init_guess('ander.p2$', 1, 0, 1,0) %>% 
  g3_init_guess('ander.p5$', max(g3_stock_def(mat_stock, 'minlen')), 10, 200, 0) %>% 
  g3_init_guess('\\.p1', 0.5, 0.000001, 1, 1) %>% 
  g3_init_guess('\\.p3', 50, 0.01, 100, 1) %>% 
  g3_init_guess('\\.p4', 50, 0.01, 100, 1) %>% 
  g3_init_guess('\\.Lmode$', 
                max(g3_stock_def(mat_stock, 'minlen'))/2, 
                max(g3_stock_def(mat_stock, 'minlen'))/2.5, 
                max(g3_stock_def(mat_stock, 'minlen'))/1.5, 1) %>% 
  #  g3_init_guess('bh_mu', 2, 0, 100, 1) %>% ## Maximum recruitment R0
  #  g3_init_guess('bh_lambda', 2, 0, 100, 1) %>% 
  g3_init_guess('srr_h', 0.8, 0, 1, 0) %>% 
  g3_init_guess('R0', params_bio$R0/10e3, (params_bio$R0/10e3)*0.25, (params_bio$R0/10e3)*1.25, 0) %>% 
  g3_init_guess('B0', 2430345539/10e3, 2430345539/10e3, 2430345539/10e3, 0) %>% 
  g3_init_guess('Lambda', 5000000/10e3, (5000000/10e3)*0.9, (5000000/10e3)*1.1, 1) %>% 
  #g3_init_guess('R0', 22102, 22102, 22102, 0) %>% 
  g3_init_guess('catchability', 0.5,0,1,1)

## --------------------------------------------------------------------------

## Run the R-model
result <- model(tmb_param$value)
print(result[[1]])

tmp <- attributes(result)



(tmp$hist_yft_imm__num*tmp$hist_yft_imm__wgt) %>% as.data.frame.table() %>% mutate(stock = 'imm') %>% 
  bind_rows(
    (tmp$hist_yft_mat__num*tmp$hist_yft_mat__wgt) %>% as.data.frame.table() %>% mutate(stock = 'mat')
  ) %>% group_by(time, stock) %>% summarise(t = sum(Freq)) %>% mutate(t = t/1000) %>% 
  separate(time, into = c('year', 'step')) %>% 
  mutate(time = as.numeric(year) + (as.numeric(step)-1)/as.numeric(step)) %>% ggplot(aes(time, t)) + geom_line(aes(col = stock))

tmp$hist_yft_imm__num %>% as.data.frame.table() %>% mutate(stock = 'imm') %>%
   bind_rows(tmp$hist_yft_mat__num %>% as.data.frame.table() %>% mutate(stock = 'mat')) %>%
   separate(time, into = c('year', 'step')) %>% filter(year %in% 1990:2000) %>% 
  group_by(age, step, stock, length) %>%
  summarise(Freq = mean(Freq)) %>%
  ungroup() %>% 
  group_by(age, step, stock) %>%
   summarise(natage = sum(Freq)) %>%
   ungroup() %>%
   group_by(age, stock) %>%
   summarise(n = mean(natage)) -> qq

init_num <- qq %>% mutate(age = gsub('age', '', age) %>% as.numeric()) %>% mutate(n = n/10000)
save(init_num, file = 'MODEL/data/init_num.Rdata')

(tmp$hist_yft_imm__num*tmp$hist_yft_imm__wgt) %>% as.data.frame.table() %>% mutate(stock = 'imm') %>%
  bind_rows(
    (tmp$hist_yft_mat__num*tmp$hist_yft_mat__wgt) %>% as.data.frame.table() %>% mutate(stock = 'mat')
  ) %>%
  separate(time, into = c('year', 'step')) %>% filter(year %in% 1990:2000) %>% 
  group_by(year, step, stock) %>% summarise(total = sum(Freq)) %>% 
  ungroup() %>% group_by(stock) %>% summarise(m = max(total)) 

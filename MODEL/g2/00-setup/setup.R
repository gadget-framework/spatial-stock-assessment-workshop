library(mfdb)
library(tidyverse)
library(devtools)
library(Rgadget)

bootstrap <- FALSE

year_range <- 1951:2015

vers <- '02-baseline'
base_dir <- 'MODEL/g2'

mat_stock <- 'yftmat'
imm_stock <- 'yftimm'
stock_names <- c(imm_stock, mat_stock)

species_name <- 'yft'
data_source_suffix <- ''

dir.create(vers)
gd <- gadget.variant.dir(sprintf(paste0("%s/",vers),base_dir))

#mdb <- mfdb("MODEL/g3/noaa_spatial.duckdb")

defaults <- list(area = mfdb_group('1' = 1),
                 timestep = mfdb_timestep_quarterly,
                 year = year_range,
                 species = 'YFT',
                 sampling_type = 'sim_4')

schedule <- 
  expand.grid(year = year_range, step = 1:4) %>% 
  arrange(year)

gadgetfile('Modelfiles/time',
           file_type = 'time',
           components = list(list(firstyear = min(year_range),
                                  firststep= 1,
                                  lastyear= max(year_range),
                                  laststep= 4,
                                  notimesteps=c(4,3,3,3,3)))) %>% 
  write.gadget.file(gd)

gadgetfile('Modelfiles/area',
           file_type = 'area',
           components = list(list(areas = 1,
                                  size = 1,
                                  temperature= schedule %>% mutate(area = 1, temperature = 5)))) %>% 
  write.gadget.file(gd)


fs::dir_ls(file.path('MODEL', 'data')) %>%
  stringr::str_subset('.Rdata') %>%
  lapply(load,.GlobalEnv)

#source(sprintf('%s/00-setup/setup-timevariablefiles.R',base_dir))
#source(sprintf('%s/00-setup/setup-fleet-data.R',base_dir))
source(sprintf('%s/00-setup/setup-fleets.R',base_dir))
source(sprintf('%s/00-setup/setup-model.R',base_dir))
#source(sprintf('%s/00-setup/setup-catchdistribution.R',base_dir))
#source(sprintf('%s/00-setup/setup-indices.R',base_dir))
source(sprintf('%s/00-setup/setup-likelihood.R',base_dir))

Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd))
callGadget(s = 1, i = 'params.in', log = 'score')
callGadget(l=1, i='params.in', p='params.init')

## setting up model variants
# source(sprintf('%s/00-setup/setup-igfs_sel.R',base_dir))
# 
 if(bootstrap){
   source(sprintf('%s/00-setup/setup-bootstrap.R',base_dir))
   file.copy(sprintf('%s/%s/bootrun.R',base_dir,'00-setup'),gd)
 }

file.copy(sprintf('%s/%s/itterfitter.sh',base_dir,'00-setup'),gd)
file.copy(sprintf('%s/%s/run.R',base_dir,'00-setup'),gd)
file.copy(sprintf('%s/%s/optinfofile',base_dir,'00-setup'),gd)



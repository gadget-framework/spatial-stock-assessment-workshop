library(Rgadget)
library(tidyverse)

tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(),
                        wgts='WGTS',
                        params.file = 'params.init')

fit <- gadget.fit()
if (!dir.exists('figs')) dir.create('figs')
fit$likelihood <- fit$likelihoodsummary
fit$likelihood$num <- fit$likelihood$likelihood_value
gadgetplots::gadget_plots(fit, path = 'figs', file_type = 'html')

#print(paste0('Running analytical retro for ', vers))
#gadget.retro(main.file = paste0('WGTS','/main.final'),params.file = 'params.init', optinfofile = 'optinfofile', pre = paste0('Retro'))

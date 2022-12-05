## -----------------------------------------------------------------------------
##
## Setup likelihood 
##
## -----------------------------------------------------------------------------

nll_breakdown <- TRUE  # Turn to TRUE to get per-step nll
lik_report <- TRUE

likelihood_actions <- list(
  g3l_understocking(stocks, nll_breakdown = nll_breakdown, weight = 1e6),
  
  g3l_catchdistribution(
    'ldist_pse',
    ldist.pse[[1]], 
    fleets = list(pse),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_catchdistribution(
    'ldist_trol',
    ldist.trol[[1]], 
    fleets = list(trol),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_catchdistribution(
    'ldist_bb',
    ldist.bb[[1]], 
    fleets = list(bb),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_catchdistribution(
    'ldist_gil',
    ldist.gil[[1]], 
    fleets = list(gil),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_catchdistribution(
    'ldist_lln',
    ldist.lln[[1]],
    fleets = list(lln),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_catchdistribution(
    'ldist_other',
    ldist.other[[1]], 
    fleets = list(other),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_catchdistribution(
    'ldist_hln',
    ldist.hln[[1]], 
    fleets = list(hln),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  # g3l_catchdistribution(
  #   'cpue1',
  #   (cpue1[[1]]),
  #   fleets = list(survey),
  #   stocks = stocks,
  #   g3l_distribution_surveyindices_log(beta = 1),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),
  
  g3l_abundancedistribution(
    'cpue1',
    (cpue1[[1]]),
    fleets = list(),
    stocks = stocks,
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  list()
)

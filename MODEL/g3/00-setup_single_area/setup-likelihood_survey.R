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
    'cpue1',
    (cpue1[[1]]),
    fleets = list(survey),
    stocks = stocks,
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  list()
)

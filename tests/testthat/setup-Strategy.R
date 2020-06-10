setup({data(strategies);
  strats <<- lapply(strategies, optPnL:::create_strat)})

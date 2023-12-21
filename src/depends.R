# requires `remotes` and `pacman` to get other dependencies
install.packages(setdiff("pacman", rownames(installed.packages())))
install.packages(setdiff("remotes", rownames(installed.packages())))

# we require an older version of mgcv pre-thin plate spline updates
# can remove after first install
remotes::install_version("mgcv", "1.8-42")

pacman::p_load(
  here, # used to set working directory
  parallel, # needed if you want to multi-core the models
  doParallel, # needed if you want to multi-core the models,
  foreach, # processing
  tibble, # processing
  lubridate, # processing
  tidyr, # processing
  dplyr, # processing
  stringr, # processing
  tibble, # processing
  magrittr, # processing
  ggplot2, # plotting
  patchwork, # plotting
  mgcv, # GAM modelling package
  matrixStats, # needed for GAM prediction interval creation
  fable, # ARIMA modelling
  fabletools, # ARIMA modelling
  tsibble, # ARIMA modelling
  scoringutils # scoring
)

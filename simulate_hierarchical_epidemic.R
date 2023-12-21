# Simulating the Winter 2022/23 flu admissions epidemic wave in England.
#
# This script produces a dataframe with all admission counts and covariates
# for simulated sub-regions.
# In addition, we produce a neightbourhood network in the form required by
# `mgcv`


## Method
# Data
# Simulate an epidemic over time steps using a probability density function with
# similar properties to the flu wave this year.
# Create perturbations at regional and sub-region level to adjust the central epidemic wave
#   This will correlate trends within region and neighbors
# Create day of week effects for 2 days per week, varying by sub-region
# Add noise
# Note: we do not try to simulate counts to match the English wave exactly, as this is a simulation
# but this could be achieved with some multiplication factor
# Network
# Link together each sub region with a region
# Link random sub regions not already linked, creating a loose spatial structure



# get the config parameters for the epidemic wave
config_path <- "./config.yaml"
config <- yaml::read_yaml(config_path)



# define time series
t <- 1:config$epidemic_data$final_t
# define "true" epidemic values, to be manipulated - looks similar to the english 2022 influenza wave
# using the pdf of a caucy we can convert to an admission rate per capita, then
# perterb the signal for each location in scale and time offset.
y <- dcauchy(t, location = 2 * max(t) / 3, scale = config$epidemic_data$final_t / 15, log = FALSE)

# original epidemic shape
# plot(t, y)

# we want to increase/change the admission rate for each region by a multiplicative factor

# generate how much "uplift" each region will have
region_mult <- 1 + rgamma(config$epidemic_data$n_regions, shape = 1, rate = 3)
# generate how much "uplift" each sub-region will have (independent of region)
sub_region_mult <- 1 + rgamma(config$epidemic_data$n_sub_regions * config$epidemic_data$n_regions, shape = 1, rate = 3)

# combine the regional and subregion effects to get the total effect for a sub region
sub_region_total_mult <- rep(region_mult, each = config$epidemic_data$n_sub_regions) * sub_region_mult

# repeat the "original" epidemic for as many sub regions as we have, then
# multiply the "original" epidemic by each sub regionals change factor
# which gives us a "unique" trend per sub-region
y_sub_regions <-  matrix(rep(y, config$epidemic_data$n_regions * config$epidemic_data$n_sub_regions),
  nrow = config$epidemic_data$final_t,
  ncol = config$epidemic_data$n_regions * config$epidemic_data$n_sub_regions)

df <- as.data.frame(y_sub_regions) |>
  dplyr::mutate(t = dplyr::row_number()) |>
  tidyr::pivot_longer(cols = dplyr::starts_with("V")) |>
  dplyr::arrange(name) |>
  # add in our sub region effect
  dplyr::mutate(sub_region_factor = rep(sub_region_total_mult, each = config$epidemic_data$final_t)) |>
  # lets say the DOW multiplicative effect is the same for saturday/sunday - but different for each
  # sub region
  dplyr::group_by(name) |>
  # the days may not match actual weekends in the data, but weekends are essentially arbitrary.
  # We just need two consecutive days with lower admissions per week.
  dplyr::mutate(dow_factor = ifelse(t %% 7 == 1 | t %% 7 == 0, rnorm(1, 0.8, 0.05), 1)) |>
  dplyr::ungroup() %>%
  # add a little noise (additive)
  dplyr::mutate(noise_factor = rnorm(n = nrow(.), sd = max(value) / 10)) |>
  # bring together all the region, day-of-week and noise factors
  dplyr::mutate(y_rate = (value * sub_region_factor * dow_factor) + noise_factor) |>
  dplyr::mutate(y_rate = ifelse(y_rate < 0, 0, y_rate)) |>
  # generate a random population size for each sub region
  dplyr::group_by(name) |>
  dplyr::mutate(population_size = round(rnorm(1, 10**4, 10**3))) |>
  dplyr::ungroup() |>
  dplyr::mutate(flu_admissions = round(y_rate * population_size)) |>
  # create identifier, regions in caps, sub-regions in lowercase
  dplyr::mutate(region = as.factor(rep(sample(LETTERS, config$epidemic_data$n_regions, replace = F), each = config$epidemic_data$n_regions * config$epidemic_data$final_t)),
    sub_region = rep(sample(letters, config$epidemic_data$n_regions * config$epidemic_data$n_sub_regions, replace = F), each = config$epidemic_data$final_t),
    sub_region = as.factor(paste0(region, sub_region))) |>
  dplyr::select(-name, -dplyr::contains("factor"), -y_rate, -value) |>
  dplyr::mutate(date = as.Date(config$epidemic_data$start_date) + t - 1)




# derive a neighbourhood list
# assume all sub regions are connected within a region
# and there are some random connections otherwise
# This is a pseudo spatial structure, rather than a true one, the geometry may not work in 2D
locations <- unique(df$sub_region)

nb <- list()

# add all other sub regions within the region to the neighbours list
for (i in locations) {
  for (j in locations) {
    if (j != i & substr(j, 1, 1) == substr(i, 1, 1))
      nb[[i]] <- c(nb[[i]], j)
  }
}

# add some random other subregions (but not this within their regions or themselves)
for (i in sample(locations)[1:floor(length(locations) / 2)]) {
  j <- sample(locations, 1)
  if (!(j %in% nb[[i]]) & i != j) {
    nb[[i]] <- c(j, nb[[i]])
  }
}


# lets add in a little data error, as we are simulating the real world
# choose one sub region
problem_sub_region <- locations[1]
df <- df |>
  dplyr::mutate(flu_admissions = dplyr::case_when(
    # area only reports 1/10 of cases and is extra noisy
    sub_region == problem_sub_region ~ round(rgamma(1, 3, 1) + flu_admissions / 10),
    T ~ flu_admissions
  ))



# quick visual check of the different areas
# df |>
#   ggplot2::ggplot() +
#   geom_line(aes(x = t, y = flu_admissions / population_size)) +
#   facet_wrap(~sub_region)


# create aggregates for ease later
national <- df |>
  dplyr::group_by(date, t) |>
  dplyr::summarise(flu_admissions = sum(flu_admissions),
    population_size = sum(population_size)) |>
  dplyr::ungroup()

regional <- df |>
  dplyr::group_by(date, t, region) |>
  dplyr::summarise(flu_admissions = sum(flu_admissions),
    population_size = sum(population_size)) |>
  dplyr::ungroup()

# bring it all together in easy to access format
simulated_output <- list(
  neighbour_list = nb,
  sub_region_data = df,
  region_data = regional,
  nation_data = national
)

saveRDS(simulated_output, file = "./data_storage/epidemic_wave.rds")

rm(list = ls())

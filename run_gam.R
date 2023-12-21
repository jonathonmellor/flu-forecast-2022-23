# Script to run GAM

# assumes working directory set to the directory this file sits in
source("./src/gam.R")

print("Running GAM")



config_path <- "./config.yaml"
config <- yaml::read_yaml(config_path)

simulated_output <- readRDS("./data_storage/epidemic_wave.rds")

# we are only using the sub_region data (most granular) to fit
flu <- simulated_output$sub_region_data


# Note: we will use 28 * 3 / 7 cores
max_date <- max(flu$date)
preds_storage <- data.frame()

factor2ref <- dplyr::distinct(flu, sub_region, region) |>
  dplyr::rename(factor = sub_region, factor2 = region)

# create parallel clusters
# WARNING - you will want to change the configuration of parallelisation
# dependent on your device as this may slam your processing cores
if (config$overall_parameters$is_parallel == TRUE) {
  cl <- parallel::makeCluster(ceiling(config$overall_parameters$fitting_window / 7), type = "FORK")
  doParallel::registerDoParallel(cl)
  `%runloop%` <- foreach::`%dopar%`

} else {
  `%runloop%` <- foreach::`%do%`
}


# run the big loop of GAMs for each lookback data
# 7 days space between each lookback period
preds_storage <- foreach::foreach(lookback_week = seq(0, config$overall_parameters$n_lookbacks * 7, 7),
  .combine = "bind_rows") %runloop% {
  flu |>
    filter(
      date <= max(date) - lubridate::days(lookback_week),
      date >= (max(date) - lubridate::days(lookback_week) - config$overall_parameters$fitting_window)
    ) -> lookback_subset

  preds <- run_gam_spatial(
    admissions = lookback_subset$flu_admissions,
    date = lubridate::ymd(lookback_subset$date),
    factor = lookback_subset$sub_region,
    factor2 = lookback_subset$region,
    population = lookback_subset$population_size,
    denominator = config$gam_parameters$national_spline,
    denominator_factor = config$gam_parameters$sub_regional_spline,
    spatial_nb_object = simulated_output$neighbour_list,
    ref = factor2ref,
    bs = config$gam_parameters$spline_type,
    horizon = config$overall_parameters$horizon,
    family = "nb",
    n_pi_samples = config$overall_parameters$n_pi_samples
  )
}

# end this cluster
if (config$overall_parameters$is_parallel) stopCluster(cl)


sub_region_gam_output <- preds_storage |>
  dplyr::filter(prediction == TRUE) |>
  dplyr::select(-prediction) |>
  dplyr::rename(region = factor2, sub_region = factor) |>
  dplyr::left_join(
    flu |>
      select(date, region, sub_region, flu_admissions, population_size) |>
      rename(true_value = flu_admissions),
    by = c("date", "region", "sub_region")
  ) |>
  dplyr::mutate(model = "GAM",
    geography = "sub_region") |>
  tidyr::pivot_longer(dplyr::starts_with(c("pi_")), names_to = "target_type", values_to = "prediction_rate") |>
  dplyr::mutate(prediction = prediction_rate * population_size) |>
  dplyr::mutate(quantile = dplyr::case_when(
    stringr::str_detect(target_type, "fit") ~ 0.5,
    stringr::str_detect(target_type, "lower_90") ~ 0.05,
    stringr::str_detect(target_type, "upper_90") ~ 0.95,
    stringr::str_detect(target_type, "lower_95") ~ 0.025,
    stringr::str_detect(target_type, "upper_95") ~ 0.975,
    stringr::str_detect(target_type, "lower_66") ~ 0.17,
    stringr::str_detect(target_type, "upper_66") ~ 0.83,
    stringr::str_detect(target_type, "lower_50") ~ 0.25,
    stringr::str_detect(target_type, "upper_50") ~ 0.75,
    TRUE ~ NA_real_
  )) |>
  dplyr::inner_join(preds_storage |>
    dplyr::group_by(start_date) |>
    dplyr::summarise(prediction_start_date = max(date) - 13),
  by = "start_date"
  ) |>
  dplyr::select(prediction_start_date, date, region, sub_region, quantile, prediction, true_value, model, geography) |>
  dplyr::filter(prediction_start_date <= max_date - config$overall_parameters$horizon)


nation_gam_output <- sub_region_gam_output |>
  dplyr::group_by(prediction_start_date, date, quantile, model) |>
  dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x) sum(x, na.rm = T))) |>
  dplyr::ungroup() |>
  dplyr::mutate(geography = "nation")


region_gam_output <- sub_region_gam_output |>
  dplyr::group_by(prediction_start_date, date, region, quantile, model) |>
  dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x) sum(x, na.rm = T))) |>
  dplyr::ungroup() |>
  dplyr::mutate(geography = "region")

saveRDS(sub_region_gam_output, "./data_storage/sub_region_gam_output.rds")
saveRDS(nation_gam_output, "./data_storage/nation_gam_output.rds")
saveRDS(region_gam_output, "./data_storage/region_gam_output.rds")

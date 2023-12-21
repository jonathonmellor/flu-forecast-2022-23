# Script to run ARIMA

# assumes working directory set to the directory this file sits in
source("./src/arima.R")

print("Running ARIMA")


config_path <- "./config.yaml"
config <- yaml::read_yaml(config_path)

simulated_output <- readRDS("./data_storage/epidemic_wave.rds")

# Create a time series
timeseries_sub_region <- simulated_output$sub_region_data |>
  tsibble::as_tsibble(key = c("sub_region", "region"), index = date)

timeseries_nation <- simulated_output$nation_data |>
  tsibble::as_tsibble(index = date)

timeseries_region <- simulated_output$region_data |>
  tsibble::as_tsibble(key = c("region"), index = date)


## Create dfs for each lookback
sub_region_lookbacks <- list()
region_lookbacks <- list()
nation_lookbacks <- list()
prediction_start_dates <- tibble::tibble(lookback = NA, prediction_start = NA)

print("Setting up historic slices")
for (num in 2:config$overall_parameters$n_lookbacks) {
  min_date <- max(timeseries_sub_region$date) - config$overall_parameters$fitting_window - (7 * num)
  max_date <- max(timeseries_sub_region$date) - (7 * num)
  name <- paste0("lookback_", num * 7)

  sub_region_lookbacks[[name]] <- timeseries_sub_region |> dplyr::filter(date >= min_date & date <= max_date)

  region_lookbacks[[name]] <- timeseries_region |> dplyr::filter(date >= min_date & date <= max_date)

  nation_lookbacks[[name]] <- timeseries_nation |> dplyr::filter(date >= min_date & date <= max_date)

  prediction_start_dates <- prediction_start_dates |>
    dplyr::add_row(lookback = name, prediction_start = max_date + 1)
}



## Run arima for different aggregations
# runs number of geographical units x number of lookbacks, so may take time
print("Running national")
nation_arima_output <- lapply(X = nation_lookbacks,
  FUN = run_arima_agg,
  geography = "nation",
  true_data = simulated_output) |>
  dplyr::bind_rows()
print("Running regional")
region_arima_output <- lapply(X = region_lookbacks,
  FUN = run_arima_agg,
  geography = "region",
  true_data = simulated_output) |>
  dplyr::bind_rows()

print("Running sub-regional")
sub_region_arima_output <- lapply(X = sub_region_lookbacks,
  FUN = run_arima_agg,
  geography = "sub_region",
  true_data = simulated_output) |>
  dplyr::bind_rows()

saveRDS(sub_region_arima_output, "./data_storage/sub_region_arima_output.rds")
saveRDS(region_arima_output, "./data_storage/region_arima_output.rds")
saveRDS(nation_arima_output, "./data_storage/nation_arima_output.rds")

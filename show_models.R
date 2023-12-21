# script to run the models, save their output calling other scripts and plot
# the projections

# may need to have `here` installed already
here::here()

set.seed(8675309)

# run the models and save their outputs
source("./src/depends.R")
source("./simulate_hierarchical_epidemic.R")
source("./run_arima.R")
source("./run_gam.R")

ggplot2::theme_set(ggplot2::theme_bw())


# load predictions
# ARIMA
nation_arima_output <- readRDS("./data_storage/nation_arima_output.rds")
region_arima_output <- readRDS("./data_storage/region_arima_output.rds")
sub_region_arima_output <- readRDS("./data_storage/sub_region_arima_output.rds")

# GAM
nation_gam_output <- readRDS("./data_storage/nation_gam_output.rds")
region_gam_output <- readRDS("./data_storage/region_gam_output.rds")
sub_region_gam_output <- readRDS("./data_storage/sub_region_gam_output.rds")

# combine together for ease
results <- dplyr::bind_rows(
  nation_gam_output,
  region_gam_output,
  sub_region_gam_output,
  nation_arima_output,
  region_arima_output,
  sub_region_arima_output
) |>
  dplyr::filter(as.numeric(quantile) %in% c(0.5, 0.95, 0.05, 0.25, 0.75))


# plot forecasts

# sub-region
results |>
  dplyr::filter(geography == "sub_region") |>
  dplyr::filter(date <= max(prediction_start_date)) |>
  dplyr::mutate(prediction_start_date = as.factor(prediction_start_date)) |>
  tidyr::pivot_wider(names_from = "quantile", values_from = "prediction") |>
  ggplot() +
  geom_point(aes(x = date, y = true_value), alpha = 0.1, size = 1) +
  geom_line(aes(x = date, y = `0.5`, group = prediction_start_date), linetype = 2, alpha = 0.5) +
  geom_ribbon(aes(x = date, ymax = `0.95`, ymin = `0.05`, group = prediction_start_date, fill = prediction_start_date), alpha = 0.2) +
  geom_ribbon(aes(x = date, ymax = `0.75`, ymin = `0.25`, group = prediction_start_date, fill = prediction_start_date), alpha = 0.5) +
  facet_grid(sub_region ~ model, scales = "free") +
  ylab("influenza admissions")

# national
# as we are aggregating uncertainty naively the prediction intervals are v wide
results |>
  dplyr::filter(geography == "nation") |>
  dplyr::filter(date <= max(prediction_start_date)) |>
  dplyr::mutate(prediction_start_date = as.factor(prediction_start_date)) |>
  tidyr::pivot_wider(names_from = "quantile", values_from = "prediction") |>
  ggplot() +
  geom_point(aes(x = date, y = true_value), alpha = 0.1, size = 1) +
  geom_line(aes(x = date, y = `0.5`, group = prediction_start_date), linetype = 2, alpha = 0.5) +
  geom_ribbon(aes(x = date, ymax = `0.95`, ymin = `0.05`, group = prediction_start_date, fill = prediction_start_date), alpha = 0.2) +
  geom_ribbon(aes(x = date, ymax = `0.75`, ymin = `0.25`, group = prediction_start_date, fill = prediction_start_date), alpha = 0.5) +
  facet_wrap(~model) +
  ylab("influenza admissions")

# region
results |>
  dplyr::filter(geography == "region") |>
  dplyr::filter(date <= max(prediction_start_date)) |>
  dplyr::mutate(prediction_start_date = as.factor(prediction_start_date)) |>
  tidyr::pivot_wider(names_from = "quantile", values_from = "prediction") |>
  ggplot() +
  geom_point(aes(x = date, y = true_value), alpha = 0.1, size = 1) +
  geom_line(aes(x = date, y = `0.5`, group = prediction_start_date), linetype = 2, alpha = 0.5) +
  geom_ribbon(aes(x = date, ymax = `0.95`, ymin = `0.05`, group = prediction_start_date, fill = prediction_start_date), alpha = 0.1) +
  geom_ribbon(aes(x = date, ymax = `0.75`, ymin = `0.25`, group = prediction_start_date, fill = prediction_start_date), alpha = 0.3) +
  facet_grid(region ~ model, scales = "free") +
  ylab("influenza admissions")


# score forecasts
# example of scoring at sub_region level prediction and showing result
sub_region_score <- results |>
  dplyr::filter(geography == "sub_region") |>
  dplyr::filter(date < max(prediction_start_date)) |>
  scoringutils::score() |>
  scoringutils::add_coverage(by = c("model", "prediction_start_date"), ranges = c(50, 90)) |>
  scoringutils::summarise_scores(
    by = c("model", "prediction_start_date"),
    na.rm = TRUE
  ) |>
  scoringutils::summarise_scores(fun = signif, digits = 3)

# explore scores over time
sub_region_score |>
  ggplot() +
  geom_line(aes(x = prediction_start_date, y = interval_score, group = model, color = model)) +
  sub_region_score |>  ggplot() +
  geom_line(aes(x = prediction_start_date, y = coverage_90, group = model, color = model)) +
  geom_line(aes(x = prediction_start_date, y = coverage_50, group = model, color = model), linetype = 2) +
  geom_hline(aes(yintercept = 0.9), alpha = 0.7) +
  geom_hline(aes(yintercept = 0.5), linetype = 2, alpha = 0.7) +
  ylim(c(0, NA)) +
  ylab("coverage")

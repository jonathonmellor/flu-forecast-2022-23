## run arima model and ouput into a format that can be scored
run_arima_agg <- function(lookback_data, geography, true_data) {
  bu_fit <- lookback_data |>
    fabletools::model(arima = fable::ARIMA(log(flu_admissions + 1)))

  forecast <- bu_fit |>
    fabletools::forecast(h = 14, bootstrap = TRUE, times = 1000)

  if (geography == "region") {
    forecast <- forecast |>
      tsibble::as_tibble() |>
      dplyr::group_by(date, region) |>
      dplyr::summarise(
        flu_admissions = sum(flu_admissions),
      ) |>
      dplyr::ungroup() |>
      dplyr::inner_join(true_data$region_data |> dplyr::rename(true_value = flu_admissions), by = c("date", "region")) |>
      dplyr::mutate(
        prediction_start_date = min(date)
      )
  } else if (geography == "nation") {
    forecast <- forecast |>
      tsibble::as_tibble() |>
      dplyr::group_by(date) |>
      dplyr::summarise(
        flu_admissions = sum(flu_admissions),
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        prediction_start_date = min(date)
      ) |>
      dplyr::inner_join(true_data$nation_data |> dplyr::rename(true_value = flu_admissions), by = "date")
  } else if (geography == "sub_region") {
    forecast <- forecast |>
      tsibble::as_tibble() |>
      dplyr::inner_join(true_data$sub_region_data |> dplyr::rename(true_value = flu_admissions), by = c("date", "region", "sub_region")) |>
      dplyr::mutate(
        prediction_start_date = min(date)
      )
  }

  summary_outputs <- forecast |>
    dplyr::mutate(
      quantile_250 = quantile(flu_admissions, p = 0.25),
      quantile_750 = quantile(flu_admissions, p = 0.75),
      quantile_170 = quantile(flu_admissions, p = 0.17),
      quantile_830 = quantile(flu_admissions, p = 0.83),
      quantile_500 = quantile(flu_admissions, p = 0.5),
      quantile_050 = quantile(flu_admissions, p = 0.05),
      quantile_950 = quantile(flu_admissions, p = 0.95),
      quantile_025 = quantile(flu_admissions, p = 0.025),
      quantile_975 = quantile(flu_admissions, p = 0.975)
    ) |>
    tidyr::pivot_longer(dplyr::starts_with("quantile"), names_to = "quantile", names_prefix = "quantile_", values_to = "prediction") |>
    dplyr::mutate(quantile = as.numeric(quantile) / 1000) |>
    dplyr::select(-c(flu_admissions, t)) |>
    dplyr::mutate(geography = geography,
      model = "ARIMA")


  return(summary_outputs)
}

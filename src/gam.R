

run_gam_spatial <- function(admissions,
                            date,
                            factor,
                            factor2,
                            population,
                            denominator,
                            denominator_factor,
                            spatial_nb_object,
                            bs = "cr",
                            horizon = 14,
                            ref,
                            family = c("poisson", "quasipoisson", "nb"),
                            predict_dow = TRUE, # do we want to predict the day of week effects for the PI?
                            n_pi_samples = 10000) {
  date_numeric <- as.numeric(date - min(date))
  d <- as.factor(weekdays(date))
  k <- floor(max(date_numeric) / denominator)
  kf <- floor(max(date_numeric) / denominator_factor)


  # set seed for random sampling of PI
  set.seed(1)

  the_formula <- as.formula(admissions ~
    s(date_numeric, k = k, bs = bs, m = 2)
    + s(factor2, bs = "re")
    + s(date_numeric, factor, bs = "fs", k = kf, m = 2, xt = list(bs = bs))
    + s(d, factor, factor2, bs = "re")
    + s(factor, bs = "mrf", xt = list(nb = spatial_nb_object)))

  mod <- mgcv::bam(
    formula = the_formula,
    family = family,
    offset = log(population),
    discrete = T
  )

  to_join <- data.frame()


  prediction_frame <- expand.grid(
    date = seq(min(date),
      max(date) + days(horizon),
      by = "day"
    ),
    factor = unique(factor)
  ) |>
    dplyr::mutate(
      d = as.factor(weekdays(date)),
      date_numeric = as.numeric(date - min(date))
    ) |>
    dplyr::left_join(ref)

  fit <- mgcv::predict.gam(
    object = mod,
    newdata = prediction_frame,
    se.fit = TRUE,
    exclude = c("s(d,factor2,factor)", "s(d,factor,factor2)", "s(d)")
  )

  # PREDICTION INTERVAL CALCULATION #
  # using a modified version of the approach:
  # https://www.r-bloggers.com/2019/08/prediction-intervals-for-generalized-additive-models-gams/
  #########################################################
  # Inverse link function for the gam
  linv <- mod$family$linkinv
  # Generate posterior sample of parameter values, assuming multivariate normal
  beta <- coef(mod)
  V <- vcov(mod)
  # Cholesky decomposition allows conversion from standard normal to
  # multivariate with the correct covariance matrix
  Cv <- chol(V)
  # Standard normal random sample
  nus <- rnorm(n_pi_samples * length(beta))
  # transformed sample via Cholesky decomposition
  beta_sims <- beta + t(Cv) %*% matrix(nus, nrow = length(beta), ncol = n_pi_samples)
  if (predict_dow == FALSE) {
    beta_sims <- as.data.frame(t(beta_sims))
    # Would be good to automate the exclusion part, rather than hardcoding which
    # parts to exclude (for when we build a general function)
    beta_sims <- beta_sims |>
      dplyr::select(-starts_with("s(d,factor2,factor)")) |>
      dplyr::select(-starts_with("s(d,factor,factor2)"))
    beta_sims <- t(as.matrix(beta_sims))
  }
  # Generate model matrix for the prediction frame - this tells the model the
  # values of each variable at which to evaluate the model
  Xp <- predict(mod, prediction_frame, type = "lpmatrix")
  if (predict_dow == FALSE) {
    Xp <- as.data.frame(Xp)
    # Would be good to automate the exclusion part, rather than hardcoding which
    # parts to exclude (for when we build a general function)
    Xp <- Xp %>%
      dplyr::select(-starts_with("s(d,factor2,factor)")) |>
      dplyr::select(-starts_with("s(d,factor,factor2)"))
    Xp <- as.matrix(Xp)
  }
  # Size parameter of the fitted negative binomial distribution
  theta_ <- mod$family$getTheta(TRUE)

  # Need to generate a data frame with population sizes for future dates
  # Probably a nicer way to do this, as this depends on the vectors always
  # being in the correct order
  populations_temp <- as.data.frame(factor) |>
    mutate(population_size = population) |>
    distinct()
  populations <- (prediction_frame |> dplyr::left_join(populations_temp, by = c("factor")))$population_size

  # Pre-allocate matrix for storing predictions
  fits_out <- matrix(0, nrow(prediction_frame), n_pi_samples)

  for (i in 1:n_pi_samples)
  {
    # For each posterior parameter sample, calculate the predicted mean trend
    # Log population added to convert back into counts before sampling from neg binom
    # This also relies on vectors always being in the correct order, which is likely to
    # be the case, but flagging for awareness
    fits <- linv(Xp %*% beta_sims[, i] + log(populations))
    # Generate a negative binomial sample from this mean trend
    fits <- rnbinom(n = length(fits), mu = fits, size = theta_)
    fits_out[, i] <- fits
  }
  # Calculate quantiles from predicted samples, currently 95% confidence level
  pi_fit <- matrixStats::rowQuantiles(fits_out, probs = 0.5)
  pi_lower_90 <- matrixStats::rowQuantiles(fits_out, probs = 0.05)
  pi_upper_90 <- matrixStats::rowQuantiles(fits_out, probs = 0.95)
  pi_lower_95 <- matrixStats::rowQuantiles(fits_out, probs = 0.025)
  pi_upper_95 <- matrixStats::rowQuantiles(fits_out, probs = 0.975)
  pi_lower_66 <- matrixStats::rowQuantiles(fits_out, probs = 0.17)
  pi_upper_66 <- matrixStats::rowQuantiles(fits_out, probs = 0.83)
  pi_lower_50 <- matrixStats::rowQuantiles(fits_out, probs = 0.25)
  pi_upper_50 <- matrixStats::rowQuantiles(fits_out, probs = 0.75)
  #################################################################
  mx <- max(date)

  prediction_frame |>
    mutate(
      model_fit = exp(fit$fit),
      lower_95 = exp(fit$fit - 1.96 * fit$se.fit),
      upper_95 = exp(fit$fit + 1.96 * fit$se.fit),
      #########################################################
      # Added predictions to output frame for comparison
      # Divide by populations for predicted rates, remove
      # division for predicted counts
      pi_fit = pi_fit / populations,
      pi_lower_90 = pi_lower_90 / populations,
      pi_upper_90 = pi_upper_90 / populations,
      pi_lower_95 = pi_lower_95 / populations,
      pi_upper_95 = pi_upper_95 / populations,
      pi_lower_66 = pi_lower_66 / populations,
      pi_upper_66 = pi_upper_66 / populations,
      pi_lower_50 = pi_lower_50 / populations,
      pi_upper_50 = pi_upper_50 / populations,
      ######################################################
      prediction = date > mx,
      start_date = min(date),
      denominator = denominator,
      denominator_factor = denominator_factor,
      bs = bs,
      horizon = horizon,
      family = family
    ) |>
    dplyr::select(-d, -date_numeric) -> predictions


  return(predictions)
}

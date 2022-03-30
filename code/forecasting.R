# This script uses the best model found via cross-validation to make
# out-of-sample predictions

# Save dependent and independent variable information
y_best <- all.vars(best_definition[["formula"]])[1]
xregs_best <- all.vars(best_definition[["formula"]])[-1]

# Create scenarios for independent variables (based on simple auto ARIMA models)
df_xregs_future <- df_trucks_ts_granger %>%
  tsibble::as_tibble() %>%
  dplyr::select(dplyr::all_of(xregs_best)) %>%
  purrr::map_df(
  .f = ~forecast::auto.arima(.x) %>%
    forecast::forecast(h = horizon) %>%
    purrr::pluck("mean") %>%
    as.numeric()
    ) %>%
  dplyr::mutate(
    date    = max(df_trucks_ts_granger$date) + 1:horizon,
    .before = 1
    ) %>%
  tsibble::as_tsibble(index = date)

# Fit the best model found
fit_best <- df_trucks_ts_granger %>%
  fabletools::model(best_definition, .safely = TRUE)

# Table to check if with coefficients they are significant (p-values)
# TODO: improve by creating some logic
coefs_best_definition <- fabletools::tidy(fit_best)
coefs_best_definition

# Plot to check if residues are distributed normally
# TODO: improve by creating some logic
plot_resids_best_model <- feasts::gg_tsresiduals(fit_best)
plot_resids_best_model

# Apply Ljung–Box test to verify residual autocorrelation
teste_ljung_box <- fit_best %>%
  fabletools::augment() %>%
  fabletools::features(
    .var     = .innov,
    features = feasts::ljung_box,
    lag      = nrow(df_trucks_ts_granger)/3.9,
    dof      = length(coefs_best_definition$term)
    )
if (teste_ljung_box$.model < 0.05) {
  stop(
    "There appear to be autocorrelation problems in the out-of-sample prediction model. Please check."
    )
  }

# Graph of observed and predicted values by the model
plot_fitted <- fit_best %>%
  fabletools::augment() %>%
  ggplot2::ggplot(ggplot2::aes(x = date)) +
  ggplot2::geom_line(ggplot2::aes(y = trucks, colour = "# of Trucks Sold")) +
  ggplot2::geom_line(ggplot2::aes(y = .fitted, colour = "Model")) +
  ggplot2::scale_colour_manual(
    values = c(`# of Trucks Sold` = "#282f6b", `Model` = "#b22200")
  ) +
  ggplot2::labs(title = "Forecast model", colour = NULL)
plot_fitted

# Make forecasts with scenarios
frcst_best <- fit_best %>%
  fabletools::forecast(new_data = df_xregs_future)
frcst_best

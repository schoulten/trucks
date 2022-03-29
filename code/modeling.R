# This script implements the modeling routine


# General steps -----------------------------------------------------------

# Some useful configuration objects for modeling


# Maximum forecast horizon
horizon <- 18

# Number of initial observations to use in Cross-Validation
init_obs <- 180

# Create new stationary variables (based on unit root tests)
df_trucks_ts_diff <- df_trucks_ts %>%
  dplyr::mutate(
    dplyr::across(
      .cols = vars_unit_root$variable[vars_unit_root$ndiffs > 0],
      .fns  = ~{
        tsibble::difference(
          x           = .x,
          differences = vars_unit_root$ndiffs[vars_unit_root$variable == dplyr::cur_column()]
          )
        },
      .names = "{.col}_diff"
    )
  )

# Select variables that have temporal precedence in the Granger sense,
# remove NA values from the beginning of the series and fill the end with
# the historical average (for independent variables)
df_trucks_ts_granger <- df_trucks_ts_diff %>%
  dplyr::select(
    dplyr::all_of(
      c(
        "date",
        "trucks",
        var_ygranger,
        names(vars_granger_cause[vars_granger_cause == TRUE])
        )
      )
    ) %>%
  dplyr::filter(!dplyr::row_number() == 1:max(vars_unit_root$ndiffs)) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = !dplyr::any_of(c("date", "trucks", "trucks_diff")),
      .fns  = ~tidyr::replace_na(data = .x, replace = mean(.x, na.rm = TRUE))
    )
  )



# Model specifications ----------------------------------------------------

# Create formulas for the specification of each model

# Dependent variable name
y <- "trucks"

# Independent variable name
xregs <- names(vars_granger_cause[vars_granger_cause == TRUE])


# AR model specifications (based on correlograms)
names_ar <- paste0("ar", dgp_guesses$ar)
specs_ar <- paste0(y, " ~ 1 + order(", dgp_guesses$ar, ")")
formulas_ar <- purrr::map(.x = specs_ar, .f = as.formula)
models_ar <- purrr::set_names(
  x  = purrr::map(.x = formulas_ar, .f = fable::AR),
  nm = names_ar
  )


# MA model specifications (based on correlograms)
names_ma <- paste0("ma", dgp_guesses$ma)
specs_ma <- paste0(y, " ~ 1 + pdq(0, 0, ", dgp_guesses$ma, ") + PDQ(0, 0, 0)")
formulas_ma <- purrr::map(.x = specs_ma, .f = as.formula)
models_ma <- purrr::set_names(
  x  = purrr::map(.x = formulas_ma, .f = fable::ARIMA),
  nm = names_ma
  )


# ETS model specification
names_ets <- "ets"
models_ets <- purrr::set_names(
  x  = c(fable::ETS(!!as.name(y))),
  nm = names_ets
  )


# ARIMA model specifications (based on correlograms)
names_arima <- purrr::map_chr(
  .x = purrr::cross(dgp_guesses),
  .f = ~paste0("arima", .x[1], "_1_", .x[2])
  )
specs_arima <- purrr::map_chr(
  .x = purrr::cross(dgp_guesses),
  .f = ~paste0(y, " ~ 1 + pdq(", .x[1], ", 1, ", .x[2], ") + PDQ(0, 0, 0)")
  )
formulas_arima <- purrr::map(.x = specs_arima, .f = as.formula)
models_arima <- purrr::set_names(
  x  = purrr::map(.x = formulas_arima, .f = fable::ARIMA),
  nm = names_arima
  )


# ARIMA model (automatic)
names_arima_auto <- "arima_auto"
models_arima_auto <- purrr::set_names(
  x  = c(fable::ARIMA(!!as.name(y) ~ PDQ(0, 0, 0))),
  nm = names_arima_auto
  )


# SARIMA model specifications (automatic PQD selection)
names_sarima <- purrr::map_chr(
  .x = purrr::cross(dgp_guesses),
  .f = ~paste0("sarima", .x[1], "_1_", .x[2])
  )
specs_sarima <- purrr::map_chr(
  .x = purrr::cross(dgp_guesses),
  .f = ~paste0(y, " ~ 1 + pdq(", .x[1], ", 1, ", .x[2], ") + PDQ(P = 0:2, D = 0:1, Q = 0:2)")
  )
formulas_sarima <- purrr::map(.x = specs_sarima, .f = as.formula)
models_sarima <- purrr::set_names(
  x  = purrr::map(.x = formulas_sarima, .f = fable::ARIMA),
  nm = names_sarima
  )


# SARIMA model (automatic)
names_sarima_auto <- "sarima_auto"
models_sarima_auto <- purrr::set_names(
  x  = c(fable::ARIMA(!!as.name(y))),
  nm = names_sarima_auto
  )


# ARIMAX model specifications (based on correlograms)
names_arimax <- purrr::map_chr(
  .x = purrr::cross(dgp_guesses),
  .f = ~paste0("arimax", .x[1], "_1_", .x[2])
  )
specs_arimax <- purrr::map_chr(
  .x = purrr::cross(dgp_guesses),
  .f = ~paste0(
    y,
    " ~ 1 + ",
    paste(xregs, collapse = " + "),
    " + pdq(", .x[1], ", 1, ", .x[2], ") + PDQ(0, 0, 0)"
    )
  )
formulas_arimax <- purrr::map(.x = specs_arimax, .f = as.formula)
models_arimax <- purrr::set_names(
  x  = purrr::map(.x = formulas_arimax, .f = fable::ARIMA),
  nm = names_arimax
  )


# ARIMAX model specification (automatic pdq selection)
names_arimax_auto <- "arimax_auto"
formulas_arimax_auto <- as.formula(
  paste0(y, " ~ 1 + ", paste(xregs, collapse = " + "), " + PDQ(0, 0, 0)")
  )
models_arimax_auto <- purrr::set_names(
  x  = c(fable::ARIMA({{ formulas_arimax_auto }})),
  nm = names_arimax_auto
  )


# SARIMAX model specifications (automatic PQD selection)
names_sarimax <- purrr::map_chr(
  .x = purrr::cross(dgp_guesses),
  .f = ~paste0("sarimax", .x[1], "_1_", .x[2])
  )
specs_sarimax <- purrr::map_chr(
  .x = purrr::cross(dgp_guesses),
  .f = ~paste0(
    y, " ~ 1 + ",
    paste(xregs, collapse = " + "),
    " + pdq(", .x[1], ", 1, ", .x[2], ") + PDQ(P = 0:2, D = 0:1, Q = 0:2)"
    )
  )
formulas_sarimax <- purrr::map(.x = specs_sarimax, .f = as.formula)
models_sarimax <- purrr::set_names(
  x  = purrr::map(.x = formulas_sarimax, .f = fable::ARIMA),
  nm = names_sarimax
  )


# SARIMAX model specification (automatic pdq selection)
names_sarimax_auto <- "sarimax_auto"
formulas_sarimax_auto <- as.formula(
  paste0(y, " ~ 1 + ", paste(xregs, collapse = " + "))
  )
models_sarimax_auto <- purrr::set_names(
  x  = c(fable::ARIMA({{ formulas_sarimax_auto }})),
  nm = names_sarimax_auto
  )



# Cross-validation --------------------------------------------------------

# Create cross-validation (CV) dataset
df_trucks_cv <- df_trucks_ts_granger %>%
  dplyr::slice(n = 1:(dplyr::n() - horizon)) %>%
  tsibble::stretch_tsibble(
    .step = 3,
    .init = init_obs
    )

# Create cross-validation test sample
df_trucks_future <- tsibble::new_data(df_trucks_cv, n = horizon) %>%
  dplyr::left_join(df_trucks_ts_granger, by = "date")

# Fit benchmark models
fit_benchmark <- df_trucks_cv %>%
  fabletools::model(
    rw           = fable::RW(trucks),
    rw_drift     = fable::RW(trucks ~ drift()),
    rw_seasonal  = fable::SNAIVE(trucks ~ lag()),
    avg          = fable::MEAN(trucks),
    .safely      = TRUE
    )

# Fit ARIMA/ETS family models
fit_arima <- df_trucks_cv %>%
  fabletools::model(
    !!!models_ar,
    !!!models_ma,
    !!!models_ets,
    !!!models_arima,
    !!!models_arima_auto,
    !!!models_sarima,
    !!!models_sarima_auto,
    !!!models_arimax,
    !!!models_arimax_auto,
    !!!models_sarimax,
    !!!models_sarimax_auto,
    .safely      = TRUE
    )

# Prediction in test samples (benchmark models)
frcst_benchmark <- fit_benchmark %>%
  fabletools::forecast(
    new_data = dplyr::select(df_trucks_future, dplyr::all_of(y))
    ) %>%
  dplyr::group_by(.id, .model) %>%
  dplyr::mutate(h = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  fabletools::as_fable(
    response     = y,
    distribution = dplyr::all_of(y)
    )

# Prediction in test samples (ARIMA/ETS family)
frcst_arima <- fit_arima %>%
  fabletools::forecast(
    new_data = dplyr::select(df_trucks_future, dplyr::all_of(xregs))
  ) %>%
  dplyr::group_by(.id, .model) %>%
  dplyr::mutate(h = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  fabletools::as_fable(
    response     = y,
    distribution = dplyr::all_of(y)
    )

# Estimate VECM models, make predictions and calculate RMSE with cross validation
names_vecm <- paste0("vecm_l", rank_coint$lag, "_r", rank_coint$rank)
fit_vecm <- purrr::pmap(
  .l = as.list(rank_coint),
  .f = function (lag, rank) {
    get_cv_rmse_vecm(
      data        = df_trucks,
      y_target    = "trucks",
      date_col    = "date",
      lags        = lag,
      rank        = rank,
      init_window = init_obs,
      step        = 3,
      horizon     = horizon,
      include     = "both",
      estim       = "ML"
      )
    }
  ) %>%
  purrr::set_names(nm = names_vecm)


# Calculate accuracy metrics by horizon vs. model (benchmark models)
acc_benchmark <- frcst_benchmark %>%
  fabletools::accuracy(df_trucks_ts_granger, by = c("h", ".model"))

# Calculate accuracy metrics by horizon vs. model (ARIMA/ETS family)
acc_arima <- frcst_arima %>%
  fabletools::accuracy(df_trucks_ts_granger, by = c("h", ".model"))

# Save accuracy metrics by horizon vs. model (VECM)
acc_vecm <- purrr::map(.x = fit_vecm, .f = purrr::pluck, "rmse") %>%
  dplyr::bind_rows(.id = ".model") %>%
  dplyr::rename("RMSE" = "rmse")



# Best model --------------------------------------------------------------

# Merge accuracy data from all models
acc_models <- dplyr::bind_rows(acc_benchmark, acc_arima, acc_vecm) %>%
  dplyr::arrange(h, RMSE, .model) %>%
  tidyr::drop_na(RMSE)

# Best models by forecast horizon
best_by_horizon <- acc_models %>%
  dplyr::group_by(h) %>%
  dplyr::slice(1)

# Best model in the 12-18 month horizon
best_overall <- best_by_horizon %>%
  dplyr::filter(h >= 12) %>%
  dplyr::pull(.model) %>%
  table() %>%
  which.max() %>%
  names()

# If the best model found is a benchmark model, stop and indicate for general review.
models_benchmark <- c("rw", "rw_drift", "rw_seasonal", "avg")
if (best_overall %in% models_benchmark) {
  stop("The best model found is a benchmark model. Please review data/models.")
}

# Merge all model definitions (except VECM)
models_specs <- c(
  models_ar,
  models_arima,
  models_arima_auto,
  models_arimax,
  models_ets,
  models_ma,
  models_sarima,
  models_sarima_auto,
  models_sarimax,
  models_arimax_auto,
  models_sarimax_auto
  )

# Save the best model definition
# TODO: implement logic for when the best model is a VECM
if (best_overall %in% names_vecm) {
  stop("The best model found is a VECM. Please review the later code.")
}
best_definition <- purrr::pluck(models_specs, best_overall)


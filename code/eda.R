# This script performs an Exploratory Data Analysis (EDA)


# General steps -----------------------------------------------------------

# Pivot table to long form
df_trucks_long <- df_trucks %>%
  tidyr::pivot_longer(
    cols      = !dplyr::all_of("date"),
    names_to  = "variable",
    values_to = "value"
    )

# Convert data to tsibble class
df_trucks_ts <- df_trucks %>%
  dplyr::mutate(date = tsibble::yearmonth(date)) %>%
  tsibble::as_tsibble(index = date)

# Set default ggplot2 theme
ggplot2::theme_set(theme_trucks())



# Line charts -------------------------------------------------------------

# Line chart of the dependent variable
plot_y <- df_trucks %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = date, y = trucks) +
  ggplot2::geom_line(size = 1, color = "#282f6b") +
  ggplot2::scale_x_date(
    breaks = df_trucks %>%
      dplyr::filter(dplyr::row_number() %% 15 == 1) %>%
      dplyr::pull(date),
    labels = ym_label
    ) +
  ggplot2::scale_y_continuous(
    breaks = scales::extended_breaks(n = 7),
    labels = scales::label_number()
    ) +
  ggplot2::labs(title = "Number of Trucks Sold", y = NULL, x = NULL)
plot_y

# Line graph of the other variables
plot_xregs <- df_trucks_long %>%
  dplyr::filter(!variable == "trucks") %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = date, y = value) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(
    facets = ggplot2::vars(variable),
    scales = "free"
    ) +
  ggplot2::scale_y_continuous(
    breaks = scales::extended_breaks(n = 4),
    labels = scales::label_number()
    ) +
  ggplot2::labs(title = "Other variables", y = NULL, x = NULL)
plot_xregs



# Descriptive statistics --------------------------------------------------

# Measures of centrality, dispersion, range, skewness, kurtosis, etc.
table_stats <- df_trucks %>%
  dplyr::select(-"date") %>%
  datawizard::describe_distribution()
table_stats

# Correlation matrix
plot_corr <- GGally::ggpairs(
  data    = tidyr::drop_na(df_trucks),
  columns = 2:ncol(df_trucks)
  )
plot_corr



# Histograms --------------------------------------------------------------

# Histogram graphs
plot_hist <- df_trucks_long %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = value) +
  ggplot2::geom_histogram() +
  ggplot2::facet_wrap(
    facets = ggplot2::vars(variable),
    scales = "free"
    ) +
  ggplot2::scale_y_continuous(
    breaks = scales::extended_breaks(n = 4)
    ) +
  ggplot2::labs(title = "Histograms", y = NULL, x = NULL)
plot_hist



# Seasonality -------------------------------------------------------------

# Decompose Trucks time series into seasonal, trend and remainder components
plot_stl_trucks <- df_trucks_ts %>%
  fabletools::model(feasts::STL(trucks, robust = TRUE), .safely = TRUE) %>%
  fabletools::components() %>%
  fabletools::autoplot() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "grey90")) +
  ggplot2::labs(x = NULL)
plot_stl_trucks

# Seasonality graph
plot_season <- feasts::gg_season(data = df_trucks_ts, y = trucks) +
  ggplot2::labs(
    title = "Monthly seasonality",
    y     = "# Trucks Sold",
    x     =  NULL
    )
plot_season

# Applies the Kruskall Wallis (KW) and modified QS tests to detect seasonality
vars_seas <- df_trucks %>%
  dplyr::select(-"date") %>%
  purrr::map_lgl(
    .f = ~seastests::isSeasonal(
      x    = na.omit(.x),
      freq = 12,
      test = "combined"
    )
  )
vars_seas



# Stationarity ------------------------------------------------------------

# Apply unit root tests (KPSS, PP and ADF) to determine number of differences
# required for a series be stationary
vars_unit_root <- df_trucks %>%
  dplyr::select(-"date") %>%
  report_ndiffs()
vars_unit_root
diffs_trucks <- vars_unit_root$ndiffs[vars_unit_root$variable == "trucks"]

if (diffs_trucks > 1) {
  stop(
    paste0(
      "Dependent variable needs more than 1 difference to be stationary. ",
      "Please review data/models."
      )
    )
  }

# Apply STL decomposition to determine number of seasonal differences
# required for a series be stationary
diffs_trucks_seas <- feasts::unitroot_nsdiffs(df_trucks$trucks)
diffs_trucks_seas



# Autocorrelation ---------------------------------------------------------

# ACF and PACF correlograms
plot_correlograms <- feasts::gg_tsdisplay(
  data      = df_trucks_ts,
  plot_type = "partial",
  y         = if (diffs_trucks == 0 && diffs_trucks_seas == 0) {
    trucks
    } else if (diffs_trucks > 0 && diffs_trucks_seas == 0) {
      tsibble::difference(trucks, differences = diffs_trucks)
      } else if (diffs_trucks == 0 && diffs_trucks_seas > 0) {
        tsibble::difference(trucks, differences = diffs_trucks_seas, lag = 12)
      }
  ) +
  ggplot2::labs(title = "ACF and PACF correlograms")
plot_correlograms

# Defines possible ARMA processes (based on correlograms)
dgp_guesses <- list(
  ar = c(1, 2, 5, 6, 9, 10, 12, 22),
  ma = c(1, 2, 5, 6, 11, 12, 18, 22)
  )



# Granger Causality -------------------------------------------------------

# Convert data to mts class (applying differences in series when necessary)
df_trucks_mts <- df_trucks_ts %>%
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
    ) %>%
  tsibble::as_tibble() %>%
  dplyr::select(
    dplyr::all_of(
      c(
        vars_unit_root$variable[vars_unit_root$ndiffs == 0],
        paste0(vars_unit_root$variable[vars_unit_root$ndiffs > 0], "_diff")
        )
      )
    ) %>%
  dplyr::relocate(
    dplyr::if_else(diffs_trucks == 0, "trucks", "trucks_diff")
    ) %>%
  stats::ts(
    start = c(
      lubridate::year(min(df_trucks$date)),
      lubridate::month(min(df_trucks$date))
      ),
    frequency = 12
    )

# Possible predictor variables (x in the Granger test)
var_xgranger <- colnames(df_trucks_mts)[!colnames(df_trucks_mts) %in% c("trucks", "trucks_diff")]
var_ygranger <- dplyr::if_else(diffs_trucks == 0, "trucks", "trucks_diff")

# Apply Granger-Causality Test with p-value 0.05
# (TRUE = H0 rejected, x variable Granger-causes y variable)
vars_granger_cause <- purrr::map(
  .x = var_xgranger,
  .f = ~lmtest::grangertest(
    y     = df_trucks_mts[, var_ygranger],
    x     = df_trucks_mts[, .x],
    order = 5
    )
  ) %>%
  purrr::set_names(nm = var_xgranger) %>%
  purrr::map_lgl(~purrr::pluck(.x, 4, 2)  < 0.05)
vars_granger_cause



# Cointegration -----------------------------------------------------------

# Convert data to mts class
df_trucks_johansen <- df_trucks %>%
  dplyr::select(
    dplyr::all_of(
      c(
        "trucks",
        vars_granger_cause[vars_granger_cause == TRUE] %>%
          names() %>%
          stringr::str_remove("_diff")
        )
      )
    ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = !dplyr::any_of(c("date", "trucks")),
      .fns  = ~tidyr::replace_na(data = .x, replace = mean(.x, na.rm = TRUE))
      )
    ) %>%
  stats::ts(
    start = c(
      lubridate::year(min(df_trucks$date)),
      lubridate::month(min(df_trucks$date))
      ),
    frequency = 12
    )

# Johansen Cointegration Test: testing for multiple lags and cointegration
# relationships and keeping results for rank >= 1 or less than ncol(data)
rank_coint <- purrr::map(
  .x = dgp_guesses$ar + 1, # of lags
  .f = ~purrr::map(
    .x = c("r" = 1:3),     # of cointegrating relationships
    .f = function (y) {
      tsDyn::rank.test(
        tsDyn::VECM(
          data    = df_trucks_johansen,
          lag     = .x,
          r       = y,
          include = "both",
          estim   = "ML"
          )
        )$r
      }
    )
  ) %>%
  purrr::set_names(nm = paste0("lag", dgp_guesses$ar + 1)) %>%
  purrr::map(
    .f = ~purrr::keep(.x = .x, .p = ~ .x >= 1 & .x < ncol(df_trucks_johansen))
  ) %>%
  purrr::compact() %>%
  dplyr::bind_rows(.id = "lag") %>%
  dplyr::rowwise() %>%
  dplyr::summarise(
    lag  = as.integer(stringr::str_remove(lag, "lag")),
    rank = as.integer(names(which.max(table(dplyr::c_across(r1:r3)))))
    )

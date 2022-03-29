# This script centralizes some user defined functions



#' Count the number of missing values
#'
#' Function to count the number of missing values in the columns of a data frame.
#'
#' @param df A data frame
#'
#' @return A data frame
#'
#' @examples
#' count_na(iris)
count_na <- function(df) {

  if (!is.data.frame(df)) {
    stop("df must be a data.frame.")
  }

  n_nas <- purrr::map_dbl(
    .x = df,
    .f = ~sum(is.na(.x))
    )

  out <- dplyr::tibble(
    variable = names(n_nas),
    na_count = n_nas
    )

  return(out)

}


#' Report number of differences to make time series stationary (vectorized)
#'
#' @param x List-like object with vectors of the series to be tested
#' @param na_rm Remove NAs from x?
#' @param ... Additional arguments to be passed on to the unit root test
#'
#' @return Tibble with variable name from x and the number of differences found
#'
#' @examples
#' report_ndiffs(as.data.frame(datasets::EuStockMarkets))
report_ndiffs <- function(x, na_rm = TRUE, ...){

  ndiffs_tests <- list(
    test = c("kpss", "pp", "adf"),
    type = c("level", "trend")
    )

  ndiffs_cross <- purrr::cross(ndiffs_tests)

  purrr::map(
    .x = if (na_rm) { na.omit(x) } else x,
    .f = ~purrr::map(
      .x = ndiffs_cross,
      .f = function(y){
        forecast::ndiffs(
          x     = .x,
          alpha = 0.05,
          test  = y[[1]],
          type  = y[[2]],
          ...
          )
        }
      )
    ) %>%
    purrr::map_df(~purrr::reduce(.x = .x, .f = c)) %>%
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns  = ~{
          .x %>%
            table() %>%
            which.max() %>%
            names() %>%
            as.integer()
        }
      )
    ) %>%
    tidyr::pivot_longer(
      cols      = -NULL,
      names_to  = "variable",
      values_to = "ndiffs"
    )

}



# Function to deseasonalize monthly data using X-13-ARIMA-SEATS
# x = série a ser ajustada sazonalmente
# first_date = data de início da série (formato "YYYY-MM-DD")


#' X-13ARIMA-SEATS Seasonal Adjustment
#'
#' @param x Numerical vector of the time series.
#' @param date_col year-month (tsibble::yearmonth) vector.
#'
#' @return Numerical vector of the seasonally adjusted time series, if it was possible to fit a model, else NULL.
x13_adjust <- function(x, date_col){

  # Prepare tsibble object
  x13_fit <- tsibble::tsibble(
    date  = date_col,
    value = x,
    index = date
    ) %>%
    fabletools::model(x13 = feasts::X_13ARIMA_SEATS(value), .safely = TRUE)

  if (
    # Return original data if generated null model
    fabletools::is_null_model(x13_fit[[1]][[1]][["fit"]]) ||

    # Return original data if generated model without fit
    is.null(x13_fit[[1]][[1]][["fit"]][["fit"]]) ||

    # Return original data if seasonality is zero
    !all(x13_fit[[1]][[1]][["fit"]][["fit"]][["data"]][,"seasonal"] != 0L)
    ) {

    warning(
      paste0(
        "Unable to perform X-13ARIMA-SEATS Seasonal Adjustment for variable ",
        dplyr::cur_column(),
        ", returning NULL."
        ),
      call. = FALSE
      )
    return(NULL)

    } else {

    return(
      x13_fit %>%
        fabletools::components() %>%
        dplyr::as_tibble() %>%
        dplyr::pull(season_adjust)
      )

  }
}



#' VECM: estimates model with cross-validation and reports accuracy by forecast horizon
#'
#' @param data A data frame
#' @param y_target Column name of the variable of interest (used to report accuracy)
#' @param date_col Date class column name
#' @param lags Number of lags (see tsDyn::VECM)
#' @param rank Number of cointegrating relationships (see tsDyn::VECM)
#' @param init_window Number of initial observations to be used in the first cross-validation subsample
#' @param step A positive integer for incremental step (see tsibble::stretch_tsibble)
#' @param horizon Forecast horizon
#' @param ... Additional arguments to tsDyn::VECM
#'
#' @return A list with 3 elements: a tibble with the RMSE per forecast horizon; a list with prediction points in each cross-validation subsample; and the original observed data.
get_cv_rmse_vecm <- function (
  data,
  y_target,
  date_col,
  lags        = 3,
  rank        = 1,
  init_window = 180,
  step        = 3,
  horizon     = 18,
  ...
  ) {

  cv_train_index <- data %>%
    dplyr::slice(1:(dplyr::n() - horizon)) %>%
    nrow() %>%
    seq_len() %>%
    tsibble:::stretcher2(.step = step, .init = init_window)

  n_fcst <- length(cv_train_index)

  point_fcst <- list()

  for (i in seq_len(n_fcst)) {

    cat(paste0("\n\nIteration: ", i, "/", n_fcst, "\n\n"))

    curr_index <- cv_train_index[[i]]
    fit_index <- curr_index[1:(length(curr_index) - (lags + 1))]
    test_index <- seq.int(
      from       = max(fit_index) + 1,
      by         = 1,
      length.out = lags + 1
      )

    data_train <- data[curr_index, ]
    data_test <- data[test_index, ]

    fit_vecm <- tsDyn::VECM(
      data    = dplyr::select(data_train, -dplyr::all_of(date_col)),
      lag     = lags,
      r       = rank,
      ...
      )

    fcsts <- as.vector(
      predict(
        fit_vecm,
        newdata = dplyr::select(data_test, -dplyr::all_of(date_col)),
        n.ahead = horizon
        )[, y_target]
      )

    point_fcst[[i]] <- dplyr::tibble(
      {{ date_col }} := seq.Date(
        from       = min(dplyr::pull(data_test, {{ date_col }})),
        by         = "month",
        length.out = length(fcsts)
        ),
      fcst = fcsts
      )

    }

  data_y <- dplyr::tibble(
    {{ date_col }} := dplyr::pull(data, {{ date_col }}),
    {{ y_target }} := dplyr::pull(data, {{ y_target }})
    )

  rmse_tbl <- dplyr::left_join(
    dplyr::bind_rows(point_fcst, .id = ".id"),
    data_y,
    by = "date"
    ) %>%
    dplyr::group_by(.id) %>%
    dplyr::mutate(h = dplyr::row_number()) %>%
    dplyr::group_by(h) %>%
    dplyr::summarise(
      rmse = sqrt(mean((!!rlang::sym(y_target) - fcst)^2, na.rm = TRUE)),
      .groups = "drop"
      )

  return(
    list(
      "rmse"     = rmse_tbl,
      "forecast" = purrr::set_names(point_fcst, paste0("id", 1:n_fcst)),
      "actual"   = data_y
      )
    )

}



#' Default theme customization for ggplot2 plots
#'
#' @return ggplot2 theme
theme_trucks <- function() {
  ggthemes::theme_fivethirtyeight() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 18, colour = "#282f6b"),
      axis.title = ggplot2::element_text(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}


#' Beautiful x-axis date labels for monthly time series
#'
#' To be used in ggplot2::scale_x_date().
#'
#' @param x A Date/POSIXct vector (breaks).
#'
#' @return A well-formatted Date/POSIXct vector.
ym_label <- function(x) {

  x <- lubridate::as_date(x)

  dplyr::if_else(
    is.na(dplyr::lag(x)) | tsibble::yearmonth(dplyr::lag(x)) != tsibble::yearmonth(x),
    paste(lubridate::month(x, label = TRUE), "\n", lubridate::year(x)),
    paste(lubridate::month(x, label = TRUE))
  )

}

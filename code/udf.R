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

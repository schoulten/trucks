# This script prepares the models output data and creates data visualizations


# General steps -----------------------------------------------------------

# Mean forecast point data and confidence intervals
frcst_tbl <- frcst_best %>%
  fabletools::hilo() %>%
  fabletools::unpack_hilo(c("80%", "95%")) %>%
  tsibble::as_tibble() %>%
  dplyr::select(
    "date",
    {{ y_best }} := ".mean",
    dplyr::contains("%")
    ) %>%
  dplyr::mutate(date = lubridate::as_date(date))

# Model fitted values data
fitted_tbl <- fit_best %>%
  fabletools::augment() %>%
  tsibble::as_tibble() %>%
  dplyr::select("date", "fitted" = ".fitted") %>%
  dplyr::mutate(date = lubridate::as_date(date))

# Join observed data with model data
fanchart_tbl <- df_trucks_ts_granger %>%
  tsibble::as_tibble() %>%
  dplyr::select("date", dplyr::all_of(y_best)) %>%
  dplyr::mutate(date = lubridate::as_date(date)) %>%
  dplyr::bind_rows(frcst_tbl) %>%
  dplyr::left_join(fitted_tbl, by = "date")

# Organize accuracy data
acc_tbl <- best_by_horizon %>% # best models by horizon
  dplyr::ungroup() %>%
  dplyr::mutate(
    .model = .model %>%
      stringr::str_to_upper() %>%
      stringr::str_replace_all("([RAOXSM])(\\d{1,2})", "\\1\\(\\2") %>%
      stringr::str_replace_all("_", ", ") %>%
      stringr::str_replace_all("(\\d$)", "\\1)") %>%
      stringr::str_replace_all("AVG", "MEAN")
    ) %>%
  dplyr::arrange(h, RMSE, .model) %>%
  dplyr::rename("Horizon" = "h", "Model" = ".model") %>%
  dplyr::select(-".type")

acc_full_tbl <- acc_models %>% # all models
  dplyr::mutate(
    .model = .model %>%
      stringr::str_to_upper() %>%
      stringr::str_replace_all("(_L)", " (L ") %>%
      stringr::str_replace_all("(_R)", ", R ") %>%
      stringr::str_replace_all("([RAOXSM])(\\d{1,2})", "\\1\\(\\2") %>%
      stringr::str_replace_all("_AUTO", " AUTO") %>%
      stringr::str_replace_all("_DRIFT", " w/ DRIFT") %>%
      stringr::str_replace_all("_SEASONAL", " SEASONAL") %>%
      stringr::str_replace_all("_", ", ") %>%
      stringr::str_replace_all("(\\d$)", "\\1)") %>%
      stringr::str_replace_all("AVG", "MEAN")
    ) %>%
  dplyr::arrange(h, RMSE, .model) %>%
  dplyr::rename("Horizon" = "h", "Model" = ".model") %>%
  dplyr::select(-".type")

# Last month of observed data
obs_max_date <- max(lubridate::as_date(df_trucks_ts_granger$date))

# Color codes for plots
colors <- c(
  "#282f6b", # blue
  "#b22200", # red
  "#eace3f", # yellow
  "#224f20", # green
  "#000000",  # black
  "#5f487c", # purple
  "#b35c1e", # orange
  "#419391", # turquoise
  "#839c56", # light green
  "#3b89bc", # light blue
  "#666666"  # gray
  )



# Fanchart plot -----------------------------------------------------------

# Out-of-sample forecast plot (fanchart)
plot_fanchart <- fanchart_tbl %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = date) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = `80%_lower`, ymax = `80%_upper`, fill  = "80% (CI)", alpha = 0.5),
    data  = dplyr::filter(fanchart_tbl, date >= obs_max_date)
    ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = `95%_lower`, ymax = `95%_upper`, fill  = "95% (CI)", alpha = 0.5),
    data  = dplyr::filter(fanchart_tbl, date >= obs_max_date)
    ) +
  ggplot2::geom_line(
    ggplot2::aes(y = trucks, color = "# Trucks Sold"),
    size = 2,
    data  = dplyr::filter(fanchart_tbl, date <= obs_max_date)
    ) +
  ggplot2::geom_line(
    ggplot2::aes(y = trucks, color = "Forecast"),
    size = 2,
    data  = dplyr::filter(fanchart_tbl, date >= obs_max_date)
    ) +
  ggplot2::geom_line(
    ggplot2::aes(y = fitted, color = "Fitted"),
    size = 1,
    data  = dplyr::filter(fanchart_tbl, date <= obs_max_date)
    ) +
  ggplot2::geom_vline(xintercept = obs_max_date, linetype = "dashed") +
  ggplot2::scale_y_continuous(
    breaks = scales::breaks_pretty(n = 8),
    labels = scales::label_number()
    ) +
  ggplot2::scale_x_date(
    breaks = scales::breaks_width("15 months"),
    labels = ym_label
    ) +
  ggplot2::scale_fill_manual(
    values = c("95% (CI)" = "#282f6b40", "80% (CI)" = "#282f6b90"),
    breaks = c("80% (CI)", "95% (CI)")
    ) +
  ggplot2::scale_color_manual(
    values = c(
      "# Trucks Sold" = colors[5],
      "Forecast" = colors[1],
      "Fitted" = colors[2]
      ),
    breaks = c("# Trucks Sold", "Fitted", "Forecast"),
    guide = ggplot2::guide_legend(
      order = 1,
      override.aes = list(size = c(2, 1, 2))
      )
    ) +
  ggplot2::guides(alpha = "none") +
  ggplot2::labs(
    title    = "**Fanchart**: Trucks Sold Forecast",
    subtitle = paste0(best_definition$model, " model, 80% and 95% confidence intervals"),
    y        = "# Trucks Sold",
    x        = NULL,
    color    = NULL,
    fill     = NULL,
    caption  = "**Author**: Fernando da Silva (fernando @ fortietwo.com)"
    ) +
  ggplot2::theme_light() +
  ggplot2::theme(
    plot.title       = ggtext::element_markdown(size = 25, colour = colors[1]),
    plot.subtitle    = ggtext::element_markdown(size = 16),
    axis.text        = ggtext::element_markdown(size = 12, face = "bold"),
    axis.title       = ggtext::element_markdown(size = 12, face = "bold"),
    panel.grid.minor = ggplot2::element_blank(),
    legend.text      = ggplot2::element_text(size = 12, face = "bold"),
    legend.position  = "bottom",
    plot.caption     = ggtext::element_textbox_simple(
      size = 12,
      colour = "grey20",
      margin = ggplot2::margin(10, 5.5, 10, 5.5)
      )
    )

plot_fanchart



# Accuracy plot -----------------------------------------------------------

# Accuracy metric (RMSE) plot by predictive horizon versus model
plt_rmse <- ggplot2::ggplot() +
  ggplot2::geom_rect(
    ggplot2::aes(xmin = 12, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = colors[1],
    alpha = 0.3
    ) +
  ggplot2::geom_point(
    data = acc_tbl,
    ggplot2::aes(x = `Horizon`, y = `RMSE`, color = `Model`),
    size = 4
    ) +
  ggplot2::scale_color_manual(values = colors) +
  ggplot2::scale_y_continuous(
    breaks = scales::breaks_pretty(n = 8),
    labels = scales::label_number()
    ) +
  ggplot2::scale_x_continuous(
    labels = scales::number_format(accuracy = 1),
    breaks = 1:horizon
    ) +
  ggplot2::theme_light() +
  ggplot2::labs(
    title    = "**Accuracy**: best model by horizon",
    subtitle = "Forecast models for the # of Trucks Sold, selected metric",
    x        = "Forecast horizon (months)",
    fill     = NULL,
    color    = NULL,
    caption  = "**Author**: Fernando da Silva (fernando @ fortietwo.com)"
    ) +
  ggplot2::theme(
    plot.title       = ggtext::element_markdown(size = 25, colour = colors[1]),
    plot.subtitle    = ggtext::element_markdown(size = 16),
    axis.text        = ggtext::element_markdown(size = 12, face = "bold"),
    axis.title       = ggtext::element_markdown(size = 12, face = "bold"),
    panel.grid.minor = ggplot2::element_blank(),
    legend.text      = ggplot2::element_text(size = 12, face = "bold"),
    legend.position  = "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    plot.caption     = ggtext::element_textbox_simple(
      size   = 12,
      colour = "grey20",
      margin = ggplot2::margin(10, 5.5, 10, 5.5)
    )
  )

plt_rmse



# Point forecast table ----------------------------------------------------

# Table of forecast points and confidence intervals
withr::with_locale(
  new = c("LC_TIME" = "C"),
  code = frcst_dt <- frcst_tbl %>%
    dplyr::mutate(date = format(date, "%Y %b")) %>%
    dplyr::relocate(
      "Date" = "date",
      "95% lower" = "95%_lower",
      "80% lower" = "80%_lower",
      "Forecast" = "trucks",
      "80% upper" = "80%_upper",
      "95% upper" = "95%_upper"
      ) %>%
    DT::datatable(
      options = list(dom = "tip", pageLength = 5, scrollX = TRUE, scrollY = TRUE),
      rownames = FALSE
      ) %>%
    DT::formatRound(columns = 2:6, digits = 0, mark = " ") %>%
    DT::formatStyle(columns = 4, fontWeight = "bold")
  )

frcst_dt


# Table of accuracy metrics for all models
acc_dt <- acc_full_tbl %>%
  DT::datatable(
    rownames = FALSE,
    options = list(pageLength = 3, scrollX = TRUE, scrollY = TRUE)
  ) %>%
  DT::formatRound(
    columns  = 3:ncol(acc_full_tbl),
    digits   = 2,
    dec.mark = ".",
    mark     = " "
    )

acc_dt



# Dashboard ---------------------------------------------------------------

# Renders dashboard with top results
rmarkdown::render(
  input       = "trucks_forecast.Rmd",
  output_file = "index.html"
  )


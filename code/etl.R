# This script is responsible for import, tidy and saving the data.


# Import ------------------------------------------------------------------

# Import the Excel spreadsheet
raw_data <- readxl::read_xlsx(
  path      = "./data/Data_Fernando.xlsx",
  sheet     = "Data",
  col_names = TRUE,
  col_types = c("date", rep("numeric", 11))
  )



# Tidy --------------------------------------------------------------------

# Rename columns and transform date column to ISO 8601 standard
df_trucks <- raw_data %>%
  dplyr::rename(
    dplyr::all_of(
      c(
        "date"         = "Date",
        "trucks"       = "Trucks",
        "ibc_br"       = "IBC-BR",
        "ind_prod"     = "Industrial Production",
        "retail_sales" = "Retail Sales",
        "credit"       = "Business Credit Concessions",
        "confidence"   = "Business Confidence Index",
        "commodity"    = "Commodity Price Index",
        "usd_brl"      = "USD/BRL",
        "ind_employed" = "Index of Employed Persons - Industry",
        "int_rate"     = "Base Interest Rate",
        "uncertainty"  = "Uncertainty Index"
        )
      )
    ) %>%
  dplyr::mutate(date = lubridate::as_date(date))



# Save --------------------------------------------------------------------

# Save processed data (for tracking purposes)
readr::write_rds(
  x    = df_trucks,
  file = paste0("./data/data_tidy_", lubridate::today(), ".rds")
  )

# This script automates all the steps of the model

# Should the results appear in the Console?
print_outputs <- TRUE


# Packages ----------------------------------------------------------------
source(file = file.path("./code/load_packages.R"), print.eval = print_outputs)


# UDF ---------------------------------------------------------------------
source(file = "./code/udf.R", print.eval = print_outputs)


# ETL ---------------------------------------------------------------------
source(file = "./code/etl.R", print.eval = print_outputs)


# EDA ---------------------------------------------------------------------
source(file = "./code/eda.R", print.eval = print_outputs)


# Modeling ----------------------------------------------------------------
source(file = "./code/modeling.R", print.eval = print_outputs)


# Forecasting -------------------------------------------------------------
source(file = "./code/forecasting.R", print.eval = print_outputs)


# Report ------------------------------------------------------------------
source(file = "./code/report.R", print.eval = print_outputs)



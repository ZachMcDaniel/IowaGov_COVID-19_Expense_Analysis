rm(list = ls())

###############################################################################
#Downloading and Cleaning Data
###############################################################################

download.file('https://data.iowa.gov/api/views/68ay-29f3/rows.csv', "expense_data.csv")
covid_df <- readr::read_csv("expense_data.csv")

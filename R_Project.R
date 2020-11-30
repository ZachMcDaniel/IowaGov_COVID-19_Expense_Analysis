rm(list = ls())

###############################################################################
#Downloading and Cleaning Data
###############################################################################

download.file('https://data.iowa.gov/api/views/68ay-29f3/rows.csv', "expense_data.csv")
covid_df <- readr::read_csv("expense_data.csv")

#Removing superfluous data
covid_df <- dplyr:: select(covid_df, -`Record ID`, -`State Department Number`, -Unit, 
                           -`Letter of Credit`, -`Fund Code`, -`Object Class`)
#setting col types
covid_df$`Record Date` <- as.Date(covid_df$`Record Date`, format = "%m/%d/%Y")
covid_df$`Budget FY` <- as.integer(covid_df$`Budget FY`)
covid_df$`Fiscal Period` <- as.integer(covid_df$`Fiscal Period`)
covid_df$Amount <- as.numeric(covid_df$Amount)
covid_df$`Federal Department` <- as.factor(covid_df$`Federal Department`)
covid_df$`Federal Agency` <- as.factor(covid_df$`Federal Agency`)
covid_df$`Federal Program Title` <- as.factor(covid_df$`Federal Program Title`)
covid_df$`State Department` <- as.factor(covid_df$`State Department`)
covid_df$`Unit Name` <- as.factor(covid_df$`Unit Name`)
covid_df$Fund <- as.factor(covid_df$Fund)
covid_df$`Object Class Name` <- as.factor(covid_df$`Object Class Name`)

#Filter to all data after march
covid_df <- dplyr:: filter(covid_df, `Record Date` > '2020-03-01')

save(covid_df, file = "covid_df.R")





###############################################################################
#Function
###############################################################################

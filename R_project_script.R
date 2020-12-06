##Data Programming in R, Group 5 Project

rm(list = ls())

###############################################################################
#Libraries
###############################################################################

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(reshape2))

###############################################################################
#Downloading and Cleaning Data
###############################################################################
##Iowa government COVID-19 expenditure data was collected from iowadata.gov and 
#is public data

download.file('https://data.iowa.gov/api/views/68ay-29f3/rows.csv', "expense_data.csv")
covid_df <- read_csv("expense_data.csv")

#Removing superfluous data
covid_df <- select(covid_df, -`Record ID`, -`State Department Number`, -Unit, 
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
covid_df <- filter(covid_df, `Record Date` > '2020-03-01')

write_csv(covid_df, "expense_data.csv")

###############################################################################
#Function
###############################################################################

##function to see how much each Federal Department spent in a specified time period

timeline <- function(start = NA, end = NA){ ## set NA as default start and end date
  time <- covid_df ## create new data frame for function
  msg <- sprintf("Summing all department expenses from %s to %s", start, end)
  message(msg)
  #handling errors that are not dates
  my.err.handler <- function(error) NA
  start <- tryCatch(as.Date(start), error = my.err.handler)
  end <- tryCatch(as.Date(end), error = my.err.handler)
  
  #Start and end dates
    if(!is.na(start))
      time <- time[as.Date(start) <= time$`Record Date`, ] ## delete any data before the start date
    if(!is.na(end))
      time <- time[time$`Record Date` <= as.Date(end), ]   ## delete any data after the end date
  
  pivot <-                                   ## create pivot table
    group_by(time, `Federal Department`)  %>%  ## Sections by federal department
    summarize(amount = sum(Amount))       %>%  ## find sum of amount spent by each department
    dcast(`Federal Department` ~ "Total Amount", value.var = "amount")  ##show the department name and total they spent
  
  pivot <- arrange(pivot, desc(`Total Amount`)) ##list in descending order of total amount spent
  print(pivot)
}

#Function Examples
timeline(start = "2020-03-05", end = "2020-06-05")
timeline(end = "2020-06-05", "2020-03-05")
timeline(start = "2020-07-01")

###############################################################################
#Summary Tables - Fund & Amount
###############################################################################

#Checked number of levels of funds to determine how many funds are present in the data frame
levels(covid_df$Fund)

#Checked minimum and maximum amounts to find ranges of contributions
max(covid_df$Amount)
min(covid_df$Amount)

#Created a Bar graph of Amount vs Fund to get a visualization of Fund contributions
fund_plot <- qplot(Amount, Fund, data = covid_df, geom = "point", color = Amount) +
  ggtitle("Fund Expenses")
ggsave(filename = "amount_by_fund.png", plot = fund_plot, height = 8, 
       width = 12, dpi = 600)

#Create a summary tables to find the number of payments each fund contributed
fund_payments <-
  group_by(covid_df, Fund, Amount)      %>%
  summarize(amount = sum(Amount))       %>%
  dcast(Fund ~ "Total Payments", value.var = "amount")
fund_payments <- arrange(fund_payments, `Total Payments`)

#Create summary table to find the total dollar amount each fund contributed to relief
fund_dollar_amounts <-
  group_by(covid_df, Fund)  %>%
  summarize(amount = sum(Amount))  %>%
  dcast(Fund ~ "Total Amount", value.var = "amount") 
fund_dollar_amounts <- arrange(fund_dollar_amounts, `Total Amount`)


################################################################################
#Graphs - Federal Departments & Agencies
################################################################################


##Percent of Requests by department top 5 scatter plot
df <- covid_df
df<- group_by(df, `Federal Department`)

department_tb <- count(df)
department_tb$percent <- department_tb$n/sum(department_tb$n)


#Getting top 5 departments by percent of requests
top_5_departments <- arrange(department_tb, desc(percent)) %>%
  head(department_tb, n = 5) %>%
  group_by(`Federal Department`)

#scatter plot of percent on department
p <- qplot(`Federal Department`, percent , data = top_5_departments, geom = "point" ) + 
  scale_y_continuous(name = "Percent of Requests", limits = c(0,1))   +
  theme(axis.text.x = element_text(angle = -45))

#saving plot of top 5 department by % of requests
ggsave(filename = "requests_by_department.png", plot = p, height = 8, 
       width = 12, dpi = 600)


#Graphs - amount by department over time
###############################################################################
### GRAPHS ARE LABELED IN ASCENDING ORDER OF THE Y-AXIS SCALE
#Graphs of Amount by Department
covid_df <- ungroup(covid_df)

#$10,000 Scale
p <- qplot(`Record Date`,Amount, data = covid_df, geom = "point",
           color = `Federal Department`) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(0,10000))+
  ggtitle("Department Expenses From Mar 2020 - Nov 2020")

ggsave(filename = "Amount_by_Department_1.png", plot = p, height = 8, 
       width = 12, dpi = 600)

#$1,000,000 scale
p <- qplot(`Record Date`,Amount, data = covid_df, geom = "point",
           color = `Federal Department`) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(0,1000000))+
  ggtitle("Department Expenses From Mar 2020 - Nov 2020")

ggsave(filename = "Amount_by_Department_2.png", plot = p, height = 8, 
       width = 12, dpi = 1200)

#$100,000,000 Scale
p <- qplot(`Record Date`,Amount, data = covid_df, geom = "point",
           color = `Federal Department`) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(0,100000000))+
  ggtitle("Department Expenses From Mar 2020 - Nov 2020")

ggsave(filename = "Amount_by_Department_3.png", plot = p, height = 8, 
       width = 12, dpi = 600)

#Full Scale
p <- qplot(`Record Date`,Amount, data = covid_df, geom = "point",
           color = `Federal Department`) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(-300000000,300000000))

ggsave(filename = "Amount_by_Department_4.png", plot = p, height = 8, 
       width = 12, dpi = 600)


#Federal Department Analysis - Health and Human Services
###############################################################################
### GRAPHS ARE LABELED IN ASCENDING ORDER OF THE Y-AXIS SCALE

df <- subset(covid_df, `Federal Department` == "U.S. Department of Health and Human Services")

### Amount by Agency within the U.S Health and Human Services Department

#scale of $10,000
p <- qplot(`Record Date`,Amount, data = df, geom = "point",
           color = `Federal Agency`, alpha = I(.75)) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(0,10000)) 
 

ggsave(filename = "HHS_Department_Agencies_1.png", plot = p, height = 8, 
       width = 12, dpi = 600)


#scale of $1,000,000
p <- qplot(`Record Date`,Amount, data = df, geom = "point",
           color = `Federal Agency`, alpha = I(.75)) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(0,1000000)) +
  ggtitle("Department of Health and Human Services Agency Expenses")
  

ggsave(filename = "HHS_Department_Agencies_2.png", plot = p, height = 8, 
       width = 12, dpi = 600)


#scale of $10,000,000
p <- qplot(`Record Date`,Amount, data = df, geom = "point",
           color = `Federal Agency`, alpha = I(.75)) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(0,10000000))

ggsave(filename = "HHS_Department_Agencies_3.png", plot = p, height = 8, 
       width = 12, dpi = 600)


#Department of Labor Analysis
###############################################################################

df <- filter(covid_df, `Federal Department` == "U.S. Department of Labor")

#Scale of $10,000,000
p <- qplot(`Record Date`,Amount, data = df, geom = "point",
           color = `Federal Agency`, alpha = I(.75)) +
  scale_y_continuous(name = "Amount", labels = dollar, limits = c(0,10000000))

ggsave(filename = "Labor_Department_Agencies_1.png", plot = p, height = 8, 
       width = 12, dpi = 600)


#Graphs - Department of Treasury - amount by unit within Department of Treasury
###############################################################################
### GRAPHS ARE LABELED IN ASCENDING ORDER OF THE Y-AXIS SCALE

df <- filter(covid_df, `Federal Department` == "U.S. Department of the Treasury")


#$1,000,000 Scale
p <- qplot(`Record Date`,Amount, data = df, geom = "point",
           color = `Unit Name`, alpha = I(.75)) +
  scale_y_continuous(name = "Amount", labels = dollar, limits = c(0,1000000))

ggsave(filename = "Treasury_Department_Units_1.png", plot = p, height = 8, 
       width = 16, dpi = 600)

#$10,000,000
p <- qplot(`Record Date`,Amount, data = df, geom = "point",
           color = `Unit Name`, alpha = I(.75)) +
  scale_y_continuous(name = "Amount", labels = dollar, limits = c(0,10000000))

ggsave(filename = "Treasury_Department_Units_2.png", plot = p, height = 8, 
       width = 16, dpi = 600)


msg <- "Warning messages are a product of cases not included in the scale of a graph"
message(msg)
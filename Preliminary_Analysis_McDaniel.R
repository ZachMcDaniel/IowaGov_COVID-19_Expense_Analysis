
rm(list = ls())

load("covid_df.R")
################################################################################
#Graph of top 5 departments by amount of requests
################################################################################

library(dplyr)
library(ggplot2)
library(scales)
#double checking for NA's
sum(is.na(covid_df))


##Percent of Requests by department scatter plot
df <- covid_df
df <- ungroup(df)
df<- group_by(df, `Federal Department`)

department_tb <- count(df)
department_tb$percent <- department_tb$n/sum(department_tb$n)
save(department_tb, file = "department_perc_tb.R")
#Getting top 5 departments by percent of requests
top_5 <- dplyr::arrange(department_tb, desc(percent)) %>%
  head(department_tb, n = 5) %>%
  group_by(`Federal Department`)
#scatter plot of percent on department
p <- qplot(`Federal Department`, percent , data = top_5, geom = "point" ) + 
  scale_y_continuous(name = "Percent of Requests", limits = c(0,1))   +
  theme(axis.text.x = element_text(angle = -45))
#saving plot image
ggsave(filename = "requests_by_department.png", plot = p, height = 8, 
       width = 12, dpi = 600)

###############################################################################
#Graphs of Amount by Department
###############################################################################

covid_df <- ungroup(covid_df)

##$10,000 Scale
p <- qplot(`Record Date`,Amount, data = covid_df, geom = "point",
           color = `Federal Department`) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(0,10000))

ggsave(filename = "Amount_by_Department_1.png", plot = p, height = 8, 
       width = 12, dpi = 600)

##$1,000,000 scale
p <- qplot(`Record Date`,Amount, data = covid_df, geom = "point",
           color = `Federal Department`) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(0,1000000))

ggsave(filename = "Amount_by_Department_2.png", plot = p, height = 8, 
       width = 12, dpi = 600)

##$100,000,000 Scale
p <- qplot(`Record Date`,Amount, data = covid_df, geom = "point",
           color = `Federal Department`) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(0,100000000))

ggsave(filename = "Amount_by_Department_3.png", plot = p, height = 8, 
       width = 12, dpi = 600)

##Full Scale
p <- qplot(`Record Date`,Amount, data = covid_df, geom = "point",
           color = `Federal Department`) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(-300000000,300000000))

ggsave(filename = "Amount_by_Department_4.png", plot = p, height = 8, 
       width = 12, dpi = 600)


###############################################################################
#Federal Department Analysis - Health and Human Services
###############################################################################
df <- subset(covid_df, `Federal Department` == "U.S. Department of Health and Human Services")

###Analysis of HHS by Agency

#scale of $10,000,000
p <- qplot(`Record Date`,Amount, data = df, geom = "point",
           color = `Federal Agency`, alpha = I(.75)) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(0,10000000))

ggsave(filename = "HHS_Department_Agencies_1.png", plot = p, height = 8, 
       width = 12, dpi = 600)

#scale of $1,000,000

p <- qplot(`Record Date`,Amount, data = df, geom = "point",
           color = `Federal Agency`, alpha = I(.75)) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(0,1000000))

ggsave(filename = "HHS_Department_Agencies_2.png", plot = p, height = 8, 
       width = 12, dpi = 600)

##scale of $10,000
p <- qplot(`Record Date`,Amount, data = df, geom = "point",
           color = `Federal Agency`, alpha = I(.75)) +
  scale_y_continuous(name = "Amount in $", labels = dollar, limits = c(0,10000))

ggsave(filename = "HHS_Department_Agencies_3.png", plot = p, height = 8, 
       width = 12, dpi = 600)


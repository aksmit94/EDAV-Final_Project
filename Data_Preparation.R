library(tidyverse)
library(Hmisc)

data_2011 <- read_csv('Medicare_Provider_Charge_Inpatient_DRG100_FY2011.csv')
data_2012 <- read_csv('Medicare_Provider_Charge_Inpatient_DRG100_FY2012.csv')
data_2013 <- read_csv('Medicare_Provider_Charge_Inpatient_DRG100_FY2013.csv')
data_2014 <- read_csv("Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv")
data_2015 <- read_csv("Medicare_Provider_Charge_Inpatient_DRGALL_FY2015.csv")
data_2016 <- read_csv("Medicare_Provider_Charge_Inpatient_DRGALL_FY2016.csv")

data_2011$Year <- 2011
data_2012$Year <- 2012
data_2013$Year <- 2013
data_2014$Year <- 2014
data_2015$Year <- 2015
data_2016$Year <- 2016

data <- rbind(data_2011,data_2012,data_2013,data_2014,data_2015,data_2016)

data$`Average Covered Charges` <- gsub('\\$', '', data$`Average Covered Charges`)
data$`Average Covered Charges` <- gsub('\\,', '', data$`Average Covered Charges`)
data$`Average Covered Charges` <- as.numeric(data$`Average Covered Charges`)
data$`Average Medicare Payments` <- gsub('\\$', '', data$`Average Medicare Payments`)
data$`Average Medicare Payments` <- gsub('\\,', '', data$`Average Medicare Payments`)
data$`Average Medicare Payments` <- as.numeric(data$`Average Medicare Payments`)
data$`Average Total Payments` <- gsub('\\$', '', data$`Average Total Payments`)
data$`Average Total Payments` <- gsub('\\,', '', data$`Average Total Payments`)
data$`Average Total Payments` <- as.numeric(data$`Average Total Payments`) 

data$Total_Payments = data$`Average Total Payments` * data$`Total Discharges`

data_na_removed <- na.omit(data)

mapping <- read_csv('Mapping.csv')
data_na_removed$Diagnostic_category <- plyr::mapvalues(data_na_removed$`DRG Definition`, from = mapping$DRG, to = mapping$Name)

data_na_removed$State <- capitalize(plyr::mapvalues(data_na_removed$`Provider State`, from = state.regions$abb, to = state.regions$region))

write.csv(data_na_removed, "Inpatient_Cost.csv")


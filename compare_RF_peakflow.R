#####################################################
#                                                   #
# Quick check matched peakflow and rainfall gauges  #
#                                                   #
#####################################################

#Load packages
library(dplyr)
library(lubridate)
install.packages("dataRetrieval")
library(dataRetrieval)

########################### Stations RF USGS_uv213215157552800 and peakflow 16296500 ###################

#Read the USGS
USGS_RF_gauge <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/USGS_uv213215157552800.csv", header = T)

#Read the comparison made by Yufen for the bad pair
df_bad_pair <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/check_date_16296500_X213215157552800.csv", header = T)

#Make Datetime column in a datetime format and create water year column
USGS_RF_gauge$DateTime <- as.POSIXct(USGS_RF_gauge$DateTime, format = "%Y-%m-%d %H:%M")
USGS_RF_gauge$Water_Year <- ifelse(month(USGS_RF_gauge$DateTime) < 10, USGS_RF_gauge$DateTime, USGS_RF_gauge$DateTime + years(1))
USGS_RF_gauge$Water_Year <- as.POSIXct(USGS_RF_gauge$Water_Year, origin="1970-01-01", tz = "HST")
USGS_RF_gauge$Water_Year <- substr(USGS_RF_gauge$Water_Year,1,4)

#Identify date of maximum amount of rainfall per year and per day
##Max RF per water year
max_RF_per_year <- USGS_RF_gauge %>%
  group_by(Water_Year) %>%
  summarise(max_RF = max(RF_mm, na.rm = T))

##Max RF per day with extra column water year
max_RF_per_day <- USGS_RF_gauge %>%
  mutate(Date = substr(DateTime,1,10)) %>%
  #mutate(Date = as.Date(DateTime, format="%Y/%m/%d")) %>%
  group_by(Date) %>%
  summarise(max_RF = max(RF_mm, na.rm = T))
max_RF_per_day$Date <- as.POSIXct(max_RF_per_day$Date, origin="1970-01-01", tz = "HST")
#max_RF_per_day$Date <- max_RF_per_day$Date + 10*60*60 #back to right time
max_RF_per_day$Water_Year <- ifelse(month(max_RF_per_day$Date) < 10, max_RF_per_day$Date, max_RF_per_day$Date + years(1))
max_RF_per_day$Water_Year <- as.POSIXct(max_RF_per_day$Water_Year, origin="1970-01-01", tz = "HST")
max_RF_per_day$Water_Year <- substr(max_RF_per_day$Water_Year,1,4)

##Vectors that will be used in the following loops
years <- unique(max_RF_per_day$Water_Year)
max_RF <- max_RF_per_year$max_RF
v_index <- c()
v_date_max_RF <- c()
v_max_Rf <- c()

##Loop to get position of max_RF in the max_RF_per_day dataframe
for (i in 1:length(years)){
  
  index <- which(max_RF_per_day$max_RF == max_RF[i] & max_RF_per_day$Water_Year == years[i])
  v_index <- append(v_index, index)
}

##Loop to get the dates and RF amount matching with the positions found in the previous loop
for (n in 1:length(v_index)){
  date_max_RF <- max_RF_per_day$Date[v_index[n]]
  v_date_max_RF <- append(v_date_max_RF, date_max_RF)
  max_Rf <- max_RF_per_day$max_RF[v_index[n]]
  v_max_Rf <- append(v_max_Rf, max_Rf)
}

#Create df with dates and max_RF
df_max_RF <- data.frame(date_rainMax = v_date_max_RF, RF_max = v_max_Rf)

#Add water year to the dataframe
df_max_RF$date_rainMax <- as.POSIXct(df_max_RF$date_rainMax, format = "%m/%d/%Y %H:%M", tz = "HST")
df_max_RF$Water_Year <- df_max_RF$date_rainMax 
df_max_RF$Water_Year <- ifelse(month(df_max_RF$Water_Year) < 10, df_max_RF$Water_Year, df_max_RF$Water_Year + years(1))
df_max_RF$Water_Year <- as.POSIXct(df_max_RF$Water_Year, origin="1970-01-01", tz = "HST")
df_max_RF$Water_Year <- substr(df_max_RF$Water_Year,1,4)

#RF_max per water year
#df_max_RF_test <- aggregate(df_max_RF[,2], list(Water_Year = df_max_RF$Water_Year), max, na.rm = T)
#names(df_max_RF_test)[2] <- "RF_max" 
#df_max_RF_test_2 <- merge(df_max_RF_test, df_max_RF, by = c("Water_Year", "RF_max"))

#Download the most recent data for the peakflow gauge associated with th rainfall station
siteNumber <- "16296500"
peakdata <- readNWISpeak(siteNumber)
peakdata <- peakdata[, c(2:3,5)]

#Add water year to the downloaded file
peakdata$peak_dt <- as.POSIXct(peakdata$peak_dt, format = "%m/%d/%Y %H:%M")
peakdata$peak_dt <- peakdata$peak_dt + 10*60*60 #Back to right day
peakdata$Water_Year <- peakdata$peak_dt
peakdata$Water_Year <- ifelse(month(peakdata$Water_Year) < 10, peakdata$Water_Year, peakdata$Water_Year + years(1))
peakdata$Water_Year <- as.POSIXct(peakdata$Water_Year, origin="1970-01-01", tz = "HST")
peakdata$Water_Year <- substr(peakdata$Water_Year, 1,4)
peakdata$peak_dt <- substr(peakdata$peak_dt,1,10)

#Merge peakflow and rainfall data together and organize
RF_peakflow <- merge(df_max_RF, peakdata, by = "Water_Year")
#RF_peakflow <- RF_peakflow[,c(4,1:3,5:7)]
RF_peakflow <- RF_peakflow[,-4]

#Calculate difference in days between peakflow and max_RF
RF_peakflow$date_rainMax <- as.Date(RF_peakflow$date_rainMax)
RF_peakflow$peak_dt <- as.Date(RF_peakflow$peak_dt)
RF_peakflow$Different_day <- difftime(RF_peakflow$date_rainMax, RF_peakflow$peak_dt, units = "days")

#Export table
write.csv(RF_peakflow, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/new_check_updated_date_USGS_uv213215157552800_16296500.csv", row.names = F)

########################### Stations RF USGS_uv213215157552800 and peakflow 16208000 ###################

#Download the most recent data for the peakflow gauge associated with th rainfall station
siteNumber <- "16208000"
peakdata <- readNWISpeak(siteNumber)
peakdata <- peakdata[, c(2:3,5)]

#Add water year to the downloaded file
peakdata$peak_dt <- as.POSIXct(peakdata$peak_dt, format = "%m/%d/%Y %H:%M")
peakdata$peak_dt <- peakdata$peak_dt + 10*60*60 #Back to right day
peakdata$Water_Year <- peakdata$peak_dt
peakdata$Water_Year <- ifelse(month(peakdata$Water_Year) < 10, peakdata$Water_Year, peakdata$Water_Year + years(1))
peakdata$Water_Year <- as.POSIXct(peakdata$Water_Year, origin="1970-01-01", tz = "HST")
peakdata$Water_Year <- substr(peakdata$Water_Year, 1,4)
peakdata$peak_dt <- substr(peakdata$peak_dt,1,10)

#Merge peakflow and rainfall data together and organize
RF_peakflow <- merge(df_max_RF, peakdata, by = "Water_Year")
#RF_peakflow <- RF_peakflow[,c(4,1:3,5:7)]
RF_peakflow <- RF_peakflow[,-4]

#Calculate difference in days between peakflow and max_RF
RF_peakflow$date_rainMax <- as.Date(RF_peakflow$date_rainMax)
RF_peakflow$peak_dt <- as.Date(RF_peakflow$peak_dt)
RF_peakflow$Different_day <- difftime(RF_peakflow$date_rainMax, RF_peakflow$peak_dt, units = "days")

#Export table
write.csv(RF_peakflow, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/new_check_updated_date_USGS_uv213215157552800_16208000.csv", row.names = F)


########################### Stations RF MNLH1(HI-17) and peakflow 16240500 ##############################

#Read the MLNH1 gauge
MNLH1 <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/MNLH1.csv", header = T)

#Make Datetime column in a datetime format and add water year
MNLH1$DateTime <- as.character(MNLH1$DateTime)
MNLH1$DateTime <- as.POSIXct(MNLH1$DateTime, format = "%m/%d/%Y %H:%M")
MNLH1$Water_Year <- ifelse(month(MNLH1$DateTime) < 10, MNLH1$DateTime, MNLH1$DateTime + years(1))
MNLH1$Water_Year <- as.POSIXct(MNLH1$Water_Year, origin="1970-01-01", tz = "HST")
MNLH1$Water_Year <- substr(MNLH1$Water_Year,1,4)

#Identify date of maximum amount of rainfall per year and per day
##Max RF per water year
max_RF_per_year <- MNLH1 %>%
  group_by(Water_Year) %>%
  summarise(max_RF = max(RF, na.rm = T))

##Max RF per day with extra column year
max_RF_per_day <- MNLH1 %>%
  mutate(Date = substr(DateTime,1,10)) %>%
  #mutate(Date = as.Date(DateTime, format="%Y/%m/%d")) %>%
  group_by(Date) %>%
  summarise(max_RF = max(RF, na.rm = T))
#max_RF_per_day$Year <- as.numeric(format(max_RF_per_day$Date, "%Y"))
max_RF_per_day$Date <- as.POSIXct(max_RF_per_day$Date, origin="1970-01-01", tz = "HST")
#max_RF_per_day$Date <- max_RF_per_day$Date + 10*60*60 #back to right time
max_RF_per_day$Water_Year <- ifelse(month(max_RF_per_day$Date) < 10, max_RF_per_day$Date, max_RF_per_day$Date + years(1))
max_RF_per_day$Water_Year <- as.POSIXct(max_RF_per_day$Water_Year, origin="1970-01-01", tz = "HST")
max_RF_per_day$Water_Year <- substr(max_RF_per_day$Water_Year,1,4)

#Replace -Inf in both tables by NA
max_RF_per_day$max_RF[max_RF_per_day$max_RF == "-Inf"] <- NA
max_RF_per_year$max_RF[max_RF_per_year$max_RF == "-Inf"] <- NA

##Vectors that will be used in the following loops
years <- unique(max_RF_per_day$Water_Year)
max_RF <- max_RF_per_year$max_RF
v_index <- c()
v_date_max_RF <- c()
v_max_Rf <- c()

##Loop to get position of max_RF in the max_RF_per_day dataframe
for (i in 1:length(years)){
  
  index <- which(max_RF_per_day$max_RF == max_RF[i] & max_RF_per_day$Water_Year == years[i])
  v_index <- append(v_index, index)
}

##Loop to get the dates and RF amount matching with the positions found in the previous loop
for (n in 1:length(v_index)){
  date_max_RF <- max_RF_per_day$Date[v_index[n]]
  v_date_max_RF <- append(v_date_max_RF, date_max_RF)
  max_Rf <- max_RF_per_day$max_RF[v_index[n]]
  v_max_Rf <- append(v_max_Rf, max_Rf)
}

#Create df with dates and max_RF
df_max_RF <- data.frame(date_rainMax = v_date_max_RF, RF_max = v_max_Rf)

#Add water year to the dataframe
df_max_RF$Water_Year <- df_max_RF$date_rainMax 
df_max_RF$Water_Year <- ifelse(month(df_max_RF$Water_Year) < 10, df_max_RF$Water_Year, df_max_RF$Water_Year + years(1))
df_max_RF$Water_Year <- as.POSIXct(df_max_RF$Water_Year, origin="1970-01-01", tz = "HST")
df_max_RF$Water_Year <- substr(df_max_RF$Water_Year,1,4)

#Add water year to the dataframe
#df_max_RF$date_rainMax <- as.POSIXct(df_max_RF$date_rainMax, format = "%m/%d/%Y %H:%M", tz = "HST")
#df_max_RF$date_rainMax <- df_max_RF$date_rainMax + 10*60*60 #Back to right day
#df_max_RF$Water_Year <- df_max_RF$date_rainMax 
#df_max_RF$Water_Year <- ifelse(month(df_max_RF$Water_Year) < 10, df_max_RF$Water_Year, df_max_RF$Water_Year + years(1))
#df_max_RF$Water_Year <- as.POSIXct(df_max_RF$Water_Year, origin="1970-01-01", tz = "HST")
#df_max_RF$Water_Year <- substr(df_max_RF$Water_Year,1,4)
#df_max_RF$date_rainMax <- substr(df_max_RF$date_rainMax,1,10)

#RF_max per water year
#df_max_RF_test <- aggregate(df_max_RF[,2], list(Water_Year = df_max_RF$Water_Year), max, na.rm = T)
#names(df_max_RF_test)[2] <- "RF_max" 
#df_max_RF_test_2 <- merge(df_max_RF_test, df_max_RF, by = c("Water_Year", "RF_max"))


#Download the most recent data for the peakflow gauge associated with th rainfall station
siteNumber <- "16240500"
peakdata <- readNWISpeak(siteNumber)
peakdata <- peakdata[, c(2:3,5)]

#Add water year to the downloaded file
peakdata$peak_dt <- as.POSIXct(peakdata$peak_dt, format = "%m/%d/%Y %H:%M")
peakdata$peak_dt <- peakdata$peak_dt + 37800 #Back to right day
peakdata$Water_Year <- peakdata$peak_dt
peakdata$Water_Year <- ifelse(month(peakdata$Water_Year) < 10, peakdata$Water_Year, peakdata$Water_Year + years(1))
peakdata$Water_Year <- as.POSIXct(peakdata$Water_Year, origin="1970-01-01", tz = "HST")
peakdata$Water_Year <- substr(peakdata$Water_Year, 1,4)
peakdata$peak_dt <- substr(peakdata$peak_dt,1,10)

#Merge peakflow and rainfall data together and organize
RF_peakflow <- merge(df_max_RF, peakdata, by = "Water_Year")
#RF_peakflow <- RF_peakflow[,c(4,1:3,5:7)]
RF_peakflow <- RF_peakflow[,-4]

#Calculate difference in days between peakflow and max_RF
RF_peakflow$date_rainMax <- as.Date(RF_peakflow$date_rainMax)
RF_peakflow$peak_dt <- as.Date(RF_peakflow$peak_dt)
RF_peakflow$Different_day <- difftime(RF_peakflow$date_rainMax, RF_peakflow$peak_dt, units = "days")

#Export table
write.csv(RF_peakflow, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/new_check_updated_date_MNLH1_16240500.csv", row.names = F)

 

#####################################################
#                                                   #
# Quick check matched peakflow and rainfall gauges  #
#                     with                          #
#      Avg, sum for 3 hours and 6 hours             #
#                                                   #
#####################################################

#Load library
install.packages("dplyr")
library(dplyr)
install.packages("naniar")
library(naniar)
install.packages("data.table")
library(data.table)
install.packages("zoo")
library(zoo)
library(lubridate)
install.packages("dataRetrieval")
library(dataRetrieval)

########################### Stations RF USGS_uv213215157552800 and peakflow 16296500 ###################

#Read the USGS
USGS_RF_gauge <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/USGS_uv213215157552800.csv", header = T)

#Keep only DateTime and corrected value columns. Make DateTime column in a datetime format
USGS_RF_gauge <- USGS_RF_gauge[, c(1,4)]
USGS_RF_gauge$DateTime <- as.POSIXct(USGS_RF_gauge$DateTime, format = "%Y-%m-%d %H:%M")

                                ######6 hours######

#Add 5 rows at the beginning of the dataset. This will allow us to calculate avg and sum every 6 hours for the first row
df_RF_6hr <- add_row(.data = USGS_RF_gauge, RF_mm = c(NA, NA, NA, NA, NA), 
                 DateTime = c(USGS_RF_gauge$DateTime[1] - 5*3600, 
                  USGS_RF_gauge$DateTime[1]- 4*3600, 
                    USGS_RF_gauge$DateTime[1]- 3*3600,
                      USGS_RF_gauge$DateTime[1] - 2*3600,
                        USGS_RF_gauge$DateTime[1] - 3600), .before = 1)

#Calculate sum and avg of 6 hours rainfall
sum_6hr <- rollapply(df_RF_6hr$RF_mm, 6, sum, by = 1, na.rm = T)
avg_6hr <- rollapply(df_RF_6hr$RF_mm, 6, mean, by = 1, na.rm = T)

#Bind sum and avg to rainfall dataset
USGS_RF <- cbind(USGS_RF_gauge, sum_6hr, avg_6hr)

                            ######3 hours#######

#Add 2 rows at the beginning of the dataset. This will allow us to calculate avg and sum every 6 hours for the first row
df_RF_3hr <- add_row(.data = USGS_RF_gauge, RF_mm = c(NA, NA), 
                     DateTime = c(USGS_RF_gauge$DateTime[1] - 2*3600,
                                  USGS_RF_gauge$DateTime[1] - 3600), .before = 1)

#Calculate sum and avg of 6 hours rainfall
sum_3hr <- rollapply(df_RF_3hr$RF_mm, 3, sum, by = 1, na.rm = T)
avg_3hr <- rollapply(df_RF_3hr$RF_mm, 3, mean, by = 1, na.rm = T)

#Bind sum and avg to rainfall dataset
USGS_RF <- cbind(USGS_RF, sum_3hr, avg_3hr)

              ######Compare the avg and sum with peakflow gauge#######

#Add water year to the USGS_RF data frame
USGS_RF$Water_Year <- ifelse(month(USGS_RF$DateTime) < 10, USGS_RF$DateTime, USGS_RF$DateTime + years(1))
USGS_RF$Water_Year <- as.POSIXct(USGS_RF$Water_Year, origin="1970-01-01", tz = "HST")
USGS_RF$Water_Year <- substr(USGS_RF$Water_Year,1,4)

########Identify date of maximum sum and avg amount of rainfall per year and per day

  ### Avg 3 and 6 hours ###

##Max RF per year for avg 6hr (remove rows with 0 as we can't have yearly avg or sum equal to 0)
max_RF_year_avg_6hr <- USGS_RF %>%
  #mutate(Year = as.numeric(format(DateTime, "%Y"))) %>%
  group_by(Water_Year) %>%
  summarise(max_RF = max(avg_6hr, na.rm = T))
#max_RF_year_avg_6hr <- max_RF_year_avg_6hr[max_RF_year_avg_6hr$max_RF != 0,]

##Max RF per day for avg 6hr with extra column year
max_RF_per_day_avg_6hr <- USGS_RF %>%
  mutate(Date = substr(DateTime,1,10)) %>%
  #mutate(Date = as.Date(DateTime, format="%Y/%m/%d")) %>%
  group_by(Date) %>%
  summarise(max_RF = max(avg_6hr, na.rm = T))
#max_RF_per_day_avg_6hr$Year <- as.numeric(format(max_RF_per_day_avg_6hr$Date, "%Y"))
max_RF_per_day_avg_6hr$Date <- as.POSIXct(max_RF_per_day_avg_6hr$Date, origin="1970-01-01", tz = "HST")
#max_RF_per_day_avg_6hr$Date <- max_RF_per_day_avg_6hr$Date + 10*60*60 #back to right time
max_RF_per_day_avg_6hr$Water_Year <- ifelse(month(max_RF_per_day_avg_6hr$Date) < 10, max_RF_per_day_avg_6hr$Date, max_RF_per_day_avg_6hr$Date + years(1))
max_RF_per_day_avg_6hr$Water_Year <- as.POSIXct(max_RF_per_day_avg_6hr$Water_Year, origin="1970-01-01", tz = "HST")
max_RF_per_day_avg_6hr$Water_Year <- substr(max_RF_per_day_avg_6hr$Water_Year,1,4)

##Max RF per year for avg 3hr (remove rows with 0 as we can't have yearly avg or sum equal to 0)
max_RF_year_avg_3hr <- USGS_RF %>%
  #mutate(Year = as.numeric(format(DateTime, "%Y"))) %>%
  group_by(Water_Year) %>%
  summarise(max_RF = max(avg_3hr, na.rm = T))
#max_RF_year_avg_3hr <- max_RF_year_avg_3hr[max_RF_year_avg_3hr$max_RF != 0,]

##Max RF per day for avg 3hr with extra column year
max_RF_per_day_avg_3hr <- USGS_RF %>%
  mutate(Date = substr(DateTime,1,10)) %>%
  #mutate(Date = as.Date(DateTime, format="%Y/%m/%d")) %>%
  group_by(Date) %>%
  summarise(max_RF = max(avg_3hr, na.rm = T))
#max_RF_per_day_avg_3hr$Year <- as.numeric(format(max_RF_per_day_avg_3hr$Date, "%Y"))
max_RF_per_day_avg_3hr$Date <- as.POSIXct(max_RF_per_day_avg_3hr$Date, origin="1970-01-01", tz = "HST")
#max_RF_per_day_avg_3hr$Date <- max_RF_per_day_avg_3hr$Date + 10*60*60 #back to right time
max_RF_per_day_avg_3hr$Water_Year <- ifelse(month(max_RF_per_day_avg_3hr$Date) < 10, max_RF_per_day_avg_3hr$Date, max_RF_per_day_avg_3hr$Date + years(1))
max_RF_per_day_avg_3hr$Water_Year <- as.POSIXct(max_RF_per_day_avg_3hr$Water_Year, origin="1970-01-01", tz = "HST")
max_RF_per_day_avg_3hr$Water_Year <- substr(max_RF_per_day_avg_3hr$Water_Year,1,4)

    ### Sum 3 and 6 hours ###

##Max RF per year for sum 6hr (remove rows with 0 as we can't have yearly avg or sum equal to 0)
max_RF_year_sum_6hr <- USGS_RF %>%
  #mutate(Year = as.numeric(format(DateTime, "%Y"))) %>%
  group_by(Water_Year) %>%
  summarise(max_RF = max(sum_6hr, na.rm = T))
#max_RF_year_sum_6hr <- max_RF_year_sum_6hr[max_RF_year_sum_6hr$max_RF != 0,]

##Max RF per day for sum 6hr with extra column year
max_RF_per_day_sum_6hr <- USGS_RF %>%
  mutate(Date = substr(DateTime,1,10)) %>%
  #mutate(Date = as.Date(DateTime, format="%Y/%m/%d")) %>%
  group_by(Date) %>%
  summarise(max_RF = max(sum_6hr, na.rm = T))
#max_RF_per_day_sum_6hr$Year <- as.numeric(format(max_RF_per_day_sum_6hr$Date, "%Y"))
max_RF_per_day_sum_6hr$Date <- as.POSIXct(max_RF_per_day_sum_6hr$Date, origin="1970-01-01", tz = "HST")
#max_RF_per_day_sum_6hr$Date <- max_RF_per_day_sum_6hr$Date + 10*60*60 #back to right time
max_RF_per_day_sum_6hr$Water_Year <- ifelse(month(max_RF_per_day_sum_6hr$Date) < 10, max_RF_per_day_sum_6hr$Date, max_RF_per_day_sum_6hr$Date + years(1))
max_RF_per_day_sum_6hr$Water_Year <- as.POSIXct(max_RF_per_day_sum_6hr$Water_Year, origin="1970-01-01", tz = "HST")
max_RF_per_day_sum_6hr$Water_Year <- substr(max_RF_per_day_sum_6hr$Water_Year,1,4)

##Max RF per year for sum 3hr (remove rows with 0 as we can't have yearly avg or sum equal to 0)
max_RF_year_sum_3hr <- USGS_RF %>%
  #mutate(Year = as.numeric(format(DateTime, "%Y"))) %>%
  group_by(Water_Year) %>%
  summarise(max_RF = max(sum_3hr, na.rm = T))
#max_RF_year_sum_3hr <- max_RF_year_sum_3hr[max_RF_year_sum_3hr$max_RF != 0,]

##Max RF per day for sum 3hr with extra column year
max_RF_per_day_sum_3hr <- USGS_RF %>%
  mutate(Date = substr(DateTime,1,10)) %>%
  #mutate(Date = as.Date(DateTime, format="%Y/%m/%d")) %>%
  group_by(Date) %>%
  summarise(max_RF = max(sum_3hr, na.rm = T))
#max_RF_per_day_sum_3hr$Year <- as.numeric(format(max_RF_per_day_sum_3hr$Date, "%Y"))
max_RF_per_day_sum_3hr$Date <- as.POSIXct(max_RF_per_day_sum_3hr$Date, origin="1970-01-01", tz = "HST")
#max_RF_per_day_sum_3hr$Date <- max_RF_per_day_sum_3hr$Date + 10*60*60 #back to right time
max_RF_per_day_sum_3hr$Water_Year <- ifelse(month(max_RF_per_day_sum_3hr$Date) < 10, max_RF_per_day_sum_3hr$Date, max_RF_per_day_sum_3hr$Date + years(1))
max_RF_per_day_sum_3hr$Water_Year <- as.POSIXct(max_RF_per_day_sum_3hr$Water_Year, origin="1970-01-01", tz = "HST")
max_RF_per_day_sum_3hr$Water_Year <- substr(max_RF_per_day_sum_3hr$Water_Year,1,4)

##Vectors that will be used in the following loops
years <- unique(max_RF_year_avg_3hr$Water_Year)
max_RF_avg3hr <- max_RF_year_avg_3hr$max_RF
max_RF_avg6hr <- max_RF_year_avg_6hr$max_RF
max_RF_sum3hr <- max_RF_year_sum_3hr$max_RF
max_RF_sum6hr <- max_RF_year_sum_6hr$max_RF
v_index_avg3hr <- c()
v_index_avg6hr <- c()
v_index_sum3hr <- c()
v_index_sum6hr <- c()
v_date_max_RF_avg3hr <- c()
v_date_max_RF_avg6hr <- c()
v_date_max_RF_sum3hr <- c()
v_date_max_RF_sum6hr <- c()
v_max_Rf_avg3hr <- c()
v_max_Rf_avg6hr <- c()
v_max_Rf_sum3hr <- c()
v_max_Rf_sum6hr <- c()

##Loop to get position of max_RF in the max_RF_per_day dataframe
for (i in 1:length(years)){
  #avg 3hr
  index_avg_3hr <- which(max_RF_per_day_avg_3hr$max_RF == max_RF_avg3hr[i] & max_RF_per_day_avg_3hr$Water_Year == years[i])
  v_index_avg3hr <- append(v_index_avg3hr, index_avg_3hr)
  
  #avg 6hr
  index_avg_6hr <- which(max_RF_per_day_avg_6hr$max_RF == max_RF_avg6hr[i] & max_RF_per_day_avg_6hr$Water_Year == years[i])
  v_index_avg6hr <- append(v_index_avg6hr, index_avg_6hr)
  
  #sum 3hr
  index_sum_3hr <- which(max_RF_per_day_sum_3hr$max_RF == max_RF_sum3hr[i] & max_RF_per_day_sum_3hr$Water_Year == years[i])
  v_index_sum3hr <- append(v_index_sum3hr, index_sum_3hr)
  
  #sum 6hr
  index_sum_6hr <- which(max_RF_per_day_sum_6hr$max_RF == max_RF_sum6hr[i] & max_RF_per_day_sum_6hr$Water_Year == years[i])
  v_index_sum6hr <- append(v_index_sum6hr, index_sum_6hr)
}

##Loop to get values of max_RF for avg and sum and their associated dates
for (n in 1:length(years)){ 
  #avg 3hr
  date_max_avg_3hr_RF <- max_RF_per_day_avg_3hr$Date[v_index_avg3hr[n]]
  v_date_max_RF_avg3hr <- append(v_date_max_RF_avg3hr, date_max_avg_3hr_RF)
  max_avg_3hr_Rf <- max_RF_per_day_avg_3hr$max_RF[v_index_avg3hr[n]]
  v_max_Rf_avg3hr <- append(v_max_Rf_avg3hr,max_avg_3hr_Rf)
  
  #avg 6hr
  date_max_avg_6hr_RF <- max_RF_per_day_avg_6hr$Date[v_index_avg6hr[n]]
  v_date_max_RF_avg6hr <- append(v_date_max_RF_avg6hr, date_max_avg_6hr_RF)
  max_avg_6hr_Rf <- max_RF_per_day_avg_6hr$max_RF[v_index_avg6hr[n]]
  v_max_Rf_avg6hr <- append(v_max_Rf_avg6hr,max_avg_6hr_Rf)

  #sum 3hr
  date_max_sum_3hr_RF <- max_RF_per_day_sum_3hr$Date[v_index_sum3hr[n]]
  v_date_max_RF_sum3hr <- append(v_date_max_RF_sum3hr, date_max_sum_3hr_RF)
  max_sum_3hr_Rf <- max_RF_per_day_sum_3hr$max_RF[v_index_sum3hr[n]]
  v_max_Rf_sum3hr <- append(v_max_Rf_sum3hr,max_sum_3hr_Rf)
  
  #sum 6hr
  date_max_sum_6hr_RF <- max_RF_per_day_sum_6hr$Date[v_index_sum6hr[n]]
  v_date_max_RF_sum6hr <- append(v_date_max_RF_sum6hr, date_max_sum_6hr_RF)
  max_sum_6hr_Rf <- max_RF_per_day_sum_6hr$max_RF[v_index_sum6hr[n]]
  v_max_Rf_sum6hr <- append(v_max_Rf_sum6hr,max_sum_6hr_Rf)
}

#Create df with dates and max_RF
df_max_RF <- data.frame(date_rainMax_avg3hr = v_date_max_RF_avg3hr, RF_max_avg3hr = v_max_Rf_avg3hr,
                        date_rainMax_avg6hr = v_date_max_RF_avg6hr, RF_max_avg6hr = v_max_Rf_avg6hr,
                        date_rainMax_sum3hr = v_date_max_RF_sum3hr, RF_max_sum3hr = v_max_Rf_sum3hr,
                        date_rainMax_sum6hr = v_date_max_RF_sum6hr, RF_max_sum6hr = v_max_Rf_sum6hr)

#Add water year to the dataframe
df_max_RF$date_rainMax_avg3hr <- as.POSIXct(df_max_RF$date_rainMax_avg3hr, format = "%m/%d/%Y %H:%M", tz = "HST")
#df_max_RF$date_rainMax_avg3hr <- df_max_RF$date_rainMax_avg3hr + 10*60*60 #Back to right day
df_max_RF$Water_Year <- df_max_RF$date_rainMax_avg3hr 
df_max_RF$Water_Year <- ifelse(month(df_max_RF$Water_Year) < 10, df_max_RF$Water_Year, df_max_RF$Water_Year + years(1))
df_max_RF$Water_Year <- as.POSIXct(df_max_RF$Water_Year, origin="1970-01-01", tz = "HST")
df_max_RF$Water_Year <- substr(df_max_RF$Water_Year,1,4)
df_max_RF <- df_max_RF[,c(9,1:8)]

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

#Calculate difference in days between peakflow and max_RF avg and sum
RF_peakflow$Different_day_avg3hr <- difftime(RF_peakflow$date_rainMax_avg3hr, RF_peakflow$peak_dt, units = "days")
RF_peakflow$Different_day_avg6hr <- difftime(RF_peakflow$date_rainMax_avg6hr, RF_peakflow$peak_dt, units = "days")
RF_peakflow$Different_day_avg6hr <- round(RF_peakflow$Different_day_avg6hr,0)
RF_peakflow$Different_day_sum3hr <- difftime(RF_peakflow$date_rainMax_sum3hr, RF_peakflow$peak_dt, units = "days")
RF_peakflow$Different_day_sum3hr <- round(RF_peakflow$Different_day_sum3hr,0)
RF_peakflow$Different_day_sum6hr <- difftime(RF_peakflow$date_rainMax_sum6hr, RF_peakflow$peak_dt, units = "days")
RF_peakflow$Different_day_sum6hr <- round(RF_peakflow$Different_day_sum6hr,0)

#Export table
write.csv(RF_peakflow, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/new_max_avg_sum_USGS_uv213215157552800_16296500.csv", row.names = F)

#Aggregate by water year and export table
#RF_water_year <- aggregate(RF_peakflow[,c(3,5,7,9)], list(Water_Year = RF_peakflow$Water_Year), max, na.rm = T)
#RF_water_year <- merge(RF_water_year, RF_peakflow, by = c("Water_Year", "RF_max_avg3hr", "RF_max_avg6hr",
                                                          #"RF_max_sum3hr", "RF_max_sum6hr"))
#RF_water_year <- RF_water_year[c(1,2,6,3,7,4,8,5,9,10:16)]
#write.csv(RF_water_year, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/aggregate_USGS_uv213215157552800_16296500.csv", row.names = F)


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

#Calculate difference in days between peakflow and max_RF avg and sum
RF_peakflow$Different_day_avg3hr <- difftime(RF_peakflow$date_rainMax_avg3hr, RF_peakflow$peak_dt, units = "days")
RF_peakflow$Different_day_avg6hr <- difftime(RF_peakflow$date_rainMax_avg6hr, RF_peakflow$peak_dt, units = "days")
RF_peakflow$Different_day_avg6hr <- round(RF_peakflow$Different_day_avg6hr,0)
RF_peakflow$Different_day_sum3hr <- difftime(RF_peakflow$date_rainMax_sum3hr, RF_peakflow$peak_dt, units = "days")
RF_peakflow$Different_day_sum3hr <- round(RF_peakflow$Different_day_sum3hr,0)
RF_peakflow$Different_day_sum6hr <- difftime(RF_peakflow$date_rainMax_sum6hr, RF_peakflow$peak_dt, units = "days")
RF_peakflow$Different_day_sum6hr <- round(RF_peakflow$Different_day_sum6hr,0)

#Export table
write.csv(RF_peakflow, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/new_max_avg_sum_USGS_uv213215157552800_16208000.csv", row.names = F)

#Aggregate by water year and export table
#RF_water_year <- aggregate(RF_peakflow[,c(3,5,7,9)], list(Water_Year = RF_peakflow$Water_Year), max, na.rm = T)
#RF_water_year <- merge(RF_water_year, RF_peakflow, by = c("Water_Year", "RF_max_avg3hr", "RF_max_avg6hr",
                                                          #"RF_max_sum3hr", "RF_max_sum6hr"))
#RF_water_year <- RF_water_year[c(1,2,6,3,7,4,8,5,9,10:16)]
#write.csv(RF_water_year, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/aggregate_USGS_uv213215157552800_16208000.csv", row.names = F)


########################### Stations RF MNLH1(HI-17) and peakflow 16240500 ##############################

#Read the MNLH1
MNLH1 <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data/MNLH1.csv", header = T)

#Make Datetime column in a datetime format and add water year
MNLH1$DateTime <- as.character(MNLH1$DateTime)
MNLH1$DateTime <- as.POSIXct(MNLH1$DateTime, format = "%m/%d/%Y %H:%M")

                               ######6 hours######

#Add 5 rows at the beginning of the dataset. This will allow us to calculate avg and sum every 6 hours for the first row
df_RF_6hr <- add_row(.data = MNLH1, RF = c(NA, NA, NA, NA, NA), 
                     DateTime = c(MNLH1$DateTime[1] - 5*3600, 
                                  MNLH1$DateTime[1]- 4*3600, 
                                  MNLH1$DateTime[1]- 3*3600,
                                  MNLH1$DateTime[1] - 2*3600,
                                  MNLH1$DateTime[1] - 3600), .before = 1)

#Calculate sum and avg of 6 hours rainfall
sum_6hr <- rollapply(df_RF_6hr$RF, 6, sum, by = 1, na.rm = T)
avg_6hr <- rollapply(df_RF_6hr$RF, 6, mean, by = 1, na.rm = T)

#Bind sum and avg to rainfall dataset
MNLH1_RF <- cbind(MNLH1, sum_6hr, avg_6hr)

                                   ######3 hours#######

#Add 2 rows at the beginning of the dataset. This will allow us to calculate avg and sum every 6 hours for the first row
df_RF_3hr <- add_row(.data = MNLH1, RF = c(NA, NA), 
                     DateTime = c(MNLH1$DateTime[1] - 2*3600,
                                  MNLH1$DateTime[1] - 3600), .before = 1)

#Calculate sum and avg of 6 hours rainfall
sum_3hr <- rollapply(df_RF_3hr$RF, 3, sum, by = 1, na.rm = T)
avg_3hr <- rollapply(df_RF_3hr$RF, 3, mean, by = 1, na.rm = T)

#Bind sum and avg to rainfall dataset
MNLH1_RF <- cbind(MNLH1_RF, sum_3hr, avg_3hr)

           ######Compare the avg and sum with peakflow gauge#######

#Add water year to the MNLH1_RF data frame
MNLH1_RF$Water_Year <- ifelse(month(MNLH1_RF$DateTime) < 10, MNLH1_RF$DateTime, MNLH1_RF$DateTime + years(1))
MNLH1_RF$Water_Year <- as.POSIXct(MNLH1_RF$Water_Year, origin="1970-01-01", tz = "HST")
MNLH1_RF$Water_Year <- substr(MNLH1_RF$Water_Year,1,4)

########Identify date of maximum sum and avg amount of rainfall per year and per day

  ### Avg 3 and 6 hours ###

##Max RF per year for avg 6hr (remove rows with 0 or -Inf as we can't have yearly avg or sum equal to 0)
max_RF_year_avg_6hr <- MNLH1_RF %>%
  #mutate(Year = as.numeric(format(DateTime, "%Y"))) %>%
  group_by(Water_Year) %>%
  summarise(max_RF = max(avg_6hr, na.rm = T))
max_RF_year_avg_6hr <- max_RF_year_avg_6hr[max_RF_year_avg_6hr$max_RF != "-Inf",]

##Max RF per day for avg 6hr with extra column year
max_RF_per_day_avg_6hr <- MNLH1_RF %>%
  mutate(Date = substr(DateTime,1,10)) %>%
  #mutate(Date = as.Date(DateTime, format="%Y/%m/%d")) %>%
  group_by(Date) %>%
  summarise(max_RF = max(avg_6hr, na.rm = T))
#max_RF_per_day_avg_6hr$Year <- as.numeric(format(max_RF_per_day_avg_6hr$Date, "%Y"))
max_RF_per_day_avg_6hr$Date <- as.POSIXct(max_RF_per_day_avg_6hr$Date, origin="1970-01-01", tz = "HST")
#max_RF_per_day_avg_6hr$Date <- max_RF_per_day_avg_6hr$Date + 10*60*60 #back to right time
max_RF_per_day_avg_6hr$Water_Year <- ifelse(month(max_RF_per_day_avg_6hr$Date) < 10, max_RF_per_day_avg_6hr$Date, max_RF_per_day_avg_6hr$Date + years(1))
max_RF_per_day_avg_6hr$Water_Year <- as.POSIXct(max_RF_per_day_avg_6hr$Water_Year, origin="1970-01-01", tz = "HST")
max_RF_per_day_avg_6hr$Water_Year <- substr(max_RF_per_day_avg_6hr$Water_Year,1,4)

##Max RF per year for avg 3hr (remove rows with 0 or -Inf as we can't have yearly avg or sum equal to 0)
max_RF_year_avg_3hr <- MNLH1_RF %>%
  #mutate(Year = as.numeric(format(DateTime, "%Y"))) %>%
  group_by(Water_Year) %>%
  summarise(max_RF = max(avg_3hr, na.rm = T))
max_RF_year_avg_3hr <- max_RF_year_avg_3hr[max_RF_year_avg_3hr$max_RF != "-Inf",]

##Max RF per day for avg 3hr with extra column year
max_RF_per_day_avg_3hr <- MNLH1_RF %>%
  mutate(Date = substr(DateTime,1,10)) %>%
  #mutate(Date = as.Date(DateTime, format="%Y/%m/%d")) %>%
  group_by(Date) %>%
  summarise(max_RF = max(avg_3hr, na.rm = T))
#max_RF_per_day_avg_3hr$Year <- as.numeric(format(max_RF_per_day_avg_3hr$Date, "%Y"))
max_RF_per_day_avg_3hr$Date <- as.POSIXct(max_RF_per_day_avg_3hr$Date, origin="1970-01-01", tz = "HST")
#max_RF_per_day_avg_3hr$Date <- max_RF_per_day_avg_3hr$Date + 10*60*60 #back to right time
max_RF_per_day_avg_3hr$Water_Year <- ifelse(month(max_RF_per_day_avg_3hr$Date) < 10, max_RF_per_day_avg_3hr$Date, max_RF_per_day_avg_3hr$Date + years(1))
max_RF_per_day_avg_3hr$Water_Year <- as.POSIXct(max_RF_per_day_avg_3hr$Water_Year, origin="1970-01-01", tz = "HST")
max_RF_per_day_avg_3hr$Water_Year <- substr(max_RF_per_day_avg_3hr$Water_Year,1,4)


   ### Sum 3 and 6 hours ###

##Max RF per year for sum 6hr (remove rows with 0 or -Inf as we can't have yearly avg or sum equal to 0)
max_RF_year_sum_6hr <- MNLH1_RF %>%
  #mutate(Year = as.numeric(format(DateTime, "%Y"))) %>%
  group_by(Water_Year) %>%
  summarise(max_RF = max(sum_6hr, na.rm = T))
max_RF_year_sum_6hr <- max_RF_year_sum_6hr[max_RF_year_sum_6hr$max_RF != 0,]

##Max RF per day for sum 6hr with extra column year
max_RF_per_day_sum_6hr <- MNLH1_RF %>%
  mutate(Date = substr(DateTime,1,10)) %>%
  #mutate(Date = as.Date(DateTime, format="%Y/%m/%d")) %>%
  group_by(Date) %>%
  summarise(max_RF = max(sum_6hr, na.rm = T))
#max_RF_per_day_sum_6hr$Year <- as.numeric(format(max_RF_per_day_sum_6hr$Date, "%Y"))
max_RF_per_day_sum_6hr$Date <- as.POSIXct(max_RF_per_day_sum_6hr$Date, origin="1970-01-01", tz = "HST")
#max_RF_per_day_sum_6hr$Date <- max_RF_per_day_sum_6hr$Date + 10*60*60 #back to right time
max_RF_per_day_sum_6hr$Water_Year <- ifelse(month(max_RF_per_day_sum_6hr$Date) < 10, max_RF_per_day_sum_6hr$Date, max_RF_per_day_sum_6hr$Date + years(1))
max_RF_per_day_sum_6hr$Water_Year <- as.POSIXct(max_RF_per_day_sum_6hr$Water_Year, origin="1970-01-01", tz = "HST")
max_RF_per_day_sum_6hr$Water_Year <- substr(max_RF_per_day_sum_6hr$Water_Year,1,4)

##Max RF per year for sum 3hr (remove rows with 0 or -Inf as we can't have yearly avg or sum equal to 0)
max_RF_year_sum_3hr <- MNLH1_RF %>%
  #mutate(Year = as.numeric(format(DateTime, "%Y"))) %>%
  group_by(Water_Year) %>%
  summarise(max_RF = max(sum_3hr, na.rm = T))
max_RF_year_sum_3hr <- max_RF_year_sum_3hr[max_RF_year_sum_3hr$max_RF != 0,]

##Max RF per day for sum 3hr with extra column year
max_RF_per_day_sum_3hr <- MNLH1_RF %>%
  mutate(Date = substr(DateTime,1,10)) %>%
  #mutate(Date = as.Date(DateTime, format="%Y/%m/%d")) %>%
  group_by(Date) %>%
  summarise(max_RF = max(sum_3hr, na.rm = T))
#max_RF_per_day_sum_3hr$Year <- as.numeric(format(max_RF_per_day_sum_3hr$Date, "%Y"))
max_RF_per_day_sum_3hr$Date <- as.POSIXct(max_RF_per_day_sum_3hr$Date, origin="1970-01-01", tz = "HST")
#max_RF_per_day_sum_3hr$Date <- max_RF_per_day_sum_3hr$Date + 10*60*60 #back to right time
max_RF_per_day_sum_3hr$Water_Year <- ifelse(month(max_RF_per_day_sum_3hr$Date) < 10, max_RF_per_day_sum_3hr$Date, max_RF_per_day_sum_3hr$Date + years(1))
max_RF_per_day_sum_3hr$Water_Year <- as.POSIXct(max_RF_per_day_sum_3hr$Water_Year, origin="1970-01-01", tz = "HST")
max_RF_per_day_sum_3hr$Water_Year <- substr(max_RF_per_day_sum_3hr$Water_Year,1,4)

##Vectors that will be used in the following loops
years <- unique(max_RF_year_avg_3hr$Water_Year)
max_RF_avg3hr <- max_RF_year_avg_3hr$max_RF
max_RF_avg6hr <- max_RF_year_avg_6hr$max_RF
max_RF_sum3hr <- max_RF_year_sum_3hr$max_RF
max_RF_sum6hr <- max_RF_year_sum_6hr$max_RF
v_index_avg3hr <- c()
v_index_avg6hr <- c()
v_index_sum3hr <- c()
v_index_sum6hr <- c()
v_date_max_RF_avg3hr <- c()
v_date_max_RF_avg6hr <- c()
v_date_max_RF_sum3hr <- c()
v_date_max_RF_sum6hr <- c()
v_max_Rf_avg3hr <- c()
v_max_Rf_avg6hr <- c()
v_max_Rf_sum3hr <- c()
v_max_Rf_sum6hr <- c()

##Loop to get position of max_RF in the max_RF_per_day dataframe
for (i in 1:length(years)){
  #avg 3hr
  index_avg_3hr <- which(max_RF_per_day_avg_3hr$max_RF == max_RF_avg3hr[i] & max_RF_per_day_avg_3hr$Water_Year == years[i])
  v_index_avg3hr <- append(v_index_avg3hr, index_avg_3hr)
  
  #avg 6hr
  index_avg_6hr <- which(max_RF_per_day_avg_6hr$max_RF == max_RF_avg6hr[i] & max_RF_per_day_avg_6hr$Water_Year == years[i])
  v_index_avg6hr <- append(v_index_avg6hr, index_avg_6hr)
  
  #sum 3hr
  index_sum_3hr <- which(max_RF_per_day_sum_3hr$max_RF == max_RF_sum3hr[i] & max_RF_per_day_sum_3hr$Water_Year == years[i])
  v_index_sum3hr <- append(v_index_sum3hr, index_sum_3hr)
  
  #sum 6hr
  index_sum_6hr <- which(max_RF_per_day_sum_6hr$max_RF == max_RF_sum6hr[i] & max_RF_per_day_sum_6hr$Water_Year == years[i])
  v_index_sum6hr <- append(v_index_sum6hr, index_sum_6hr)
}

#odd... we have two times the same for avg and sum 6. We keep the first one (They are one day after the other)
v_index_avg6hr <- v_index_avg6hr[-3]
v_index_sum6hr <- v_index_sum6hr[-3]


##Loop to get values of max_RF for avg and sum and their associated dates
for (n in 1:length(v_index_avg3hr)){ 
  #avg 3hr
  date_max_avg_3hr_RF <- max_RF_per_day_avg_3hr$Date[v_index_avg3hr[n]]
  v_date_max_RF_avg3hr <- append(v_date_max_RF_avg3hr, date_max_avg_3hr_RF)
  max_avg_3hr_Rf <- max_RF_per_day_avg_3hr$max_RF[v_index_avg3hr[n]]
  v_max_Rf_avg3hr <- append(v_max_Rf_avg3hr,max_avg_3hr_Rf)
  
  #avg 6hr
  date_max_avg_6hr_RF <- max_RF_per_day_avg_6hr$Date[v_index_avg6hr[n]]
  v_date_max_RF_avg6hr <- append(v_date_max_RF_avg6hr, date_max_avg_6hr_RF)
  max_avg_6hr_Rf <- max_RF_per_day_avg_6hr$max_RF[v_index_avg6hr[n]]
  v_max_Rf_avg6hr <- append(v_max_Rf_avg6hr,max_avg_6hr_Rf)
  
  #sum 3hr
  date_max_sum_3hr_RF <- max_RF_per_day_sum_3hr$Date[v_index_sum3hr[n]]
  v_date_max_RF_sum3hr <- append(v_date_max_RF_sum3hr, date_max_sum_3hr_RF)
  max_sum_3hr_Rf <- max_RF_per_day_sum_3hr$max_RF[v_index_sum3hr[n]]
  v_max_Rf_sum3hr <- append(v_max_Rf_sum3hr,max_sum_3hr_Rf)
  
  #sum 6hr
  date_max_sum_6hr_RF <- max_RF_per_day_sum_6hr$Date[v_index_sum6hr[n]]
  v_date_max_RF_sum6hr <- append(v_date_max_RF_sum6hr, date_max_sum_6hr_RF)
  max_sum_6hr_Rf <- max_RF_per_day_sum_6hr$max_RF[v_index_sum6hr[n]]
  v_max_Rf_sum6hr <- append(v_max_Rf_sum6hr,max_sum_6hr_Rf)
}

#Create df with dates and max_RF
df_max_RF <- data.frame(date_rainMax_avg3hr = v_date_max_RF_avg3hr, RF_max_avg3hr = v_max_Rf_avg3hr,
                        date_rainMax_avg6hr = v_date_max_RF_avg6hr, RF_max_avg6hr = v_max_Rf_avg6hr,
                        date_rainMax_sum3hr = v_date_max_RF_sum3hr, RF_max_sum3hr = v_max_Rf_sum3hr,
                        date_rainMax_sum6hr = v_date_max_RF_sum6hr, RF_max_sum6hr = v_max_Rf_sum6hr)

#Add water year to the dataframe
df_max_RF$date_rainMax_avg3hr <- as.POSIXct(df_max_RF$date_rainMax_avg3hr, format = "%m/%d/%Y %H:%M", tz = "HST")
df_max_RF$date_rainMax_avg3hr <- df_max_RF$date_rainMax_avg3hr + 10*60*60 #Back to right day
df_max_RF$Water_Year <- df_max_RF$date_rainMax_avg3hr 
df_max_RF$Water_Year <- ifelse(month(df_max_RF$Water_Year) < 10, df_max_RF$Water_Year, df_max_RF$Water_Year + years(1))
df_max_RF$Water_Year <- as.POSIXct(df_max_RF$Water_Year, origin="1970-01-01", tz = "HST")
df_max_RF$Water_Year <- substr(df_max_RF$Water_Year,1,4)
df_max_RF <- df_max_RF[,c(9,1:8)]
df_max_RF$date_rainMax_avg3hr <- substr(df_max_RF$date_rainMax_avg3hr,1,10)

#Download the most recent data for the peakflow gauge associated with th rainfall station
siteNumber <- "16240500"
peakdata <- readNWISpeak(siteNumber)
peakdata <- peakdata[, c(2:3,5)]

#Add water year to the downloaded file
peakdata$peak_dt <- as.POSIXct(peakdata$peak_dt, format = "%m/%d/%Y %H:%M")
peakdata$peak_dt <- peakdata$peak_dt + 10*60*60+1800 #Back to right day
peakdata$Water_Year <- peakdata$peak_dt
peakdata$Water_Year <- ifelse(month(peakdata$Water_Year) < 10, peakdata$Water_Year, peakdata$Water_Year + years(1))
peakdata$Water_Year <- as.POSIXct(peakdata$Water_Year, origin="1970-01-01", tz = "HST")
peakdata$Water_Year <- substr(peakdata$Water_Year, 1,4)
peakdata$peak_dt <- substr(peakdata$peak_dt,1,10)

#Merge peakflow and rainfall data together and organize
RF_peakflow <- merge(df_max_RF, peakdata, by = "Water_Year")

#Calculate difference in days between peakflow and max_RF avg and sum
RF_peakflow$Different_day_avg3hr <- difftime(RF_peakflow$date_rainMax_avg3hr, RF_peakflow$peak_dt, units = "days")
RF_peakflow$Different_day_avg6hr <- difftime(RF_peakflow$date_rainMax_avg6hr, RF_peakflow$peak_dt, units = "days")
RF_peakflow$Different_day_avg6hr <- round(RF_peakflow$Different_day_avg6hr,0)
RF_peakflow$Different_day_sum3hr <- difftime(RF_peakflow$date_rainMax_sum3hr, RF_peakflow$peak_dt, units = "days")
RF_peakflow$Different_day_sum3hr <- round(RF_peakflow$Different_day_sum3hr,0)
RF_peakflow$Different_day_sum6hr <- difftime(RF_peakflow$date_rainMax_sum6hr, RF_peakflow$peak_dt, units = "days")
RF_peakflow$Different_day_sum6hr <- round(RF_peakflow$Different_day_sum6hr,0)

#Aggregate by water year and export table
#RF_water_year <- aggregate(RF_peakflow[,c(3,5,7,9)], list(Water_Year = RF_peakflow$Water_Year), max, na.rm = T)
#RF_water_year <- merge(RF_water_year, RF_peakflow, by = c("Water_Year", "RF_max_avg3hr", "RF_max_avg6hr",
                                                          #"RF_max_sum3hr", "RF_max_sum6hr"))
#RF_water_year <- RF_water_year[c(1,2,6,3,7,4,8,5,9,10:16)]
#write.csv(RF_water_year, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/aggregate_MNLH1_16240500.csv", row.names = F)

#Export table
write.csv(RF_peakflow, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/new_max_avg_sum_MNLH1_16240500.csv", row.names = F)









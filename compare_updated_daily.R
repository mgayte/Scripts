#############################################
#                                           #
#           Compare peakflow                #
#                  with                     #
#         more recent daily data            #
#                                           #
#############################################

#Load packages------------------------------------------------------------------------------------------
library(dplyr)
library(lubridate)
install.packages("dataRetrieval")
library(dataRetrieval)
#END----------------------------------------------------------------------------------------------------


#Get most recent daily data for the USGS (from website) gauge and MLNH1 (from yufen)--------------------
siteNumber <- "213215157552800"
daily_USGS <- readNWISdv(siteNumber, parameterCd = "00045", startDate = "", endDate = "", 
                         statCd="00006")
daily_MNLH1 <- read.csv("C:/Users/mgayt/Documents/NCDC_daily_precip_USC00517810.csv", header = T)
#END----------------------------------------------------------------------------------------------------


##Organize daily data-----------------------------------------------------------------------------------
  #daily_USGS
  daily_USGS <- daily_USGS[, c(3,6)] #keep relevant columns
  daily_USGS$X_00045_00006 <- daily_USGS$X_00045_00006*25.4 #in to mm
  names(daily_USGS)[c(1,2)] <- c("DateTime", "Rf_mm") #rename columns
  daily_USGS <- daily_USGS[c(13370:17056),] #keep only time period similar to the hourly data
  daily_USGS$DateTime <- as.POSIXct(daily_USGS$DateTime, format = "%Y-%m-%d %H:%M") #datetime format
  daily_USGS$DateTime <- daily_USGS$DateTime + 10*60*60 #Back to right day
  seq_USGS <- seq(as.POSIXct(daily_USGS$DateTime[1]), 
                  as.POSIXct(daily_USGS$DateTime[length(daily_USGS$DateTime)]), by = "day") #time sequence
  df_seq_USGS <- data.frame(DateTime = seq_USGS) #dataframe for perfect sequence of time
  daily_USGS <- merge(df_seq_USGS, daily_USGS, by = "DateTime", all.x = T) #merge perfect sequence of time
  daily_USGS$Water_Year <- ifelse(month(daily_USGS$DateTime) < 10, 
                                  daily_USGS$DateTime, daily_USGS$DateTime + years(1)) #create water year column
  daily_USGS$Water_Year <- as.POSIXct(daily_USGS$Water_Year, origin="1970-01-01", tz = "HST") #back to right format
  daily_USGS$Water_Year <- substr(daily_USGS$Water_Year,1,4) #keep only the year

  #daily_MNLH1
  names(daily_MNLH1)[c(1,2)] <- c("DateTime", "Rf_mm") #rename columns
  daily_MNLH1 <- daily_MNLH1[c(17223:24714),] #keep only time period similar to the hourly data
  daily_MNLH1$DateTime <- as.character(daily_MNLH1$DateTime) #from factor to character for datetime after
  daily_MNLH1$DateTime <- as.POSIXct(daily_MNLH1$DateTime, format = "%Y-%m-%d") #datetime format
  seq_MNLH1 <- seq(as.POSIXct(daily_MNLH1$DateTime[1]), 
                  as.POSIXct(daily_MNLH1$DateTime[length(daily_MNLH1$DateTime)]), by = "day") #time sequence
  df_seq_MNLH1 <- data.frame(DateTime = seq_MNLH1) #dataframe for perfect sequence of time
  daily_MNLH1 <- merge(df_seq_MNLH1, daily_MNLH1, by = "DateTime", all.x = T) #merge perfect sequence of time
  daily_MNLH1$Water_Year <- ifelse(month(daily_MNLH1$DateTime) < 10, 
                                  daily_MNLH1$DateTime, daily_MNLH1$DateTime + years(1)) #create water year column
  daily_MNLH1$Water_Year <- as.POSIXct(daily_MNLH1$Water_Year, origin="1970-01-01", tz = "HST") #back to right format
  daily_MNLH1$Water_Year <- substr(daily_MNLH1$Water_Year,1,4) #keep only the year
#END----------------------------------------------------------------------------------------------------
  
  
#Identify date of maximum amount of rainfall per year for USGS------------------------------------------
  ##Max RF per water year
  max_RF_per_year <- daily_USGS %>%
    group_by(Water_Year) %>%
    summarise(max_RF = max(Rf_mm, na.rm = T))
  
  ##Vectors that will be used in the following loops
  years <- unique(max_RF_per_year$Water_Year)
  max_RF <- max_RF_per_year$max_RF
  v_index <- c()
  v_date_max_RF <- c()
  v_max_Rf <- c()
  
  ##Loop to get position of max_RF 
  for (i in 1:length(years)){
    
    index <- which(daily_USGS$Rf_mm == max_RF[i] & daily_USGS$Water_Year == years[i])
    v_index <- append(v_index, index)
  }
  
  ##Loop to get the dates and RF amount matching with the positions found in the previous loop
  for (n in 1:length(v_index)){
    date_max_RF <- daily_USGS$DateTime[v_index[n]]
    v_date_max_RF <- append(v_date_max_RF, date_max_RF)
    max_Rf <- daily_USGS$Rf_mm[v_index[n]]
    v_max_Rf <- append(v_max_Rf, max_Rf)
  }
  
  #Create df with dates and max_RF
  df_max_daily_USGS <- data.frame(date_rainMax = v_date_max_RF, RF_max = v_max_Rf)
  
  #Add water year to the dataframe
  df_max_daily_USGS$Water_Year <- df_max_daily_USGS$date_rainMax 
  df_max_daily_USGS$Water_Year <- ifelse(month(df_max_daily_USGS$Water_Year) < 10, 
                                         df_max_daily_USGS$Water_Year, 
                                         df_max_daily_USGS$Water_Year + years(1))
  df_max_daily_USGS$Water_Year <- as.POSIXct(df_max_daily_USGS$Water_Year, 
                                             origin="1970-01-01", tz = "HST")
  df_max_daily_USGS$Water_Year <- substr(df_max_daily_USGS$Water_Year,1,4)
#END---------------------------------------------------------------------------------------------------  

  
#Identify date of maximum amount of rainfall per year for MNLH1----------------------------------------
  ##Max RF per water year
  max_RF_per_year <- daily_MNLH1 %>%
    group_by(Water_Year) %>%
    summarise(max_RF = max(Rf_mm, na.rm = T))
  
  ##Vectors that will be used in the following loops
  years <- unique(max_RF_per_year$Water_Year)
  max_RF <- max_RF_per_year$max_RF
  v_index <- c()
  v_date_max_RF <- c()
  v_max_Rf <- c()
  
  ##Loop to get position of max_RF 
  for (i in 1:length(years)){
    
    index <- which(daily_MNLH1$Rf_mm == max_RF[i] & daily_MNLH1$Water_Year == years[i])
    v_index <- append(v_index, index)
  }
  
  ##Loop to get the dates and RF amount matching with the positions found in the previous loop
  for (n in 1:length(v_index)){
    date_max_RF <- daily_MNLH1$DateTime[v_index[n]]
    v_date_max_RF <- append(v_date_max_RF, date_max_RF)
    max_Rf <- daily_MNLH1$Rf_mm[v_index[n]]
    v_max_Rf <- append(v_max_Rf, max_Rf)
  }
  
  #Create df with dates and max_RF
  df_max_daily_MNLH1 <- data.frame(date_rainMax = v_date_max_RF, RF_max = v_max_Rf)
  
  #Add water year to the dataframe
  df_max_daily_MNLH1$Water_Year <- df_max_daily_MNLH1$date_rainMax 
  df_max_daily_MNLH1$Water_Year <- ifelse(month(df_max_daily_MNLH1$Water_Year) < 10, 
                                         df_max_daily_MNLH1$Water_Year, 
                                         df_max_daily_MNLH1$Water_Year + years(1))
  df_max_daily_MNLH1$Water_Year <- as.POSIXct(df_max_daily_MNLH1$Water_Year, 
                                             origin="1970-01-01", tz = "HST")
  df_max_daily_MNLH1$Water_Year <- substr(df_max_daily_MNLH1$Water_Year,1,4)
#END---------------------------------------------------------------------------------------------------  
  
  
#Load peakflow data associated to the USGS (2 peakflow stations) and MNLH1 rainfall data-------------------------------------

  #USGS and 16296500
  siteNumber <- "16296500"
  peakdata_USGS1 <- readNWISpeak(siteNumber)
  peakdata_USGS1 <- peakdata_USGS1[, c(2:3,5)]
  
  #Add water year to the downloaded file
  peakdata_USGS1$peak_dt <- as.POSIXct(peakdata_USGS1$peak_dt, format = "%Y-%m-%d %H:%M")
  peakdata_USGS1$peak_dt <-peakdata_USGS1$peak_dt + 36000 #Back to right day
  peakdata_USGS1$Water_Year <- peakdata_USGS1$peak_dt
  peakdata_USGS1$Water_Year <- ifelse(month(peakdata_USGS1$Water_Year) < 10, 
                                      peakdata_USGS1$Water_Year, peakdata_USGS1$Water_Year + years(1))
  peakdata_USGS1$Water_Year <- as.POSIXct(peakdata_USGS1$Water_Year, origin="1970-01-01", tz = "HST")
  peakdata_USGS1$Water_Year <- substr(peakdata_USGS1$Water_Year, 1,4)
  peakdata_USGS1$peak_dt <- substr(peakdata_USGS1$peak_dt,1,10)
  
  #USGS and 16208000
  siteNumber <- "16208000"
  peakdata_USGS2 <- readNWISpeak(siteNumber)
  peakdata_USGS2 <- peakdata_USGS2[, c(2:3,5)]
  
  #Add water year to the downloaded file
  peakdata_USGS2$peak_dt <- as.POSIXct(peakdata_USGS2$peak_dt, format = "%Y-%m-%d %H:%M")
  peakdata_USGS2$peak_dt <- peakdata_USGS2$peak_dt + 36000 #Back to right day
  peakdata_USGS2$Water_Year <- peakdata_USGS2$peak_dt
  peakdata_USGS2$Water_Year <- ifelse(month(peakdata_USGS2$Water_Year) < 10, 
                                      peakdata_USGS2$Water_Year, peakdata_USGS2$Water_Year + years(1))
  peakdata_USGS2$Water_Year <- as.POSIXct(peakdata_USGS2$Water_Year, origin="1970-01-01", tz = "HST")
  peakdata_USGS2$Water_Year <- substr(peakdata_USGS2$Water_Year, 1,4)
  peakdata_USGS2$peak_dt <- substr(peakdata_USGS2$peak_dt,1,10)
  
  #MNLH1 and 16240500
  siteNumber <- "16240500"
  peakdata_MNLH1 <- readNWISpeak(siteNumber)
  peakdata_MNLH1 <- peakdata_MNLH1[, c(2:3,5)]
  
  #Add water year to the downloaded file
  peakdata_MNLH1$peak_dt <- as.POSIXct(peakdata_MNLH1$peak_dt, format = "%Y-%m-%d %H:%M")
  peakdata_MNLH1$peak_dt <- peakdata_MNLH1$peak_dt + 37800 #Back to right day
  peakdata_MNLH1$Water_Year <- peakdata_MNLH1$peak_dt
  peakdata_MNLH1$Water_Year <- ifelse(month(peakdata_MNLH1$Water_Year) < 10,
                                      peakdata_MNLH1$Water_Year, peakdata_MNLH1$Water_Year + years(1))
  peakdata_MNLH1$Water_Year <- as.POSIXct(peakdata_MNLH1$Water_Year, origin="1970-01-01", tz = "HST")
  peakdata_MNLH1$Water_Year <- substr(peakdata_MNLH1$Water_Year, 1,4)
  peakdata_MNLH1$peak_dt <- substr(peakdata_MNLH1$peak_dt,1,10)
#END----------------------------------------------------------------------------------------------------


#Calculate difference in time between max rainfall and peaflow for each stations------------------------

  #USGS and 16296500
  RF_peakflow_USGS1 <- merge(df_max_daily_USGS, peakdata_USGS1, by = "Water_Year")
  RF_peakflow_USGS1 <- RF_peakflow_USGS1[,-4]
  RF_peakflow_USGS1$date_rainMax <- as.Date(RF_peakflow_USGS1$date_rainMax)
  RF_peakflow_USGS1$peak_dt <- as.Date(RF_peakflow_USGS1$peak_dt)
  RF_peakflow_USGS1$Different_day <- difftime(RF_peakflow_USGS1$date_rainMax, 
                                              RF_peakflow_USGS1$peak_dt, units = "days")
  
  #USGS and 16208000
  RF_peakflow_USGS2 <- merge(df_max_daily_USGS, peakdata_USGS2, by = "Water_Year")
  RF_peakflow_USGS2 <- RF_peakflow_USGS2[,-4]
  RF_peakflow_USGS2$date_rainMax <- as.Date(RF_peakflow_USGS2$date_rainMax)
  RF_peakflow_USGS2$peak_dt <- as.Date(RF_peakflow_USGS2$peak_dt)
  RF_peakflow_USGS2$Different_day <- difftime(RF_peakflow_USGS2$date_rainMax, 
                                              RF_peakflow_USGS2$peak_dt, units = "days")
  
  #MNLH1 and 16240500
  RF_peakflow_MNLH1 <- merge(df_max_daily_MNLH1, peakdata_MNLH1, by = "Water_Year")
  RF_peakflow_MNLH1 <- RF_peakflow_MNLH1[,-4]
  RF_peakflow_MNLH1$date_rainMax <- as.Date(RF_peakflow_MNLH1$date_rainMax)
  RF_peakflow_MNLH1$peak_dt <- as.Date(RF_peakflow_MNLH1$peak_dt)
  RF_peakflow_MNLH1$Different_day <- difftime(RF_peakflow_MNLH1$date_rainMax, 
                                              RF_peakflow_MNLH1$peak_dt, units = "days")
#END---------------------------------------------------------------------------------------------------
  
  
#Export table------------------------------------------------------------------------------------------
  
write.csv(RF_peakflow_MNLH1, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/daily_updated_date_MNLH1_16240500.csv", row.names = F)
write.csv(RF_peakflow_USGS1, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/daily_updated_date_USGS_16296500.csv", row.names = F)
write.csv(RF_peakflow_USGS2, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/daily_updated_date_USGS_16208000.csv", row.names = F)
  

















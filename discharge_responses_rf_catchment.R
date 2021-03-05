#############################################
#                                           #
#           Discharge Response              #
#                  for                      #
#           Different Catchment             #
#                  for                      #
#                Yinphan                    #
#                                           #
#############################################


#Load library
library(dplyr)
library(naniar)
library(data.table)
library(zoo)
library(lubridate)
library(dataRetrieval)


##Get rainfall and discharge data for the desired locations-----------------------------------------------------------

  #Rainfall
  USGS_Wah <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/USGS_uv213215157552800.csv", header = T)
  USGS_Pun <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/USGS_uv213237157530701.csv", header = T)
  USGS_Mak <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/USGS_uv213335157540601.csv", header = T)
  COOP_Pun <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/USC00518314.csv", header = T)
  
  #Discharge
  siteNumbers <- c("16296500", "16301050", "16304200", "16200000")
  parameterCd <- "00060"
  startDate <- substr(USGS_Wah$DateTime[1],1,10)
  endDate <- substr(COOP_Pun$DateTime[length(COOP_Pun$DateTime)],1,10)
  
  Q_Kah <- readNWISuv(siteNumbers[1], parameterCd, startDate = startDate, endDate = endDate)
  Q_Pun <- readNWISuv(siteNumbers[2], parameterCd, startDate = startDate, endDate = endDate)
  Q_Kalu <- readNWISuv(siteNumbers[3], parameterCd, startDate = startDate, endDate = endDate)
  Q_Kauk <- readNWISuv(siteNumbers[4], parameterCd, startDate = startDate, endDate = endDate)
  
##END OF get rainfall and discharge data for the desired locations-----------------------------------------------------------
    
 
   
##Organize discharge and rainfall data--------------------------------------------------------------------------------------  

  #datetime format for the USGS/COOP data
  COOP_Pun$DateTime <- as.character(COOP_Pun$DateTime)
  COOP_Pun$DateTime <- as.POSIXct(COOP_Pun$DateTime, origin = "%Y-%m-%d %H:%M")
  USGS_Wah$DateTime <- as.character(USGS_Wah$DateTime)
  USGS_Wah$DateTime <- as.POSIXct(USGS_Wah$DateTime, origin = "%Y-%m-%d %H:%M")
  USGS_Pun$DateTime <- as.character(USGS_Pun$DateTime)
  USGS_Pun$DateTime <- as.POSIXct(USGS_Pun$DateTime, origin = "%Y-%m-%d %H:%M")
  USGS_Mak$DateTime <- as.character(USGS_Mak$DateTime)
  USGS_Mak$DateTime <- as.POSIXct(USGS_Mak$DateTime, origin = "%Y-%m-%d %H:%M")
  
  #Keep only shared time period for rainfall data
  COOP_Pun <- COOP_Pun[c(329090:417936),]
  USGS_Mak <- USGS_Mak[c(55284:144130),]
  USGS_Pun <- USGS_Pun[c(19370:108216),]
  USGS_Wah <- USGS_Wah[c(1:88847),]
  
  #Keep needed columns for discharge data
  Q_kah1 <- Q_Kah[,c(2:4)]
  Q_Kalu1 <- Q_Kalu[,c(2:4)]
  Q_Kauk1 <- Q_Kauk[,c(2:4)]
  Q_Pun1 <- Q_Pun[,c(2:4)]
  
  #Rename columns discharge
  names(Q_kah1)[3] <- "Q_cfs"
  names(Q_Kauk1)[3] <- "Q_cfs"
  names(Q_Kalu1)[3] <- "Q_cfs"
  names(Q_Pun1)[3] <- "Q_cfs"
  
  #Get discharge data in HST time
  Q_Pun1$dateTime <- as.POSIXct(Q_Pun1$dateTime, origin = "%Y-%m-%d %H:%M", tz = "GMT")
  Q_Pun1$dateTime <- as.POSIXct(format(Q_Pun1$dateTime, tz = "HST"))
  Q_Kalu1$dateTime <- as.POSIXct(Q_Kalu1$dateTime, origin = "%Y-%m-%d %H:%M", tz = "GMT")
  Q_Kalu1$dateTime <- as.POSIXct(format(Q_Kalu1$dateTime, tz = "HST"))
  Q_kah1$dateTime <- as.POSIXct(Q_kah1$dateTime, origin = "%Y-%m-%d %H:%M", tz = "GMT")
  Q_kah1$dateTime <- as.POSIXct(format(Q_kah1$dateTime, tz = "HST"))
  Q_Kauk1$dateTime <- as.POSIXct(Q_Kauk1$dateTime, origin = "%Y-%m-%d %H:%M", tz = "GMT")
  Q_Kauk1$dateTime <- as.POSIXct(format(Q_Kauk1$dateTime, tz = "HST"))
  
##END OF organize discharge and rainfall data------------------------------------------------------------- 
  
  
  
##Define a time to study and calculate cumulative sum for the rainfall data and standardized streamflow by drainage area---------------------------------------------------------  
 
  #Plot rainfall data
  par(mfrow = c(2,2))
  par(mar = c(2,2,2,2))
  plot(COOP_Pun$DateTime, COOP_Pun$RF_mm, type = "l")
  plot(USGS_Mak$DateTime, USGS_Mak$RF_mm, type = "l")
  plot(USGS_Pun$DateTime, USGS_Pun$RF_mm, type = "l")
  plot(USGS_Wah$DateTime, USGS_Wah$RF_mm, type = "l")
  
  #Keep rainfall data for the events occurring from 2016-07-24 18:00:00 to 2016-07-25 12:00:00
  COOP_Pun_event <- COOP_Pun[c(which(COOP_Pun$DateTime == "2016-07-24 18:00:00"):which(COOP_Pun$DateTime == "2016-07-25 12:00:00")),]
  USGS_Mak_event <- USGS_Mak[c(which(USGS_Mak$DateTime == "2016-07-24 18:00:00"):which(USGS_Mak$DateTime == "2016-07-25 12:00:00")),]
  USGS_Pun_event <- USGS_Pun[c(which(USGS_Pun$DateTime == "2016-07-24 18:00:00"):which(USGS_Pun$DateTime == "2016-07-25 12:00:00")),]
  USGS_Wah_event <- USGS_Wah[c(which(USGS_Wah$DateTime == "2016-07-24 18:00:00"):which(USGS_Wah$DateTime == "2016-07-25 12:00:00")),]    
  
  #Calculate accumulated and standardized accumulated rainfall
  COOP_Pun_event$Rf_acc <- cumsum(COOP_Pun_event$RF_mm)
  COOP_Pun_event$Rf_std_acc <- (COOP_Pun_event$Rf_acc/max(COOP_Pun_event$Rf_acc))
  USGS_Mak_event$Rf_acc <- cumsum(USGS_Mak_event$RF_mm)
  USGS_Mak_event$Rf_std_acc <- (USGS_Mak_event$Rf_acc/max(USGS_Mak_event$Rf_acc))
  USGS_Pun_event$Rf_acc <- cumsum(USGS_Pun_event$RF_mm)
  USGS_Pun_event$Rf_std_acc <- (USGS_Pun_event$Rf_acc/max(USGS_Pun_event$Rf_acc))
  USGS_Wah_event$Rf_acc <- cumsum(USGS_Wah_event$RF_mm)
  USGS_Wah_event$Rf_std_acc <- (USGS_Wah_event$Rf_acc/max(USGS_Wah_event$Rf_acc))
  
  #Keep discharge data for the event period 
  Q_kah1_event <- Q_kah1[c(188072:188145),]
  Q_Kalu1_event <- Q_Kalu1[c(which(Q_Kalu1$dateTime == "2016-07-24 18:00:00"):which(Q_Kalu1$dateTime == "2016-07-25 12:00:00")),]
  Q_Kauk1_event <- Q_Kauk1[c(which(Q_Kauk1$dateTime == "2016-07-24 18:00:00"):which(Q_Kauk1$dateTime == "2016-07-25 12:00:00")),]  
  Q_Pun1_event <- Q_Pun1[c(which(Q_Pun1$dateTime == "2016-07-24 18:00:00"):which(Q_Pun1$dateTime == "2016-07-25 12:00:00")),]  

  #Standardize streamflow by drainarge area. Make sure the unit is mm/s!!!!
  Q_kah1_event$Q_mm_s <- (Q_kah1_event$Q_cfs*0.0283168/(9660656))*1000
  Q_Kalu1_event$Q_mm_s <- (Q_Kalu1_event$Q_cfs*0.0283168/(2823087))*1000
  Q_Pun1_event$Q_mm_s <- (Q_Pun1_event$Q_cfs*0.0283168/(7174267))*1000
  Q_Kauk1_event$Q_mm_s <- (Q_Kauk1_event$Q_cfs*0.0283168/(3574184))*1000

##END OF define a time to study and Calculate cumulative sum for the rainfall data-------------------------------------------------- 

  
  
##Plot accumulated/std accumulated rainfall and discharge---------------------------------------------------------------------------  
  
  #plot std accumulated rainfall and discharge on two separated graphs
  par(mfrow = c(2,1))
  par(mar = c(3,4,1.5,1))
  plot(COOP_Pun_event$DateTime, COOP_Pun_event$Rf_acc, type = "l", col = "red",
       ylab = "Accumulated Rainfall (mm)", ylim = c(0,200))
  lines(USGS_Mak_event$DateTime, USGS_Mak_event$Rf_acc, type = "l", col = "green")
  lines(USGS_Pun_event$DateTime, USGS_Pun_event$Rf_acc, type = "l", col = "blue")
  lines(USGS_Wah_event$DateTime, USGS_Wah_event$Rf_acc, type = "l", col = "darkgoldenrod1")
  title("Event from 2016-07-24 18:00 to 2016-07-25 12:00", cex = 0.8)
  
  plot(Q_kah1_event$dateTime, Q_kah1_event$Q_mm_s, type = "l", col = "blue",
       ylab = "Discharge (mm/s)")
  lines(Q_Kalu1_event$dateTime, Q_Kalu1_event$Q_mm_s, type = "l", col = "red")
  lines(Q_Kauk1_event$dateTime, Q_Kauk1_event$Q_mm_s, type = "l", col = "darkgoldenrod1")
  lines(Q_Pun1_event$dateTime, Q_Pun1_event$Q_mm_s, type = "l", col = "green")
  
  

 
  
  
  
  
  
  
  
  
  
  
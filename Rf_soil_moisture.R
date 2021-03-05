#############################################
#                                           #
#  Rainfall/discharge and soil moisture     #
#                 of                        #
#           Lab Lyon Station                #
#                                           #
#############################################


#Load library
#install.packages("dataRetrieval")
library(dataRetrieval)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)



##Get rainfall/moisture and discharge data for the Lyon location and organize the data-----------------------------------------------------

  #Get rainfall and soil moisture and rename, change format of the datetime
  rf_sm <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/weather(soil)_data/Lab_weather_station/Lyon_weather_15minR.csv", header = T, sep = ",")
  names(rf_sm)[1] <- "dateTime"
  rf_sm$dateTime <- as.character(rf_sm$dateTime)
  rf_sm$dateTime <- as.POSIXct(rf_sm$dateTime, format = "%m/%d/%Y %H:%M")
  
  #Get the discharge data from the USGS website
  siteNumbers <- "16238500"
  parameter_cd <- "00060"
  startDate <- "2018-02-23"
  endDate <- "2020-12-03"
  Q_data <- readNWISuv(siteNumbers = siteNumbers, parameterCd = parameter_cd,
                       startDate = startDate, endDate = endDate)
  
  #Get right timezone for the discharge data
  Q_data$dateTime <- as.POSIXct(Q_data$dateTime, origin = "1960-01-01 00:00:00", tz = "GMT")
  Q_data$dateTime <- format(Q_data$dateTime, tz = "HST")
  Q_data$dateTime <- as.POSIXct(Q_data$dateTime) 
  
  #Keep columns we need for discharge data
  Q_data <- Q_data[,c(2:4)]
  names(Q_data)[3] <- "Q_cfs"
  
  #Define common time period and time interval (15min) between the datasets
  Q_data <- Q_data[c(51:251995),]
  #seq <- seq(Q_data$dateTime[1], Q_data$dateTime[length(Q_data$dateTime)], by = 60*15)
  #seq_df <- data.frame(dateTime = seq)
  
  #Sequence time by hour
  #seq_hr <- seq(Q_data$dateTime[1], Q_data$dateTime[length(Q_data$dateTime)], by = 60*60)
  #seq_hr_df <- data.frame(dateTime = seq_hr)
  
  #Get discharge for every 15min as we have some data with a 5 and 15 minutes intervals
  #Q_data$by15 <- cut(Q_data$dateTime, breaks="15 min")
  #Q_15_min <- aggregate(Q_cfs ~ by15, FUN = sum, data = Q_data )
  #names(Q_15_min)[1] <- "dateTime"
  
  #Get discharge for every hours 
  #Q_data$by60 <- cut(Q_data$dateTime, breaks="60 min")
  #Q_hr <- aggregate(Q_cfs ~ by60, FUN = sum, data = Q_data )
  #names(Q_hr)[1] <- "dateTime"
  
  #Get Q_15_min dateTime in a datetime format
  #Q_15_min$dateTime <- as.character(Q_15_min$dateTime)
  #Q_15_min$dateTime <- as.POSIXct(Q_15_min$dateTime, format = "%Y-%m-%d %H:%M")
  
  #Get Q_hr dateTime in a datetime format
  #Q_hr$dateTime <- as.character(Q_hr$dateTime)
  #Q_hr$dateTime <- as.POSIXct(Q_hr$dateTime, format = "%Y-%m-%d %H:%M")
  
  #Keep only needed columns in the soil moisture data
  rf_sm <- rf_sm[,c(1,20:22,26,27)]
  
  #Remove duplicated value in soil data
  rf_sm <- rf_sm[!duplicated(rf_sm$dateTime),]
  
  #Get rainfall and soil moisture for each hour
  #rf_sm$by60 <- cut(rf_sm$dateTime, breaks="60 min")
  #rf_sm_hr <- rf_sm %>%
    #group_by(by60) %>%
    #summarise(SoilMoisture_Avg.1. = sum(SoilMoisture_Avg.1.),
              #SoilMoisture_Avg.2. = sum(SoilMoisture_Avg.2.),
              #SoilMoisture_Avg.3. = sum(SoilMoisture_Avg.3.),
              #SoilMoisture_T_Avg = sum(SoilMoisture_T_Avg),
              #Rainfall_Tot = sum(Rainfall_Tot))
  #names(rf_sm_hr)[1] <- "dateTime"
  
  #Get right format for datetime in the rf_sm_hr data
  #rf_sm_hr$dateTime <- as.character(rf_sm_hr$dateTime)
  #rf_sm_hr$dateTime <- as.POSIXct(rf_sm_hr$dateTime, format = "%Y-%m-%d %H:%M")
  
  #Merge perfect 15min sequence time in the two data frames
  #rf_sm <- merge(seq_df, rf_sm, by = "dateTime", all.x = T)
  #Q_15_min <- merge(seq_df, Q_15_min, by = "dateTime", all.x = T)
  
  #Merge perfect hourly sequence time in the two data frames
  #rf_sm_hr <- merge(seq_hr_df, rf_sm_hr, by = "dateTime", all.x = T)
  #Q_hr <- merge(seq_hr_df, Q_hr, by = "dateTime", all.x = T)
  
  
##END OF get rainfall/moisture and discharge data for the Lyon location and organize the data--------------

  

##Plot moisture, rainfall, and discharge data------------------------------------------------------------
  
  #Plot all in one
  par(mar = c(3,5,0,0))
  par(fig=c(0,0.8,0,0.8))
  plot(Q_data$dateTime, Q_data$Q_cfs, type = "l", col = "black",
       ylab = "Streamflow (cfs)", xlab = "", ylim = c(0, max(Q_data$Q_cfs, na.rm = T)))
  par(new=T)
  par(fig=c(0,0.8,0.35,0.8))
  plot(rf_sm$Rainfall_Tot, col = "blue", lty = 12, type = "l",
          ylim = rev(c(0, max(rf_sm$Rainfall_Tot, na.rm = T))), ylab = "", xlab = "", xaxt = "n",
          yaxt = "n", bty = "n")
  axis(side = 4, ylim = c(0, max(rf_sm$Rainfall_Tot, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 3)
  par(new=T)
  par(fig = c(0,0.8,0,0.4))
  plot(rf_sm$SoilMoisture_Avg.1., col = "red", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n")
  axis(side = 4, ylim = c(0, max(rf_sm$SoilMoisture_Avg.1.1, na.rm = T)))
  mtext("Soil Moisture Avg", side = 4, line = 3)
  par(new=T)
  par(fig = c(0,0.8,0,0.4))
  plot(rf_sm$SoilMoisture_Avg.2., col = "green", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n")
  par(new=T)
  par(fig = c(0,0.8,0,0.4))
  plot(rf_sm$SoilMoisture_Avg.3., col = "coral4", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n")
  par(new=T)
  par(fig = c(0,0.8,0,0.4))
  plot(rf_sm$SoilMoisture_T_Avg, col = "deepskyblue", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n")
  
  
##END OF plot moisture, rainfall, and discharge data-----------------------------------------------------------

    

##Identify interesting events with variations between rainfall and discharge and plot them--------------------------------------------------------------  
 
  #Get rainfall/discharge in 2019 (after the soil moisture change) and 2020
  rf_2019 <- rf_sm[c(which(rf_sm$dateTime == "2019-01-01 00:00:00"):which(rf_sm$dateTime == "2020-01-01 00:00:00")),]
  rf_2020 <- rf_sm[c(which(rf_sm$dateTime == "2020-01-01 00:00:00"):which(rf_sm$dateTime == "2020-12-03 12:30:00")),]
  Q_2019 <- Q_data[c(which(Q_data$dateTime == "2019-01-01 00:00:00"):which(Q_data$dateTime == "2020-01-01 00:00:00")),]
  Q_2020 <- Q_data[c(which(Q_data$dateTime == "2020-01-01 00:00:00"):which(Q_data$dateTime == "2020-12-03 12:30:00")),]
  #Based on these tables different events have been defined (4 events)
  
  #Intermediate events with Rf = 28mm for Q = 866cfs and Rf = 12mm and 22mm for Q = 1457cfs
  interRF_event <- rf_2019[c(which(rf_2019$dateTime == "2019-09-13 16:30:00"):which(rf_2019$dateTime == "2019-09-26 19:30:00")),]
  interQ_event <- Q_2019[c(which(Q_2019$dateTime == "2019-09-13 16:30:00"):which(Q_2019$dateTime == "2019-09-26 19:30:00")),]
  
  #Plot intermediate event (september 2019)
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.5))
  plot(interQ_event$dateTime, interQ_event$Q_cfs, type = "l", col = "black",
       ylab = "", xlab = "", yaxt = "n", bty = "l")
  axis(2, line = 0)
  mtext("Streamflow (cfs)", side = 2, line = 2)
  par(new=T)
  par(fig=c(0,0.8,0.2,0.8))
  barplot(interRF_event$Rainfall_Tot, col = "blue", border = "blue",
       ylim = rev(c(0, max(interRF_event$Rainfall_Tot, na.rm = T))), ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n")
  axis(side = 4, ylim = c(0, max(interRF_event$Rainfall_Tot, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 2)
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(interRF_event$SoilMoisture_Avg.1., col = "red", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  axis(2,line=4)
  mtext("Soil Moisture (m^3/m^3)", side = 2, line = 6)
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(interRF_event$SoilMoisture_Avg.2., col = "green", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(interRF_event$SoilMoisture_Avg.3., col = "coral4", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  #par(new=T)
  #par(fig=c(0,0.8,0,0.8))
  #plot(interRF_event$SoilMoisture_T_Avg, col = "deepskyblue", type = "l", ylab = "", xlab = "", xaxt = "n",
       #yaxt = "n", bty = "n", ylim = c(0,1))
  title("September 2019")
  
  #Dry event Rf=21mm for Q=35cfs
  dryRF_event <- rf_2019[c(which(rf_2019$dateTime == "2019-11-15 17:30:00"):which(rf_2019$dateTime == "2019-11-17 23:30:00")),]
  dryQ_event <- Q_2019[c(which(Q_2019$dateTime == "2019-11-15 17:30:00"):which(Q_2019$dateTime == "2019-11-17 23:30:00")),]
  
  #Plot dry event
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.35))
  plot(dryQ_event$dateTime, dryQ_event$Q_cfs, type = "l", col = "black",
       ylab = "", xlab = "", yaxt = "n", bty = "l")
  axis(2, line = 0)
  mtext("Streamflow (cfs)", side = 2, line = 2)
  par(new=T)
  par(fig=c(0,0.8,0.2,0.8))
  barplot(dryRF_event$Rainfall_Tot, col = "blue", border = "blue",
       ylim = rev(c(0, max(dryRF_event$Rainfall_Tot, na.rm = T))), ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n")
  axis(side = 4, ylim = c(0, max(dryRF_event$Rainfall_Tot, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 2)
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(dryRF_event$SoilMoisture_Avg.1., col = "red", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  axis(2,line=4)
  mtext("Soil Moisture (m^3/m^3)", side = 2, line = 6)
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(dryRF_event$SoilMoisture_Avg.2., col = "green", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(dryRF_event$SoilMoisture_Avg.3., col = "coral4", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  #par(new=T)
  #par(fig=c(0,0.8,0,0.8))
  #plot(dryRF_event$SoilMoisture_T_Avg, col = "deepskyblue", type = "l", ylab = "", xlab = "", xaxt = "n",
       #yaxt = "n", bty = "n", ylim = c(0,1))
  title("November 2019")
  
  #Second dry event
  dryRF_event2 <- rf_2020[c(which(rf_2020$dateTime == "2020-04-11 00:00:00"):which(rf_2020$dateTime == "2020-04-17 00:00:00")),]
  dryQ_event2 <- Q_2020[c(which(Q_2020$dateTime == "2020-04-11 00:00:00"):which(Q_2020$dateTime == "2020-04-17 00:00:00")),]
  
  #Plot dry event
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.35))
  plot(dryQ_event2$dateTime, dryQ_event2$Q_cfs, type = "l", col = "black",
       ylab = "", xlab = "", yaxt = "n", bty = "l")
  axis(2, line = 0)
  mtext("Streamflow (cfs)", side = 2, line = 2)
  par(new=T)
  par(fig=c(0,0.8,0.2,0.8))
  barplot(dryRF_event2$Rainfall_Tot, col = "blue", border = "blue",
          ylim = rev(c(0, max(dryRF_event2$Rainfall_Tot, na.rm = T))), ylab = "", xlab = "", xaxt = "n",
          yaxt = "n", bty = "n")
  axis(side = 4, ylim = c(0, max(dryRF_event2$Rainfall_Tot, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 2)
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(dryRF_event2$SoilMoisture_Avg.1., col = "red", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  axis(2,line=4)
  mtext("Soil Moisture (m^3/m^3)", side = 2, line = 6)
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(dryRF_event2$SoilMoisture_Avg.2., col = "green", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(dryRF_event2$SoilMoisture_Avg.3., col = "coral4", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  #par(new=T)
  #par(fig=c(0,0.8,0,0.8))
  #plot(dryRF_event$SoilMoisture_T_Avg, col = "deepskyblue", type = "l", ylab = "", xlab = "", xaxt = "n",
  #yaxt = "n", bty = "n", ylim = c(0,1))
  title("April 2020")
  
  
  #High flow event. RF = 12.7 and 23.4 mm for Q = 2308 cfs
  highRF_event <- rf_2020[c(which(rf_2020$dateTime == "2020-01-09 16:30:00"):which(rf_2020$dateTime == "2020-01-11 23:30:00")),]
  highQ_event <- Q_2020[c(which(Q_2020$dateTime == "2020-01-09 16:30:00"):which(Q_2020$dateTime == "2020-01-11 23:30:00")),]
  
  #Plot high event
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.5))
  plot(highQ_event$dateTime, highQ_event$Q_cfs, type = "l", col = "black",
       ylab = "", xlab = "", bty = "l")
  mtext("Streamflow (cfs)", side = 2, line = 2)
  par(new=T)
  par(fig=c(0,0.8,0.2,0.8))
  barplot(highRF_event$Rainfall_Tot, col = "blue", border = "blue",
       ylim = rev(c(0, max(highRF_event$Rainfall_Tot, na.rm = T))), ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n")
  axis(side = 4, ylim = c(0, max(highRF_event$Rainfall_Tot, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 2)
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(highRF_event$SoilMoisture_Avg.1., col = "red", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  axis(2,line=4)
  mtext("Soil Moisture (m^3/m^3)", side = 2, line = 6)
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(highRF_event$SoilMoisture_Avg.2., col = "green", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(highRF_event$SoilMoisture_Avg.3., col = "coral4", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  #par(new=T)
  #par(fig=c(0,0.8,0,0.8))
  #plot(highRF_event$SoilMoisture_T_Avg, col = "deepskyblue", type = "l", ylab = "", xlab = "", xaxt = "n",
       #yaxt = "n", bty = "n", ylim = c(0,1))
  title("January 2020")
  
  #Similar to intermediate scenario
  sim_interRF_event <- rf_2020[c(which(rf_2020$dateTime == "2020-03-15 10:30:00"):which(rf_2020$dateTime == "2020-03-29 00:30:00")),]
  sim_interQ_event <- Q_2020[c(which(Q_2020$dateTime == "2020-03-15 10:30:00"):which(Q_2020$dateTime == "2020-03-29 00:30:00")),]
  
  #Plot similar to intermediate event
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.4))
  plot(sim_interQ_event$dateTime, sim_interQ_event$Q_cfs, type = "l", col = "black",
       ylab = "", xlab = "", bty = "l")
  mtext("Streamflow (cfs)", side = 2, line = 2)
  par(new=T)
  par(fig=c(0,0.8,0.3,0.8))
  barplot(sim_interRF_event$Rainfall_Tot, col = "blue",
       ylim = rev(c(0, max(sim_interRF_event$Rainfall_Tot, na.rm = T))), ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", border = "blue")
  axis(side = 4, ylim = c(0, max(sim_interRF_event$Rainfall_Tot, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 2)
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(sim_interRF_event$SoilMoisture_Avg.1., col = "red", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  axis(2,line=4)
  mtext("Soil Moisture (m^3/m^3)", side = 2, line = 6)
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(sim_interRF_event$SoilMoisture_Avg.2., col = "green", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(sim_interRF_event$SoilMoisture_Avg.3., col = "coral4", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  #par(new=T)
  #par(fig=c(0,0.8,0,0.8))
  #plot(sim_interRF_event$SoilMoisture_T_Avg, col = "deepskyblue", type = "l", ylab = "", xlab = "", xaxt = "n",
       #yaxt = "n", bty = "n", ylim = c(0,1))
  title("March 2020")
  
  #Similar to intermediate scenario
  sim2_interRF_event <- rf_2019[c(which(rf_2019$dateTime == "2019-02-12 00:00:00"):which(rf_2019$dateTime == "2019-02-18 00:00:00")),]
  sim2_interQ_event <- Q_2019[c(which(Q_2019$dateTime == "2019-02-12 00:00:00"):which(Q_2019$dateTime == "2019-02-18 00:00:00")),]
  
  #Plot similar to intermediate event
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.5))
  plot(sim2_interQ_event$dateTime, sim2_interQ_event$Q_cfs, type = "l", col = "black",
       ylab = "", xlab = "", bty = "l")
  mtext("Streamflow (cfs)", side = 2, line = 2)
  par(new=T)
  par(fig=c(0,0.8,0.3,0.8))
  barplot(sim2_interRF_event$Rainfall_Tot, col = "blue",
          ylim = rev(c(0, max(sim2_interRF_event$Rainfall_Tot, na.rm = T))), ylab = "", xlab = "", xaxt = "n",
          yaxt = "n", bty = "n", border = "blue")
  axis(side = 4, ylim = c(0, max(sim2_interRF_event$Rainfall_Tot, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 2)
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(sim2_interRF_event$SoilMoisture_Avg.1., col = "red", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  axis(2,line=4)
  mtext("Soil Moisture (m^3/m^3)", side = 2, line = 6)
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(sim2_interRF_event$SoilMoisture_Avg.2., col = "green", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(sim2_interRF_event$SoilMoisture_Avg.3., col = "coral4", type = "l", ylab = "", xlab = "", xaxt = "n",
       yaxt = "n", bty = "n", ylim = c(0,1))
  #par(new=T)
  #par(fig=c(0,0.8,0,0.8))
  #plot(sim2_interRF_event$SoilMoisture_T_Avg, col = "deepskyblue", type = "l", ylab = "", xlab = "", xaxt = "n",
  #yaxt = "n", bty = "n", ylim = c(0,1))
  title("February 2019")
  
  

##END OF identify interesting events with variations between rainfall and discharge and plot them----------------------

  
  
##Make a 3D plot of the rainfall/soil moisture/discharge for the entire time period-------------------------------  

  #Load package required
  #install.packages("scatterplot3d")
  library(scatterplot3d)
  #install.packages("rgl")
  library(rgl)
  #install.packages("magick")
  library(magick)
  
  #Get data from 2019-01-01 to last available data
  rf_sm_3D <- rf_sm[c(which(rf_sm$dateTime == "2019-01-01"):which(rf_sm$dateTime == "2020-12-03 12:30:00")),]
  Q_data_3D <- Q_data[c(which(Q_data$dateTime == "2019-01-01"):which(Q_data$dateTime == "2020-12-03 12:30:00")),]
  
  #Need to get same length in the data for the 3D plot
  Q_data_3D$by15 <- cut(Q_data_3D$dateTime, breaks="15 min") #divide data per 15min
  Q_15_min_3D <- aggregate(Q_cfs ~ by15, FUN = sum, data = Q_data_3D ) #sum Q for each 15min
  names(Q_15_min_3D)[1] <- "dateTime"
  seq <- seq(rf_sm_3D$dateTime[1], rf_sm_3D$dateTime[length(rf_sm_3D$dateTime)], by = 60*15)
  seq_df <- data.frame(dateTime = seq) #create time sequence
  Q_15_min_3D$dateTime <- as.character(Q_15_min_3D$dateTime) #dateTime from factor to character
  Q_15_min_3D$dateTime <- as.POSIXct(Q_15_min_3D$dateTime, format = "%Y-%m-%d %H:%M") #dateTime from character as date time 
  Q_15_min_3D <- merge(seq_df, Q_15_min_3D, by = "dateTime", all.x = T) #merge with perfect time sequence
  
  #3D plot
  plot3d(rf_sm_3D$Rainfall_Tot, rf_sm_3D$SoilMoisture_Avg.1., Q_15_min_3D$Q_cfs, xlab = "",
         ylab = "", zlab = "", col = "red", axes =F)
  plot3d(rf_sm_3D$Rainfall_Tot, rf_sm_3D$SoilMoisture_Avg.2., Q_15_min_3D$Q_cfs, xlab = "",
         ylab = "", zlab = "", col = "blue", add = T)
  plot3d(rf_sm_3D$Rainfall_Tot, rf_sm_3D$SoilMoisture_Avg.3., Q_15_min_3D$Q_cfs, xlab = "",
         ylab = "", zlab = "", col = "green", add =T)
  axes3d(edges = c("x--", "y+-", "z--"))
  mtext3d("Rainfall (mm)", edge="x--", line=2)
  mtext3d("Soil Moisture (m^3/m^3)", edge="y+-", line=3)
  mtext3d("Discharge (cfs)", edge="z--", line=3)
  legend3d("topright", title = "Soil Depth", legend = c("Surface", "0-30cm", "30-60cm"), 
           pch = 16, col = c("red", "blue", "green"), cex=1, inset=c(0.02))
  box3d()
  #play3d(spin3d(axis=c(0,0,1), rpm=4), duration=20) Play the 3D plot
  movie3d(spin3d(axis=c(0,0,1), rpm=4), duration=20, dir = "C:/Users/mgayt/Documents/test" )
  
##Look at soil moisture accumulation for 2 days before the different events----------------------------------------------
  
  ##Calculate accumulation for each of the events

  #Intermediate event first discharge pic !!!!NEED MODIFICATION!!!!
  acc_moist_inter_1 <-  interRF_event[c(which(interRF_event$dateTime == "2019-09-13 18:30:00"):which(interRF_event$dateTime == "2019-09-15 18:30:00")), c(1:6)]
  acc_moist_inter_1$acc_M1 <- cumsum(acc_moist_inter_1$SoilMoisture_Avg.1.)
  acc_moist_inter_1$acc_M2 <- cumsum(acc_moist_inter_1$SoilMoisture_Avg.2.)
  acc_moist_inter_1$acc_M3 <- cumsum(acc_moist_inter_1$SoilMoisture_Avg.3.)
  acc_moist_inter_1$acc_MT <- cumsum(acc_moist_inter_1$SoilMoisture_T_Avg)
  
  #Intermediate event second discharge pic
  acc_moist_inter_2 <-  interRF_event[c(which(interRF_event$dateTime == "2019-09-24 13:30:00"):which(interRF_event$dateTime == "2019-09-26 13:30:00")), c(1:6)]
  acc_moist_inter_2$acc_M1 <- cumsum(acc_moist_inter_2$SoilMoisture_Avg.1.)
  acc_moist_inter_2$acc_M2 <- cumsum(acc_moist_inter_2$SoilMoisture_Avg.2.)
  acc_moist_inter_2$acc_M3 <- cumsum(acc_moist_inter_2$SoilMoisture_Avg.3.)
  acc_moist_inter_2$acc_MT <- cumsum(acc_moist_inter_2$SoilMoisture_T_Avg)
  
  #Low flow event
  acc_moist_low <- dryRF_event[c(which(dryRF_event$dateTime == "2019-11-15 17:30:00"):which(dryRF_event$dateTime == "2019-11-17 17:30:00")), c(1:6)]
  acc_moist_low$acc_M1 <- cumsum(acc_moist_low$SoilMoisture_Avg.1.)
  acc_moist_low$acc_M2 <- cumsum(acc_moist_low$SoilMoisture_Avg.2.)
  acc_moist_low$acc_M3 <- cumsum(acc_moist_low$SoilMoisture_Avg.3.)
  acc_moist_low$acc_MT <- cumsum(acc_moist_low$SoilMoisture_T_Avg)
  
  #High flow event
  acc_moist_high <- highRF_event[c(which(highRF_event$dateTime == "2020-01-09 18:30:00"):which(highRF_event$dateTime == "2020-01-11 18:30:00")), c(1:6)]
  acc_moist_high$acc_M1 <- cumsum(acc_moist_high$SoilMoisture_Avg.1.)
  acc_moist_high$acc_M2 <- cumsum(acc_moist_high$SoilMoisture_Avg.2.)
  acc_moist_high$acc_M3 <- cumsum(acc_moist_high$SoilMoisture_Avg.3.)
  acc_moist_high$acc_MT <- cumsum(acc_moist_high$SoilMoisture_T_Avg)
  
  #Similar to intermediate event first discharge pic
  acc_moist_sim_inter1 <- sim_interRF_event[c(which(sim_interRF_event$dateTime == "2020-03-15 11:30:00"):which(sim_interRF_event$dateTime == "2020-03-17 11:30:00")), c(1:6)]
  acc_moist_sim_inter1$acc_M1 <- cumsum(acc_moist_sim_inter1$SoilMoisture_Avg.1.)
  acc_moist_sim_inter1$acc_M2 <- cumsum(acc_moist_sim_inter1$SoilMoisture_Avg.2.)
  acc_moist_sim_inter1$acc_M3 <- cumsum(acc_moist_sim_inter1$SoilMoisture_Avg.3.)
  acc_moist_sim_inter1$acc_MT <- cumsum(acc_moist_sim_inter1$SoilMoisture_T_Avg)
  
  #Similar to intermediate event second discharge pic
  acc_moist_sim_inter2 <- sim_interRF_event[c(which(sim_interRF_event$dateTime == "2020-03-26 16:30:00"):which(sim_interRF_event$dateTime == "2020-03-28 16:30:00")), c(1:6)]
  acc_moist_sim_inter2$acc_M1 <- cumsum(acc_moist_sim_inter2$SoilMoisture_Avg.1.)
  acc_moist_sim_inter2$acc_M2 <- cumsum(acc_moist_sim_inter2$SoilMoisture_Avg.2.)
  acc_moist_sim_inter2$acc_M3 <- cumsum(acc_moist_sim_inter2$SoilMoisture_Avg.3.)
  acc_moist_sim_inter2$acc_MT <- cumsum(acc_moist_sim_inter2$SoilMoisture_T_Avg)
  
  #Plot all the accumulation moisture events
  par(mfrow = c(3,2))
  plot(acc_moist_inter_1$dateTime, acc_moist_inter_1$acc_M1, type = "l", col = "red",
       xaxt = "n", ylab = "Soil Moisture", xlab = "2 days prior discharge")
  lines(acc_moist_inter_1$dateTime, acc_moist_inter_1$acc_M2, type = "l", col = "green")
  lines(acc_moist_inter_1$dateTime, acc_moist_inter_1$acc_M3, type = "l", col = "deeppink")
  lines(acc_moist_inter_1$dateTime, acc_moist_inter_1$acc_MT, type = "l", col = "deepskyblue")
  axis(1, at = c(0,25,50), labels = c("2","1","0"))
  title("Accumulated Soil Moisture intermediate event 1")
  
  plot(acc_moist_inter_2$dateTime, acc_moist_inter_2$acc_M1, type = "l", col = "red",
       xaxt = "n", ylab = "Soil Moisture", xlab = "2 days prior discharge")
  lines(acc_moist_inter_2$dateTime, acc_moist_inter_2$acc_M2, type = "l", col = "green")
  lines(acc_moist_inter_2$dateTime, acc_moist_inter_2$acc_M3, type = "l", col = "deeppink")
  lines(acc_moist_inter_2$dateTime, acc_moist_inter_2$acc_MT, type = "l", col = "deepskyblue")
  axis(1, at = c(0,25,50), labels = c("2","1","0"))
  title("Accumulated Soil Moisture intermediate event 1")  
  
  
  
  
  
  
  
  
 
  
  
 
  
  
  
  













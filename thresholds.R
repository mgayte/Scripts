#############################################
#                                           #
#         Threshold soil moisture           #
#                 and                       #
#         rainfall intensity LYON           #
#                                           #
#############################################

#Load library
library(dplyr)
library(dataRetrieval)
library(ggplot2)
library(tseries)


##Define at least 30 rainfall events from 2019 to the end of the data-------------------------------------------------------------------------

  #Load data
  rf_sm <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/weather(soil)_data/Lab_weather_station/Lyon_weather_15minR.csv", header = T)

  #datetime format
  names(rf_sm)[1] <- "dateTime"
  rf_sm$dateTime <- as.character(rf_sm$dateTime)
  rf_sm$dateTime <- as.POSIXct(rf_sm$dateTime, format = "%m/%d/%Y %H:%M")
  
  #Keep only data from 01-01-2019
  rf_sm <- rf_sm[c(13451:81413),]
  
  #Keep only needed columns in the soil moisture data
  rf_sm <- rf_sm[,c(1,20:22,26,27)]
  
  #Remove duplicated value in soil data
  rf_sm <- rf_sm[!duplicated(rf_sm$dateTime),]
  
  #Create a dataframe with hourly rainfall and not by 15min intervals
  rf_sm_hourly <- rf_sm %>%
    mutate(DateTime.H = as.POSIXct(strptime(rf_sm$dateTime,format="%Y-%m-%d %H"))) %>%
    group_by(DateTime.H) %>%
    summarise(RF_mm=sum(Rainfall_Tot))
  names(rf_sm_hourly)[1] <- "datetime"
  rf_sm_hourly <- as.data.frame(rf_sm_hourly)
  
  #Find 5 hours of consecutive 0 in the rainfall column
  dry_weather <- with(rle(rf_sm_hourly$RF_mm), {
    ok <- values == 0 & lengths > 4 #here we set up more than 4 rows as 5 rows equal 5 hours
    ends <- cumsum(lengths)
    starts <- ends - lengths + 1
    data.frame(starts, ends)[ok, ]
  }) #Give us row index when a dry event (at least 5 hours) starts and when it ends.
  
  #Get sum of rainfall for the storms identified 
  row_dry_weather <- nrow(dry_weather) #will be used as index for the loop
  
  ##empty vectors for the loop
  v_start_time <- c()
  v_end_time <- c()
  v_max_rf <- c()
  v_tot_rf <- c()
  
  for (i in 1:(row_dry_weather-1)){
    
    #Get start time of the storm
    start_time <- rf_sm_hourly$datetime[dry_weather[i,2]+1]
    v_start_time <- append(v_start_time, start_time)
    
    #Get end time 
    end_time <- rf_sm_hourly$datetime[dry_weather[(i+1),1]]
    v_end_time <- append(v_end_time, end_time)
    
    #Get sum of rainfall per event
    tot_rf <- sum(rf_sm_hourly[c((dry_weather[i,2]+1):(dry_weather[(i+1),1]-1)),2], na.rm = T)
    v_tot_rf <- append(v_tot_rf, tot_rf)
    
    #Get max hourly rainfall per event 
    max_rf <- max(rf_sm_hourly[c((dry_weather[i,2]+1):(dry_weather[(i+1),1]-1)),2], na.rm = T)
    v_max_rf <- append(v_max_rf, max_rf)
    
  }
  
  #Create dataframe with start/end time, sum of rainfall, and sum of flow for each storm
  df_rf_events <- data.frame(Start_time = v_start_time, End_time = v_end_time, Total_rf = v_tot_rf,
                               max_hourly_rf = v_max_rf)
  
  #Delete events with less than 10mm of accumulated rainfall and max hourly rainfall less 5mm/h
  df_rf_events <- df_rf_events[df_rf_events$Total_rf >= 10,]
  df_rf_events <- df_rf_events[df_rf_events$max_hourly >= 5,]
  
  #Get the discharge data from the USGS website
  siteNumbers <- "16238500"
  parameter_cd <- "00060"
  startDate <- "2019-01-01"
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
  
  #Aggregate discharge data per hour
  Q_hourly <- Q_data %>%
    mutate(DateTime.H = as.POSIXct(strptime(Q_data$dateTime,format="%Y-%m-%d %H"))) %>%
    group_by(DateTime.H) %>%
    summarise(Q_cfs=sum(Q_cfs))
  names(Q_hourly)[1] <- "datetime"
  Q_hourly <- as.data.frame(Q_hourly)
  
  #Perfect time sequence for the discharge data
  seq_hr <- seq(Q_data$dateTime[1], Q_data$dateTime[length(Q_data$dateTime)], by = 60*60)
  seq_hr_df <- data.frame(datetime = seq_hr)
  Q_hourly <- merge(seq_hr_df,Q_hourly, by = "datetime", all.x = T )
  
  #Add max discharge for each event in the dataframe
  v_max_Q <- c()
  for(i in 1:length(df_rf_events$Start_time)){
    max_Q <- max(Q_hourly$Q_cfs[c(which(Q_hourly$datetime == df_rf_events$Start_time[i]):
                           which(Q_hourly$datetime == df_rf_events$End_time[i]))], na.rm=T)
    v_max_Q <- append(v_max_Q, max_Q)
  }
  df_max_Q <- data.frame(max_Q_event = v_max_Q)
  
  #Merge max discharge per event to the event dataframe
  df_rf_events <- cbind(df_rf_events, df_max_Q)
  
  #Plots test
  par(mfrow=c(2,1))
  par(mar = c(3,5,0,2))
  plot(Q_hourly$datetime[10709:10930], Q_hourly$Q_cfs[10709:10930], type= "l", col = "black", ylab = "Discharge_cfs")
  plot(rf_sm_hourly$datetime[10709:10930], rf_sm_hourly$RF_mm[10709:10930], type ="l", col = "blue", ylab = "Rainfall_mm")

##END OF define at least 30 rainfall events from 2019 to the end of the data-------------------------------   

  
  
##Get soil moisture data in mm for each start dates of the events------------------------------------------  

  #Based on Saffarpour, McGuire, and Haga method
  ASI <- (rf_sm$SoilMoisture_Avg.2.+rf_sm$SoilMoisture_Avg.3.)*300
  ASI <- data.frame(ASI_mm=ASI)
  rf_sm <- cbind(rf_sm, ASI)
  
  #Same method but only for 0-30cm
  ASI_30 <- rf_sm$SoilMoisture_Avg.2.*300
  ASI_30 <- data.frame(ASI_30_mm=ASI_30)
  rf_sm <- cbind(rf_sm, ASI_30)
  
  #Same method but only for the surface so 5cm
  ASI_surface <- rf_sm$SoilMoisture_Avg.1.*50
  ASI_surface <- data.frame(ASI_surface_mm=ASI_surface)
  rf_sm <- cbind(rf_sm, ASI_surface)
  
  #Get ASI for each beginning of events
  v_ASI <- c()
  for(i in 1:length(df_rf_events$Start_time)){
    ASI_index <- rf_sm$ASI_mm[which(rf_sm$dateTime == df_rf_events$Start_time[i])]
    v_ASI <- append(v_ASI, ASI_index)
  }
  ASI_event <- data.frame(ASI_event_mm = v_ASI)
  df_rf_events <- cbind(df_rf_events, ASI_event)
  
  #Get ASI_30 for each beginning of events
  v_ASI_30 <- c()
  for(i in 1:length(df_rf_events$Start_time)){
    ASI_30_index <- rf_sm$ASI_30_mm[which(rf_sm$dateTime == df_rf_events$Start_time[i])]
    v_ASI_30 <- append(v_ASI_30, ASI_30_index)
  }
  ASI_30_event <- data.frame(ASI_30_event_mm = v_ASI_30)
  df_rf_events <- cbind(df_rf_events, ASI_30_event)
  
  #Get ASI_surface for each beginning of events
  v_ASI_surface <- c()
  for(i in 1:length(df_rf_events$Start_time)){
    ASI_surface_index <- rf_sm$ASI_surface_mm[which(rf_sm$dateTime == df_rf_events$Start_time[i])]
    v_ASI_surface <- append(v_ASI_surface, ASI_surface_index)
  }
  ASI_surface_event <- data.frame(ASI_surface_event_mm = v_ASI_surface)
  df_rf_events <- cbind(df_rf_events, ASI_surface_event)
  
  #Calculate ASI+rainfall
  df_rf_events$ASI_rf_mm <- df_rf_events$Total_rf+df_rf_events$ASI_event_mm
  
  #Calculate ASI_30+rainfall
  df_rf_events$ASI_30_rf_mm <- df_rf_events$Total_rf+df_rf_events$ASI_30_event_mm
  
  #Calculate ASI_surface+rainfall
  df_rf_events$ASI_surface_rf_mm <- df_rf_events$Total_rf+df_rf_events$ASI_surface_event_mm
  
  #Remove the events with incorrect soil moisture data (before April 27th 2019)
  df_rf_gd_events <- df_rf_events[-c(1:8),]
  

## END OF get soil moisture data in mm for each start dates of the events------------------------------------------

  
  
##Plot graph with ASI+rainfall(0-60cm), rainfall intensity and max event discharge------------------------------------------

  #All discharge
  ggplot(data = df_rf_gd_events) + geom_point(mapping = aes(x = ASI_rf_mm, y = max_hourly_rf,
               color = max_Q_event, size = max_Q_event)) + 
               scale_color_gradient(low="blue", high="red") +
               xlim(250,650)+ ylim(5,26)+
               xlab("Antecendent Soil Moisture Index + Rainfall accumulation (mm)")+
               ylab("Maximum hourly intensity (mm/hr)")+
               labs(color = "Maximum Discharge per event (cfs)")+
               labs(size = "Maximum Discharge per event (cfs)")
  
  #90th, 95th, 99th percentile discharge
  quantiles <- quantile(df_rf_gd_events$max_Q_event, c(0.90, 0.95, 0.99))
  df_rf_gd_events90 <- df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantiles[1],]
  df_rf_gd_events95 <- df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantiles[2],]
  df_rf_gd_events99 <- df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantiles[3],]
  
  #90th percentile plot
  ggplot(data = df_rf_gd_events90) + geom_point(mapping = aes(x = ASI_rf_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red", 
          limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
          breaks = c(500,1000,1500,2000)) +
    scale_size(limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
               breaks = c(500,1000,1500,2000)) +
    xlim(250,650)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index + Rainfall accumulation (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "90th percentile maximum discharge per event (cfs)")+
    labs(size = "90th percentile maximum discharge per event (cfs)")
  
  #95th percentile plot
  ggplot(data = df_rf_gd_events95) + geom_point(mapping = aes(x = ASI_rf_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red", 
    limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(500,1000,1500,2000)) +
    scale_size(limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(500,1000,1500,2000)) +
    xlim(250,650)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index + Rainfall accumulation (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "95th percentile maximum discharge per event (cfs)")+
    labs(size = "95th percentile maximum discharge per event (cfs)")
  
  #ASI only
  ggplot(data = df_rf_gd_events) + geom_point(mapping = aes(x = ASI_event_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red") +
    xlim(250,400)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  #Rainfall accumulation only
  ggplot(data = df_rf_gd_events) + geom_point(mapping = aes(x = Total_rf, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red") +
    xlim(0,300)+ ylim(5,26)+
    xlab("Total rainfall (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
 
##END OF plot graph with ASI+rainfall, rainfall intensity and max event discharge------------------------------  



##Plot graph with ASI_30+rainfall, rainfall intensity and max event discharge------------------------------------------

  #All discharge with cross for events at the 75 percentile discharge
  ggplot(data = df_rf_gd_events) + 
    geom_point(mapping = aes(x = ASI_30_rf_mm, y = max_hourly_rf, colour = max_Q_event, size = max_Q_event,)) +
    geom_point(data = df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantile(df_rf_gd_events$max_Q_event, 0.75),], 
               aes(x = ASI_30_rf_mm, y = max_hourly_rf), pch=4, fill=NA, colour="black", stroke=1)+
    scale_color_gradient(low="blue", high="red") +
    xlim(110,500)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index 30cm + Rainfall accumulation (mm)")+
    ylab("Maximum hourly rainfall intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  #90th, 95th, 99th percentile discharge
  quantiles <- quantile(df_rf_gd_events$max_Q_event, c(0.90, 0.95, 0.99))
  df_rf_gd_events90 <- df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantiles[1],]
  df_rf_gd_events95 <- df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantiles[2],]
  df_rf_gd_events99 <- df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantiles[3],]
  
  #90th percentile plot
  ggplot(data = df_rf_gd_events90) + geom_point(mapping = aes(x = ASI_30_rf_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red", 
    limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(500,1000,1500,2000)) +
    scale_size(limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(500,1000,1500,2000)) +
    xlim(150,500)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index 30cm + Rainfall accumulation (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "90th percentile maximum discharge per event (cfs)")+
    labs(size = "90th percentile maximum discharge per event (cfs)")
  
  #95th percentile plot
  ggplot(data = df_rf_gd_events95) + geom_point(mapping = aes(x = ASI_30_rf_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red", 
    limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(500,1000,1500,2000)) +
    scale_size(limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(500,1000,1500,2000)) +
    xlim(150,500)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index 30cm + Rainfall accumulation (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "95th percentile maximum discharge per event (cfs)")+
    labs(size = "95th percentile maximum discharge per event (cfs)")
  
  #ASI only
  ggplot(data = df_rf_gd_events) + geom_point(mapping = aes(x = ASI_30_event_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red") +
    xlim(100,200)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index 30cm (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  #Rainfall accumulation only
  ggplot(data = df_rf_gd_events) + geom_point(mapping = aes(x = Total_rf, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red") +
    xlim(0,300)+ ylim(5,26)+
    xlab("Total rainfall (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
##END OF plot graph with ASI_30+rainfall, rainfall intensity and max event discharge------------------------------  

  
  
##Plot graph with ASI_surface+rainfall, rainfall intensity and max event discharge------------------------------------------
  
  #All discharge with cross for events at the 75 percentile discharge
  ggplot(data = df_rf_gd_events) + 
    geom_point(mapping = aes(x = ASI_surface_rf_mm, y = max_hourly_rf, colour = max_Q_event, size = max_Q_event,)) +
    geom_point(data = df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantile(df_rf_gd_events$max_Q_event, 0.75),], 
               aes(x = ASI_surface_rf_mm, y = max_hourly_rf), pch=4, fill=NA, colour="black", stroke=1)+
    scale_color_gradient(low="blue", high="red") +
    xlim(20,310)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index Surface(0-5cm) + Rainfall accumulation (mm)")+
    ylab("Maximum hourly rainfall intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  #90th, 95th, 99th percentile discharge
  quantiles <- quantile(df_rf_gd_events$max_Q_event, c(0.90, 0.95, 0.99))
  df_rf_gd_events90 <- df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantiles[1],]
  df_rf_gd_events95 <- df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantiles[2],]
  df_rf_gd_events99 <- df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantiles[3],]
  
  #ASI only
  ggplot(data = df_rf_gd_events) + geom_point(mapping = aes(x = ASI_surface_event_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red") +
    xlim(10,40)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index Surface0-5cm (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  #Rainfall accumulation only
  ggplot(data = df_rf_gd_events) + geom_point(mapping = aes(x = Total_rf, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red") +
    xlim(0,300)+ ylim(5,26)+
    xlab("Total rainfall (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
##END OF plot graph with ASI_30+rainfall, rainfall intensity and max event discharge------------------------------  
  
  
  
##Define correlation factor between rainfall intensity/ASI and discharge---------------------------------------------
  
  #Rainfall intensity and discharge
  cor(rf_sm_hourly$RF_mm, Q_hourly$Q_cfs[1:16861], method = "pearson", use = "complete.obs")
  
  #ASI and discharge
  ASI_hourly <- rf_sm %>%
    mutate(DateTime.H = as.POSIXct(strptime(rf_sm$dateTime,format="%Y-%m-%d %H"))) %>%
    group_by(DateTime.H) %>%
    summarise(ASI=mean(ASI_mm))
  names(ASI_hourly)[1] <- "datetime"
  ASI_hourly <- as.data.frame(ASI_hourly)
  rf_sm_hourly <- cbind(rf_sm_hourly, ASI_hourly)
  rf_sm_hourly <- rf_sm_hourly[,-3]
  cor(rf_sm_hourly$ASI, Q_hourly$Q_cfs[1:16861], method = "pearson", use = "complete.obs")
  
##END OF define correlation factor between rainfall intensity/ASI and discharge-----------------------------
  
  
  
##Linear regression for the high discharge events--------------------------------------------------  

  #Create dataframe with only the events exceeding the thresholds for ASI(0-60cm),ASI_30(0-30cm), and surface (0-5cm)
  thre_events <- df_rf_gd_events[df_rf_gd_events$ASI_rf_mm >= 400 & df_rf_gd_events$max_hourly_rf >= 10,]
  #thre_events_30 <- df_rf_gd_events[df_rf_gd_events$ASI_30_rf_mm >= 235 & df_rf_gd_events$max_hourly_rf >= 10,]
  thre_events_30 <- df_rf_gd_events[df_rf_gd_events$ASI_30_rf_mm >= 250,]
  thre_events_surface <- df_rf_gd_events[df_rf_gd_events$ASI_surface_rf_mm >= 110 & df_rf_gd_events$max_hourly_rf >= 10,]
  
  #Plot the events above threshold values
  ggplot(data = thre_events) + geom_point(mapping = aes(x = ASI_rf_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red",
    limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(500,1000,1500,2000)) +
    scale_size(limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(500,1000,1500,2000)) +                   
    xlim(250,650)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index + Rainfall accumulation (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  #Plot the events above threshold values for ASI 0-30cm
  ggplot(data = thre_events_30) + geom_point(mapping = aes(x = ASI_30_rf_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red",
    limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(500,1000,1500,2000)) +
    scale_size(limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(500,1000,1500,2000)) +                   
    xlim(200,500)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index 30cm + Rainfall accumulation (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  #Plot the events above threshold values for ASI surface (0-5cm)
  ggplot(data = thre_events_surface) + geom_point(mapping = aes(x = ASI_surface_rf_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red",
                         limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
                         breaks = c(500,1000,1500,2000)) +
    scale_size(limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
               breaks = c(500,1000,1500,2000)) +                   
    xlim(70,310)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index Surface + Rainfall accumulation (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  #Delete events occurring before April 2019 NO NEED ANYMORE DONE EARLIER!!!!!
  #thre_events <- thre_events[-c(1,2),]
  #thre_events_30 <- thre_events_30[-c(1,2,3),]
  
  #Calculate accumulated rainfall averaged by day (not depending anymore on the length of the event)
  thre_events$rf_acc_day <- (difftime(thre_events$End_time, thre_events$Start_time, units = "day"))
  thre_events$rf_acc_day <- as.numeric(thre_events$rf_acc_day)
  thre_events$rf_acc_day <- thre_events$Total_rf/thre_events$rf_acc_day
  thre_events_30$rf_acc_day <- (difftime(thre_events_30$End_time, thre_events_30$Start_time, units = "day"))
  thre_events_30$rf_acc_day <- as.numeric(thre_events_30$rf_acc_day)
  thre_events_30$rf_acc_day <- thre_events_30$Total_rf/thre_events_30$rf_acc_day
  thre_events_surface$rf_acc_day <- (difftime(thre_events_surface$End_time, thre_events_surface$Start_time, units = "day"))
  thre_events_surface$rf_acc_day <- as.numeric(thre_events_surface$rf_acc_day)
  thre_events_surface$rf_acc_day <- thre_events_surface$Total_rf/thre_events_surface$rf_acc_day
  
  #Linear regression for ASI 0-60cm 
  fit1 <- lm(max_Q_event ~ max_hourly_rf + ASI_event_mm, data=thre_events)
  summary(fit1) # show results
  fit2 <- lm(max_Q_event ~ max_hourly_rf + ASI_event_mm + rf_acc_day, data=thre_events)
  summary(fit2) # show results
  
  #Linear regression for ASI 0-30cm 
  fit3 <- lm(max_Q_event ~ max_hourly_rf + ASI_30_event_mm, data=thre_events_30)
  summary(fit3) # show results
  fit4 <- lm(max_Q_event ~ max_hourly_rf + ASI_30_event_mm + Total_rf, data=thre_events_30)
  summary(fit4) # show results (better results with Total_rf for the new threshold values)
  
  #Linear regression for ASI Surface (0-5cm) 
  fit5 <- lm(max_Q_event ~ max_hourly_rf + ASI_surface_event_mm, data=thre_events_surface)
  summary(fit5) # show results
  fit6 <- lm(max_Q_event ~ max_hourly_rf + ASI_surface_event_mm + Total_rf, data=thre_events_surface)
  summary(fit6) # show results
  
  #Use correlation factor
  thre_events$lm1 <- thre_events$max_hourly_rf*90.340+thre_events$ASI_event_mm*14.226
  thre_events$lm2 <- thre_events$max_hourly_rf*96.003+thre_events$ASI_event_mm*16.563+thre_events$rf_acc_day*2.907
  #thre_events_30$lm1 <- thre_events_30$max_hourly_rf*87.371+thre_events_30$ASI_event_mm*18.616
  #thre_events_30$lm2 <- thre_events_30$max_hourly_rf*91.749+thre_events_30$ASI_event_mm*21.260+thre_events_30$rf_acc_day*2.469
  thre_events_30$lm1 <- thre_events_30$max_hourly_rf*74.69+thre_events_30$ASI_event_mm*76.07
  thre_events_30$lm2 <- thre_events_30$max_hourly_rf*103.320+thre_events_30$ASI_event_mm*18.375-thre_events_30$Total_rf*1.703
  thre_events_surface$lm1 <- thre_events_surface$max_hourly_rf*76.28+thre_events_surface$ASI_surface_event_mm*83.27
  thre_events_surface$lm2 <- thre_events_surface$max_hourly_rf*100.978+thre_events_surface$ASI_surface_event_mm*65.887-thre_events_surface$Total_rf*1.625
  
 
##END OF linear regression for the high discharge events-------------------------------------------------- 
 
  
  
##Linear regression with log values-----------------------------------------------------------------------   
  
  #Log values for dataframe with ASI 0-60cm
  thre_events$log_rf <- log(thre_events$max_hourly_rf)
  thre_events$log_ASI <- log(thre_events$ASI_event_mm)
  thre_events$log_ASI_rf <- log(thre_events$ASI_rf_mm)
  thre_events$log_Q <- log(thre_events$max_Q_event)
  thre_events$log_rf_day <- log(thre_events$rf_acc_day)
  
  #Log values for dataframe with ASI 0-30cm
  thre_events_30$log_rf <- log(thre_events_30$max_hourly_rf)
  thre_events_30$log_ASI <- log(thre_events_30$ASI_30_event_mm)
  thre_events_30$log_ASI_rf <- log(thre_events_30$ASI_30_rf_mm)
  thre_events_30$log_Q <- log(thre_events_30$max_Q_event)
  thre_events_30$log_rf_day <- log(thre_events_30$rf_acc_day)
  thre_events_30$log_tot_rf <- log(thre_events_30$Total_rf)
  
  #Log values for dataframe with ASI Surface (0-5cm)
  thre_events_surface$log_rf <- log(thre_events_surface$max_hourly_rf)
  thre_events_surface$log_ASI <- log(thre_events_surface$ASI_surface_event_mm)
  thre_events_surface$log_ASI_rf <- log(thre_events_surface$ASI_surface_rf_mm)
  thre_events_surface$log_Q <- log(thre_events_surface$max_Q_event)
  thre_events_surface$log_rf_day <- log(thre_events_surface$rf_acc_day)
  thre_events_surface$log_tot_rf <- log(thre_events_surface$Total_rf)
  
  #Linear regression for ASI 0-60cm 
  log_fit1 <- lm(log_Q ~ log_rf + log_ASI, data=thre_events)
  summary(log_fit1) # show results
  log_fit2 <- lm(log_Q ~ log_rf + log_ASI + log_rf_day, data=thre_events)
  summary(log_fit2) # show results
  
  #Linear regression for ASI 0-30cm 
  log_fit3 <- lm(log_Q ~ log_rf + log_ASI, data=thre_events_30)
  summary(log_fit3) # show results
  log_fit4 <- lm(log_Q ~ log_rf + log_ASI + log_tot_rf, data=thre_events_30)
  summary(log_fit4) # show results
  
  #Linear regression for ASI Surface(0-5cm) 
  log_fit5 <- lm(log_Q ~ log_rf + log_ASI, data=thre_events_surface)
  summary(log_fit5) # show results --> not good R ~ 35%
  log_fit6 <- lm(log_Q ~ log_rf + log_ASI + log_tot_rf, data=thre_events_surface)
  summary(log_fit6) # show results --> not good R ~40%
  
  #Use correlation factor
  thre_events$log_lm1 <- thre_events$log_rf*1.2389+thre_events$log_ASI*3.8995
  thre_events$log_lm2 <- thre_events$log_rf*1.4262+thre_events$log_ASI*5.3301+thre_events$log_rf_day*0.3753
  #thre_events_30$log_lm1 <- thre_events_30$log_rf*1.2056+thre_events_30$log_ASI*2.4362
  #thre_events_30$log_lm2 <- thre_events_30$log_rf*1.3685+thre_events_30$log_ASI*3.2728+thre_events_30$log_rf_day*0.3495
  thre_events_30$log_lm1 <- thre_events_30$log_rf*1.295+thre_events_30$log_ASI*2.433
  thre_events_30$log_lm2 <- thre_events_30$log_rf*1.3931+thre_events_30$log_ASI*2.3907-thre_events_30$log_tot_rf*0.1366
  
  #Organize dataframe
  thre_events <- thre_events[,c(1:8,11:15,9,10,16,17)]
  thre_events_30 <- thre_events_30[,c(1:10,13:17,11,12,18,19)]
  
  #Test linear regression
  log_test <- lm(max_Q_event ~ log_rf + log_ASI , data=thre_events_surface)
  summary(log_test)
  #thre_events_30$log_test <- thre_events_30$log_rf*1.178545+thre_events_30$ASI_30_event_mm*0.015724
  thre_events_30$log_test <- thre_events_30$log_rf*1.270095+thre_events_30$ASI_30_event_mm*0.015755
  thre_events_30$log_test <- thre_events_30$log_rf*1.365626+thre_events_30$ASI_30_event_mm*0.020008+thre_events_30$log_rf_day*0.272049
  
  #3D plot
  plot3d(thre_events_30$max_hourly_rf, thre_events_30$ASI_30_event_mm, thre_events$max_Q_event,
         xlab = "", ylab = "", zlab = "", col = "red", axes =F)
  axes3d(edges = c("x--", "y+-", "z--"))
  mtext3d("Max Hourly Rainfall (mm)", edge="x--", line=2)
  mtext3d("Antecedent Soil Moisture + Rainfall accumulation (mm)", edge="y+-", line=3)
  mtext3d("Max event Discharge (cfs)", edge="z--", line=3)
  box3d()
  
##END OF linear regression with log values-----------------------------------------------------------------------    
 
  

##Cross-correlation to identify potential lag between ASI and discharge----------------------------------------- 
  
  #Get same time period for ASI and discharge
  rf_sm_ccf <- rf_sm_hourly[c(which(rf_sm_hourly$datetime=="2019-04-26 15:00:00"):16861),]
  Q_ccf <- Q_hourly[c(which(rf_sm_hourly$datetime=="2019-04-26 15:00:00"):16861),]
  
  #Cross-correlation
  ccfvalues <- ccf(rf_sm_ccf$RF_mm,Q_ccf$Q_cfs,10, na.action = na.pass)
  
  #Linear regression
  
  
  
  
##Trials or garbage below---------------------------------------------------------------------------------
  
  rf_sm <- rf_sm[,-8]
  
  #TEST
  test <- rf_sm[c(26209:61344),]
    rf_test_hourly <- test %>%
    mutate(DateTime.H = as.POSIXct(strptime(test$dateTime,format="%Y-%m-%d %H"))) %>%
    group_by(DateTime.H) %>%
    summarise(RF_mm=sum(Rainfall_Tot))
  names(rf_test_hourly)[1] <- "datetime"
  rf_test_hourly <- as.data.frame(rf_test_hourly)
  
  #Event 1
  event1 <- rf_sm[c(which(rf_sm$dateTime == "2020-01-06 05:00:00"):which(rf_sm$dateTime == "2020-01-13 19:00:00")),]
  event1 <- event1[,c(1,6,7)]
  event1$ASI_rf_mm <- event1$ASI_mm + sum(event1$Rainfall_Tot)
  event1_hourly <- event1 %>%
    mutate(DateTime.H = as.POSIXct(strptime(event1$dateTime,format="%Y-%m-%d %H"))) %>%
    group_by(DateTime.H) %>%
    summarise(Rainfall_Tot=sum(Rainfall_Tot), ASI_mm = mean(ASI_mm), ASI_rf = mean(ASI_rf_mm))
  names(event1_hourly)[1] <- "datetime"
  event1_hourly <- as.data.frame(event1_hourly)
  
  #Event 2
  event2 <- rf_sm[c(which(rf_sm$dateTime == "2020-03-22 04:00:00"):which(rf_sm$dateTime == "2020-03-31 09:00:00")),]
  event2 <- event2[,c(1,6,7)]
  event2$ASI_rf_mm <- event2$ASI_mm + sum(event2$Rainfall_Tot)
  event2_hourly <- event2 %>%
    mutate(DateTime.H = as.POSIXct(strptime(event2$dateTime,format="%Y-%m-%d %H"))) %>%
    group_by(DateTime.H) %>%
    summarise(Rainfall_Tot=sum(Rainfall_Tot), ASI_mm = mean(ASI_mm), ASI_rf = mean(ASI_rf_mm))
  names(event2_hourly)[1] <- "datetime"
  event2_hourly <- as.data.frame(event2_hourly)
  
  #correlation test 
  test2 <- thre_events
  test2$corr <- test2$max_hourly_rf*0.4966155+test2$ASI_event_mm*0.4289846 #entire dataset
  test2$corr2 <- test2$max_hourly_rf*0.516102+test2$ASI_event_mm*0.1919122 #only threshold events
  
  #rainfall accumulation averaged per day
  test2$rf_acc_day <- (difftime(test2$End_time, test2$Start_time, units = "day"))
  test2$rf_acc_day <- as.numeric(test2$rf_acc_day)
  test2$rf_acc_day <- test2$Total_rf/test2$rf_acc_day
  test2 <- test2[,c(1:7,10,8,9)]
  
  #correlation with rainfall accumulated (averaged per day)
  cor(test2$rf_acc_day, test2$max_Q_event, method = "pearson", use = "complete.obs")
  test2$corr3 <- test2$max_hourly_rf*0.516102+test2$ASI_event_mm*0.1919122-test2$rf_acc_day*0.4415088
  
  #plot all soil moisture to understand weird event in February 2019
  plot(rf_sm$dateTime, rf_sm$SoilMoisture_Avg.1., type="l", col = "red", ylab = "Soil Moisture (m^3/m^3)")
  par(new=T)
  plot(rf_sm$dateTime, rf_sm$SoilMoisture_Avg.2., type="l", col = "green", ylab = "", yaxt = "n")
  par(new=T)
  plot(rf_sm$dateTime, rf_sm$SoilMoisture_Avg.3., type="l", col = "blue", ylab = "", yaxt = "n")
  
  #Threshold events without events happening before 2019-04-26
  test3 <- thre_events[-c(1,2),]
  test3$corr <- test3$max_hourly_rf*0.4966155+test3$ASI_event_mm*0.4289846 #entire dataset
  test3$corr2 <- test3$max_hourly_rf*0.7871428+test3$ASI_event_mm*0.5629094 #only remaining threshold events
  test3$rf_acc_day <- (difftime(test3$End_time, test3$Start_time, units = "day"))
  test3$rf_acc_day <- as.numeric(test3$rf_acc_day)
  test3$rf_acc_day <- test3$Total_rf/test3$rf_acc_day
  test3 <- test3[,c(1:7,10,8,9)]
  test3$corr3 <- test3$max_hourly_rf*0.7871428+test3$ASI_event_mm*0.5629094-test3$rf_acc_day*0.5476747
  
  #Plot events after 2019-04-26 for ASI+rf_accumulation
  ggplot(data = test3) + geom_point(mapping = aes(x = ASI_rf_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red",
      limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
      breaks = c(500,1000,1500,2000)) +
    scale_size(limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(500,1000,1500,2000)) +                   
    xlim(250,650)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index + Rainfall accumulation (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  #Plot events after 2019-04-26 for ASI+rf_accumulation
  ggplot(data = test3) + geom_point(mapping = aes(x = ASI_event_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red",
      limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
      breaks = c(500,1000,1500,2000)) +
    scale_size(limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(500,1000,1500,2000)) +                   
    xlim(250,400)+ ylim(5,26)+
    xlab("Antecendent Soil Moisture Index (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  #Linear regression
  fit2 <- lm(max_Q_event ~ max_hourly_rf + ASI_event_mm, data=test3)
  summary(fit2) # show results
  
  #Apply correlation factor form the linear regression
  test3$lm <- test3$max_hourly_rf*90.340+test3$ASI_event_mm*14.226
  #test3$lm <- test3$max_hourly_rf*96.003+test3$ASI_event_mm*16.563+test3$rf_acc_day*2.907
  #test3$lm <- test3$max_hourly_rf*100.889+test3$ASI_event_mm*13.817-test3$Total_rf*1.389
  
  #Log
  test4 <- test3[,-c(9:12)]
  test4$log_rf <- log(test4$max_hourly_rf)
  test4$log_ASI <- log(test4$ASI_event_mm)
  test4$log_ASI_rf <- log(test4$ASI_rf_mm)
  test4$log_Q <- log(test4$max_Q_event)
  test4$log_rf_day <- log(test4$rf_acc_day)
  
  #Linear regression with log
  fit3 <- lm(log_Q ~ log_rf + log_ASI, data=test4)
  summary(fit3) # show results
  
  #Apply correlation factor form the linear regression
  test4$lm <- test4$log_rf*1.2389+test4$log_ASI*3.8995
  test4$lm <- test4$log_rf*1.4262+test4$log_ASI*5.3301+test4$log_rf_day*0.3753
 
  #Plot log
  ggplot(data = test4) + geom_point(mapping = aes(x = log_ASI, y = log_rf,
    color = log_Q, size = log_Q)) + 
    scale_color_gradient(low="blue", high="red",
        limits = c(min(test4$log_Q), max(test4$log_Q)))+
        #breaks = c(500,1000,1500,2000)) +
    scale_size(limits = c(min(test4$log_Q), max(test4$log_Q)),
    breaks = c(500,1000,1500,2000)) +                   
    xlim(5.75,6)+ ylim(0,10)+
    xlab("Antecendent Soil Moisture Index (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  
  #Try different combinations for linear regression with ASI 0-30cm
  fit_test <- lm(log_Q ~ log_rf + log_ASI_rf + log_rf_day, data=thre_events_30)
  summary(fit_test) # show results
  
  test_ASI30 <- thre_events_30
  test_ASI30$lm_test <- test_ASI30$log_rf*1.0759+test_ASI30$log_ASI_rf*0.1114-test_ASI30$log_rf_day*0.2582
  

#############################################
#                                           #
#  Rainfall/discharge and soil moisture     #
#                of                         #
#           HN164 on Maui                   #
#                                           #
#############################################



#Load library
library(dataRetrieval)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tseries)

##Get rainfall/moisture and discharge data for the Maui location and organize the data------------------------------------

  #Get rainfall and soil moisture and rename, change format of the datetime, -9999 as NA
  rf_sm <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/weather(soil)_data/Mike_SM_RF/HN_164.csv", header = T, sep = ",")
  names(rf_sm)[1] <- "dateTime"
  rf_sm$dateTime <- as.POSIXct(rf_sm$dateTime, format = "%Y-%m-%d %H:%M")
  rf_sm$SM_1[rf_sm$SM_1 == -9999] <- NA
  rf_sm$SM_1[rf_sm$SM_1 == -6999.000] <- NA
  rf_sm$RF[rf_sm$RF == -9999] <- NA
  rf_sm$RF[rf_sm$RF == -6999.000] <- NA
  
  #Get the discharge data from the USGS website
  siteNumbers <- "16501200"
  parameter_cd <- "00060"
  startDate <- "2000-05-12"
  endDate <- "2019-09-24"
  Q_data <- readNWISuv(siteNumbers = siteNumbers, parameterCd = parameter_cd,
                     startDate = startDate, endDate = endDate)

  #Get right timezone for the discharge data
  Q_data$dateTime <- as.POSIXct(Q_data$dateTime, origin = "1960-01-01 00:00:00", tz = "GMT")
  Q_data$dateTime <- format(Q_data$dateTime, tz = "HST")
  Q_data$dateTime <- as.POSIXct(Q_data$dateTime) 

  #Keep columns we need for discharge data
  Q_data <- Q_data[,c(2:4)]
  names(Q_data)[3] <- "Q_cfs"
  
  #Create water year column in the rf_sm dataframe
  rf_sm$Water_Year <- ifelse(month(rf_sm$dateTime) < 10, rf_sm$dateTime, rf_sm$dateTime + years(1))
  rf_sm$Water_Year <- as.POSIXct(rf_sm$Water_Year, origin="1970-01-01", tz = "HST")
  rf_sm$Water_Year <- substr(rf_sm$Water_Year,1,4)
  
  #Create water year column in the discharge dataframe
  Q_data$Water_Year <- ifelse(month(Q_data$dateTime) < 10, Q_data$dateTime, Q_data$dateTime + years(1))
  Q_data$Water_Year <- as.POSIXct(Q_data$Water_Year, origin="1970-01-01", tz = "HST")
  Q_data$Water_Year <- substr(Q_data$Water_Year,1,4)
  
  #Identify with plot which period of time to focus 
  par(mar = c(5,5,5,5))
  plot(rf_sm$dateTime, rf_sm$RF, type = "l", col = "blue", ylab = "Rainfall (mm)",
       xlab = "dateTime")
  par(new = T)
  plot(rf_sm$dateTime, rf_sm$SM_1, type = "l", ylab = "", yaxt = "n", xlab = "", xaxt = "n")
  axis(4, ylim = c(0,max(rf_sm$SM_1, na.rm = T)))
  mtext("Soil Moisture (m^3/m^3)", side = 4, line = 3)
  
  #Look at NA in soil moisture and rainfall after 2005-03-28 9am
  rf_sm_no_NA <- rf_sm[c(42737:101639),] #From water year 2006 to 2011 no NA
  
  #Get common time period (WY 2006 to 2011) between soil moisture/rainfall and discharge
  discharge <- Q_data[c(103860:335566),]
  rf_sm_no_NA <- rf_sm_no_NA[c(4480:57064),]
  
  #Aggregate discharge hourly
  discharge_hourly <- discharge %>%
    mutate(DateTime.H = as.POSIXct(strptime(discharge$dateTime,format="%Y-%m-%d %H"))) %>%
    group_by(DateTime.H) %>%
    summarise(Q_cfs=sum(Q_cfs))
  names(discharge_hourly)[1] <- "datetime"
  discharge_hourly <- as.data.frame(discharge_hourly)
  
  #Perfect time sequence for the discharge
  seq_hr <- seq(rf_sm_no_NA$dateTime[1], rf_sm_no_NA$dateTime[length(rf_sm_no_NA$dateTime)], by = 60*60)
  seq_hr_df <- data.frame(datetime = seq_hr)
  discharge_hourly <- merge(seq_hr_df,discharge_hourly, by = "datetime", all.x = T ) 
  

##END OF get rainfall/moisture and discharge data for the Maui location and organize the data---------------------------------

  

##Define rainfall events and add maximum discharge for each event-----------------------------------------  
  
  #Find 5 hours of consecutive 0 in the rainfall column
  dry_weather <- with(rle(rf_sm_no_NA$RF), {
    ok <- values == 0 & lengths > 4 #here we set up more than 4 rows as 5 rows equal 5 hours
    ends <- cumsum(lengths)
    starts <- ends - lengths + 1
    data.frame(starts, ends)[ok, ]
  }) #Give us row index when a dry event (at least 5 hours) starts and when it ends.
  
  #Get sum of rainfall for the storms identified 
  row_dry_weather <- nrow(dry_weather) #will be used as index for the loop
  
  #empty vectors for the loop
  v_start_time <- c()
  v_end_time <- c()
  v_max_rf <- c()
  v_tot_rf <- c()
  
  for (i in 1:(row_dry_weather-1)){
    
    #Get start time of the storm
    start_time <- rf_sm_no_NA$dateTime[dry_weather[i,2]+1]
    v_start_time <- append(v_start_time, start_time)
    
    #Get end time 
    end_time <- rf_sm_no_NA$dateTime[dry_weather[(i+1),1]]
    v_end_time <- append(v_end_time, end_time)
    
    #Get sum of rainfall per event
    tot_rf <- sum(rf_sm_no_NA[c((dry_weather[i,2]+1):(dry_weather[(i+1),1]-1)),6], na.rm = T)
    v_tot_rf <- append(v_tot_rf, tot_rf)
    
    #Get max hourly rainfall per event 
    max_rf <- max(rf_sm_no_NA[c((dry_weather[i,2]+1):(dry_weather[(i+1),1]-1)),6], na.rm = T)
    v_max_rf <- append(v_max_rf, max_rf)
    
  }
  
  #Create dataframe with start/end time, sum of rainfall, and sum of flow for each storm
  df_rf_events <- data.frame(Start_time = v_start_time, End_time = v_end_time, Total_rf = v_tot_rf,
                             max_hourly_rf = v_max_rf)
  
  #Add first event at the beginning of the dataset
  missing_event <- data.frame(Start_time = "2005-10-01 00:00:00", End_time = "2005-10-02 12:00:00",
                              Total_rf = sum(rf_sm_no_NA$RF[1:37], na.rm = T), 
                              max_hourly_rf = max(rf_sm_no_NA$RF[1:37], na.rm = T))
  missing_event$Start_time <- as.POSIXct(missing_event$Start_time)
  missing_event$End_time <- as.POSIXct(missing_event$End_time)
  df_rf_events <- rbind(missing_event, df_rf_events)
  
  #Delete events with less than 10mm of accumulated rainfall and max hourly rainfall less 5mm/h
  df_rf_events <- df_rf_events[df_rf_events$Total_rf >= 10,]
  df_rf_events <- df_rf_events[df_rf_events$max_hourly >= 5,]
  
  #Add max discharge for each event in the dataframe
  v_max_Q <- c()
  for(i in 1:length(df_rf_events$Start_time)){
    max_Q <- max(discharge_hourly$Q_cfs[c(which(discharge_hourly$datetime == df_rf_events$Start_time[i]):
                                    which(discharge_hourly$datetime == df_rf_events$End_time[i]))], na.rm=T)
    v_max_Q <- append(v_max_Q, max_Q)
  }
  df_max_Q <- data.frame(max_Q_event = v_max_Q)
  
  #Set up max discharge as Na for the events where we have missing data (some are -inf as no discharge at all)
  df_max_Q$max_Q_event[c(5,7,8,20)] <- NA
  
  #Merge max discharge per event to the event dataframe
  df_rf_events <- cbind(df_rf_events, df_max_Q)

##END OF define rainfall events and add maximum discharge for each event-----------------------------------------------   



##Get soil moisture data in mm for each start dates of the events------------------------------------------------------
  
  #Based on Saffarpour, McGuire, and Haga method
  ASI <- rf_sm_no_NA$SM_1*300
  ASI <- data.frame(ASI_mm=ASI)
  rf_sm_no_NA <- cbind(rf_sm_no_NA, ASI)
  
  #Get ASI for each beginning of events
  v_ASI <- c()
  for(i in 1:length(df_rf_events$Start_time)){
    ASI_index <- rf_sm_no_NA$ASI_mm[which(rf_sm_no_NA$dateTime == df_rf_events$Start_time[i])]
    v_ASI <- append(v_ASI, ASI_index)
  }
  ASI_event <- data.frame(ASI_event_mm = v_ASI)
  df_rf_events <- cbind(df_rf_events, ASI_event)
  
  #Calculate ASI+rainfall
  df_rf_events$ASI_rf_mm <- df_rf_events$Total_rf+df_rf_events$ASI_event_mm
  
  #Remove events with missing discharge data
  df_rf_gd_events <- df_rf_events[-c(5,7,8,20),]

##END OF get soil moisture data in mm for each start dates of the events-----------------------------------------
  


##Plot graph with ASI+rainfall, rainfall intensity and max event discharge------------------------------------------
  
  #All discharge with cross for events at the 75 percentile discharge
  ggplot(data = df_rf_gd_events) + 
    geom_point(mapping = aes(x = ASI_rf_mm, y = max_hourly_rf,color = max_Q_event, size = max_Q_event)) +
    geom_point(data = df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantile(df_rf_gd_events$max_Q_event, 0.75),], 
               aes(x = ASI_rf_mm, y = max_hourly_rf), pch=4, fill=NA, colour="black", stroke=1)+
    scale_color_gradient(low="blue", high="red") +
    xlim(200,3000)+ ylim(0,80)+
    xlab("Antecendent Soil Moisture Index + Rainfall accumulation (mm)")+
    ylab("Maximum hourly rainfall intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  #90th, 95th, 99th percentile discharge
  quantiles <- quantile(df_rf_gd_events$max_Q_event, c(0.90, 0.95, 0.99))
  df_rf_gd_events90 <- df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantiles[1],]
  df_rf_gd_events95 <- df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantiles[2],]
  df_rf_gd_events99 <- df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantiles[3],]
  #Quantile values are very high. Only look at 90th percentile.
  
  #90th percentile plot
  ggplot(data = df_rf_gd_events90) + geom_point(mapping = aes(x = ASI_rf_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red", 
    limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(0,20000,40000,60000,80000)) +
    scale_size(limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(0,20000,40000,60000,80000)) +
    xlim(200,3000)+ ylim(0,80)+
    xlab("Antecendent Soil Moisture Index + Rainfall accumulation (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "90th percentile maximum discharge per event (cfs)")+
    labs(size = "90th percentile maximum discharge per event (cfs)")
  
  #ASI only
  ggplot(data = df_rf_gd_events) + geom_point(mapping = aes(x = ASI_event_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    geom_point(data = df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantile(df_rf_gd_events$max_Q_event, 0.75),], 
               aes(x = ASI_event_mm, y = max_hourly_rf), pch=4, fill=NA, colour="black", stroke=1)+
    scale_color_gradient(low="blue", high="red") +
    xlim(200,250)+ ylim(0,80)+
    xlab("Antecendent Soil Moisture Index (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  #Rainfall accumulation only
  ggplot(data = df_rf_gd_events) + geom_point(mapping = aes(x = Total_rf, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    geom_point(data = df_rf_gd_events[df_rf_gd_events$max_Q_event >= quantile(df_rf_gd_events$max_Q_event, 0.75),], 
               aes(x = Total_rf, y = max_hourly_rf), pch=4, fill=NA, colour="black", stroke=1)+
    scale_color_gradient(low="blue", high="red") +
    xlim(0,2510)+ ylim(0,80)+
    xlab("Total rainfall (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
##END OF plot graph with ASI+rainfall, rainfall intensity and max event discharge-------------------------------
  
  
  
##Linear regression for the high discharge events--------------------------------------------------  
  
  #Create dataframe with only events exceeding the thresholds defined from the previous graphs
  thre_events <- df_rf_gd_events[df_rf_gd_events$ASI_rf_mm >= 380 & df_rf_gd_events$max_hourly_rf >= 25,]
  thre_events <- df_rf_gd_events[df_rf_gd_events$max_hourly_rf >= 20,]
  
  #Plot the events above threshold values
  ggplot(data = thre_events) + geom_point(mapping = aes(x = ASI_rf_mm, y = max_hourly_rf,
    color = max_Q_event, size = max_Q_event)) + 
    scale_color_gradient(low="blue", high="red",
    limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(0,20000,40000,60000,80000)) +
    scale_size(limits = c(min(df_rf_gd_events$max_Q_event), max(df_rf_gd_events$max_Q_event)),
    breaks = c(0,20000,40000,60000,80000)) +                   
    xlim(200,3000)+ ylim(0,80)+
    xlab("Antecendent Soil Moisture Index + Rainfall accumulation (mm)")+
    ylab("Maximum hourly intensity (mm/hr)")+
    labs(color = "Maximum Discharge per event (cfs)")+
    labs(size = "Maximum Discharge per event (cfs)")
  
  #Calculate accumulated rainfall averaged by day (not depending anymore on the length of the event)
  thre_events$rf_acc_day <- (difftime(thre_events$End_time, thre_events$Start_time, units = "day"))
  thre_events$rf_acc_day <- as.numeric(thre_events$rf_acc_day)
  thre_events$rf_acc_day <- thre_events$Total_rf/thre_events$rf_acc_day
  
  #Linear regression 
  fit1 <- lm(max_Q_event ~ max_hourly_rf + ASI_event_mm, data=thre_events)
  summary(fit1) # show results
  fit2 <- lm(max_Q_event ~ max_hourly_rf + ASI_event_mm + rf_acc_day, data=thre_events)
  summary(fit2) # show results
  fit3 <- lm(max_Q_event ~ max_hourly_rf + ASI_rf_mm, data=thre_events)
  summary(fit3) # show results
  fit4 <- lm(max_Q_event ~ max_hourly_rf + ASI_rf_mm + Total_rf, data=thre_events)
  summary(fit4) # show results
  
  #Use correlation factor
  thre_events$lm1 <- thre_events$max_hourly_rf*592.436+thre_events$ASI_rf_mm*8.131
  thre_events$lm2 <- thre_events$max_hourly_rf*1175.26-thre_events$ASI_event_mm*359.48-thre_events$rf_acc_day*113.27
  
##END OF linear regression for the high discharge events--------------------------------------------------------------
  
  
  
##Linear regression with log values------------------------------------------------------------------------------------
  
  #Log values for thre_events dataframe
  thre_events$log_rf <- log(thre_events$max_hourly_rf)
  thre_events$log_ASI <- log(thre_events$ASI_event_mm)
  thre_events$log_ASI_rf <- log(thre_events$ASI_rf_mm)
  thre_events$log_Q <- log(thre_events$max_Q_event)
  thre_events$log_rf_day <- log(thre_events$rf_acc_day)
  
  #Linear regression for log values 
  log_fit1 <- lm(log_Q ~ log_rf + log_ASI, data=thre_events)
  summary(log_fit1) # show results
  log_fit2 <- lm(log_Q ~ log_rf + log_ASI + log_rf_day, data=thre_events)
  summary(log_fit2) # show results
  log_fit3 <- lm(log_Q ~ log_rf + log_ASI_rf, data=thre_events)
  summary(log_fit3) # show results
  log_fit4 <- lm(log_Q ~ log_rf + log_ASI_rf + log_rf_day, data=thre_events)
  summary(log_fit4) # show results
  
  #Use correlation factor
  thre_events$log_lm1 <- thre_events$log_rf*1.3713+thre_events$log_ASI*4.1484
  thre_events$log_lm2 <- thre_events$log_rf*1.5551-thre_events$log_ASI*1.6761-thre_events$log_rf_day*0.5277
  
  #Test on Linear regression
  fit_test <- lm(log_Q ~ max_hourly_rf + log_ASI_rf + rf_acc_day, data=thre_events)
  summary(fit_test)
  thre_events$test_lm <- thre_events$max_hourly_rf*483.218+thre_events$ASI_event_mm*71.165+thre_events$Total_rf*8.180 
  
##END OF linear regression with log values-------------------------------------------------------------------------------------
 
  
  
##Compare peakflow event estimates based on max hourly rainfall only and max hourly rainfall and ASI--------------------------- 

  #Identify dates max hourly rainfall per water year
  rf_sm_2006 <- rf_sm_no_NA[c(which(rf_sm_no_NA$dateTime == "2005-10-01 00:00:00"):which(rf_sm_no_NA$dateTime == "2006-09-30 23:00:00")),]
  rf_sm_2007 <- rf_sm_no_NA[c(which(rf_sm_no_NA$dateTime == "2006-10-01 00:00:00"):which(rf_sm_no_NA$dateTime == "2007-09-30 23:00:00")),]
  rf_sm_2008 <- rf_sm_no_NA[c(which(rf_sm_no_NA$dateTime == "2007-10-01 00:00:00"):which(rf_sm_no_NA$dateTime == "2008-09-30 23:00:00")),]
  rf_sm_2009 <- rf_sm_no_NA[c(which(rf_sm_no_NA$dateTime == "2008-10-01 00:00:00"):which(rf_sm_no_NA$dateTime == "2009-09-30 23:00:00")),]
  rf_sm_2010 <- rf_sm_no_NA[c(which(rf_sm_no_NA$dateTime == "2009-10-01 00:00:00"):which(rf_sm_no_NA$dateTime == "2010-09-30 23:00:00")),]
  rf_sm_2011 <- rf_sm_no_NA[c(which(rf_sm_no_NA$dateTime == "2010-10-01 00:00:00"):which(rf_sm_no_NA$dateTime == "2011-09-30 23:00:00")),]
  
  v_water_year <- c(2006:2011)
  max_2006 <- rf_sm_2006$dateTime[which(rf_sm_2006$RF==max(rf_sm_2006$RF,na.rm = T))]
  max_2007 <- rf_sm_2007$dateTime[which(rf_sm_2007$RF==max(rf_sm_2007$RF,na.rm = T))]
  max_2008 <- rf_sm_2008$dateTime[which(rf_sm_2008$RF==max(rf_sm_2008$RF,na.rm = T))]
  max_2009 <- rf_sm_2009$dateTime[which(rf_sm_2009$RF==max(rf_sm_2009$RF,na.rm = T))]
  max_2010 <- rf_sm_2010$dateTime[which(rf_sm_2010$RF==max(rf_sm_2010$RF,na.rm = T))]
  max_2011 <- rf_sm_2011$dateTime[which(rf_sm_2011$RF==max(rf_sm_2011$RF,na.rm = T))]
  v_max_rf <- c(max_2006, max_2007, max_2008, max_2009, max_2010, max_2011)
  
  #Identify dates with rainfall and ASI combined
  rf_ASI_2006 <- rf_sm_no_NA$dateTime[rf_sm_no_NA$RF==49.78]
  rf_ASI_2007 <- rf_sm_no_NA$dateTime[rf_sm_no_NA$RF==41.91]
  rf_ASI_2008 <- rf_sm_no_NA$dateTime[rf_sm_no_NA$RF==62.99]
  rf_ASI_2009 <- rf_sm_no_NA$dateTime[rf_sm_no_NA$RF==72.40]
  rf_ASI_2010 <- rf_sm_no_NA$dateTime[rf_sm_no_NA$RF==38.35]
  rf_ASI_2010 <- rf_ASI_2010[2]#only rainfall associated to the correct water year
  rf_ASI_2011 <- rf_sm_no_NA$dateTime[rf_sm_no_NA$RF==35.56]
  v_rf_ASI <- c(rf_ASI_2006,rf_ASI_2007,rf_ASI_2008,rf_ASI_2009,rf_ASI_2010,rf_ASI_2011)
  
  #Create dataframe with max hourly rf and rf+ASI dates for estimated peakflow
  final_df <- data.frame(Water_Year = v_water_year, max_rf = v_max_rf, rf_ASI = v_rf_ASI)
  
  #Download peakflow data
  siteNumber <- "16501200"
  peakdata <- readNWISpeak(siteNumber)
  peakdata <- peakdata[c(14:19), c(2:3,5)]
  
  #Merge peakflow data and estimated peakflow dates
  final_df <- cbind(peakdata, final_df)
  final_df <- final_df[,c(1,4,2,3,5,6)]
  
  #Calculate differences in days between estimates and actual peakflow dates
  final_df$diff_rf <- difftime(final_df$peak_dt, final_df$max_rf, units = "days")
  final_df$diff_rf_ASI <- difftime(final_df$peak_dt, final_df$rf_ASI, units = "days")
  final_df$diff_rf <- round(final_df$diff_rf, 2)
  final_df$diff_rf_ASI <- round(final_df$diff_rf_ASI, 2)
  
  write.csv(final_df, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/weather(soil)_data/HN164_16501200.csv", row.names = F)
  
##END OF compare peakflow event estimates based on max hourly rainfall only and max hourly rainfall and ASI---------------------------  

  
  
##Understand/Visualize relationships between soil moisture, rainfall, and discharge for the threshold events------------------------------------------  
  
  #Load package required
  library(scatterplot3d)
  library(rgl)
  library(magick)
  
  #3D plot
  plot3d(thre_events$max_hourly_rf, thre_events$ASI_rf_mm, thre_events$max_Q_event, xlab = "",
         ylab = "", zlab = "", col = "red", axes =F)
  axes3d(edges = c("x--", "y+-", "z--"))
  mtext3d("Max Hourly Rainfall (mm)", edge="x--", line=2)
  mtext3d("Antecedent Soil Moisture + Rainfall accumulation (mm)", edge="y+-", line=3)
  mtext3d("Max event Discharge (cfs)", edge="z--", line=3)
  box3d()
  
##END OF understand/Visualize relationships between soil moisture, rainfall, and discharge for the threshold events------------------------------------------  

  
  
##Try linear regression for all the events-------------------------------------------------------------------------  
  
  #Add column of accumulated rainfall per day
  df_rf_gd_events$rf_acc_day <- (difftime(df_rf_gd_events$End_time, df_rf_gd_events$Start_time, units = "day"))
  df_rf_gd_events$rf_acc_day <- as.numeric(df_rf_gd_events$rf_acc_day)
  df_rf_gd_events$rf_acc_day <- df_rf_gd_events$Total_rf/df_rf_gd_events$rf_acc_day
  
  #Calculate log values for good events
  df_rf_gd_events$log_rf <- log(df_rf_gd_events$max_hourly_rf)
  df_rf_gd_events$log_ASI <- log(df_rf_gd_events$ASI_event_mm)
  df_rf_gd_events$log_ASI_rf <- log(df_rf_gd_events$ASI_rf_mm)
  df_rf_gd_events$log_Q <- log(df_rf_gd_events$max_Q_event)
  df_rf_gd_events$log_Q[c(197,198,200)] <- 0
  df_rf_gd_events$log_rf_day <- log(df_rf_gd_events$rf_acc_day)
  
  #Good events --> 273 events in total
  lm_events <- lm(max_Q_event ~ max_hourly_rf + ASI_event_mm + Total_rf, data=df_rf_gd_events)
  summary(lm_events) # show results
  log_events <- lm(log_Q ~ max_hourly_rf + ASI_rf_mm + Total_rf, data=df_rf_gd_events)
  summary(log_events) # show results
  
##END OF try linear regression for all the events-------------------------------------------------------------------------  
 
  
  
##Cross-correlation to identify potential lag between ASI and discharge-----------------------------------------  
  
  #Cross-correlation
  ccfvalues <- ccf(rf_sm_no_NA$ASI_mm,discharge_hourly$Q_cfs, 10, na.action = na.pass)
  ccfvalues <- ccf(rf_sm_no_NA$RF,rf_sm_no_NA$ASI_mm, 10, na.action = na.pass)
  ccfvalues <- ccf(rf_sm_no_NA$RF,discharge_hourly$Q_cfs, 10, na.action = na.pass)
  
  
  
  
  
  
  
  
  
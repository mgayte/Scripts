#############################################
#                                           #
#             Build graph for               #
#                  for                      #
#         max hourly and peakflow           #
#                                           #
#############################################

###Load library
library(dataRetrieval)
library(ggplot2)

###USGS station 213215157552800 and peakflow station 16296500-------------------------------------------
##Load and import all the required data
  
  #Import info file
  info <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/new_max_avg_sum_USGS_uv213215157552800_16296500.csv", header = T)
  info$peak_dt <- as.character(info$peak_dt)#need character format for later plots
  info
  
  #Import rainfall file
  USGS <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/USGS_uv213215157552800.csv", header = T)
  USGS$DateTime <- as.POSIXct(USGS$DateTime, origin = "%Y-%m-%d %H:%M")
  
  #Download discharge data and organize
  siteNumbers <- "16296500"
  parameterCd <- "00060"
  startDate <- substr(USGS$DateTime[1],1,10)
  endDate <- substr(USGS$DateTime[length(USGS$DateTime)],1,10)
  Qtable <- readNWISuv(siteNumbers, parameterCd, startDate = startDate, endDate = endDate)
  Qtable$dateTime <- as.POSIXct(Qtable$dateTime, origin = "%Y-%m-%d %H:%M", tz = "GMT") #datetime format
  Qtable$dateTime <- as.POSIXct(format(Qtable$dateTime, tz = "HST")) #Hawaii time zone
  Qtable <- Qtable[,c(3,4)] #keep columns we are interested in
  names(Qtable)[c(1:2)] <- c("DateTime", "Q_cfs") #rename columns
  
##Graph discharge/peakflow/rainfall together for 3 good matches and 3 bad matches
  
  #3 good matches
  #1st good match (6/4/2011)
  USGS_gd1 <- USGS[c(12768:12864),]#here we have only NA for a couple of days after the max
  USGS_gd1 <- 
  Qtable_gd1 <- Qtable[c(51056:51343),]#miss data from the 4th to the 18th
  peakdate1 <- data.frame(peak_dt = info[2,11], peak_vl = info[2,12])
  peakdate1$peak_dt <- as.POSIXct(peakdate1$peak_dt,format= "%m/%d/%Y")+60*60*20#add hours and min of peakflow
  peakflow1 <- data.frame(DateTime = USGS_gd1$DateTime)
  peakflow1$peakflow <- 0
  peakflow1$peakflow[93] <- peakdate1$peak_vl
  
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.8))
  plot(Qtable_gd1$DateTime, Qtable_gd1$Q_cfs,type="l", col="black",
       ylab = "Streamflow (cfs)", xlab = "DateTime", ylim = c(0,12000))
  par(new=T)
  par(fig=c(0,0.8,0.35,0.8))
  barplot(USGS_gd1$RF_mm, type = "l", col ="blue", lty =12,
          ylim = rev(c(0, max(USGS_gd1$RF_mm, na.rm = T))), ylab = "", xlab = "", xaxt = "n", yaxt = "n" )
  axis(side = 4, ylim = c(0, max(USGS_gd1$RF_mm, na.rm = T)))
  par(new=T)
  par(fig=c(0,0.8,0,0.8))
  plot(peakflow1$DateTime, peakflow1$peakflow,type = "l", lty = 12, col = "red",
       xaxt= "n", yaxt = "n", xlab = "", ylab = "")
  mtext("Rainfall (mm)", side = 4, line = 3)
  title("Good match 6-4-2011")
  
  #2nd good match (9/11/2015)
  USGS_gd2 <- USGS[c(50208:50376),]
  Qtable_gd2 <- Qtable[c(157277:157950),]
  peakdate2 <- data.frame(peak_dt = info[6,11], peak_vl = info[6,12])
  peakdate2$peak_dt <- as.POSIXct(peakdate2$peak_dt,format= "%m/%d/%Y")+60*60*18+60*50#add hours and min of peakflow
  peakflow2 <- data.frame(DateTime = USGS_gd2$DateTime)
  peakflow2$peakflow <- 0
  peakflow2$peakflow[91] <- peakdate2$peak_vl
  
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.8))
  plot(Qtable_gd2$DateTime, Qtable_gd2$Q_cfs,type="l", col="black",
       ylab = "Streamflow (cfs)", xlab = "DateTime", ylim = c(0,5000))
  par(new=T)
  par(fig=c(0,0.8,0.35,0.8))
  barplot(USGS_gd2$RF_mm, type = "l", col ="blue", lty =12,
          ylim = rev(c(0, max(USGS_gd2$RF_mm, na.rm = T))), ylab = "", xlab = "", xaxt = "n", yaxt = "n" )
  axis(side = 4, ylim = c(0, max(USGS_gd2$RF_mm, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 3)
  title("Good match 9-11-2015")
  
  #3rd good match (12/28/2018)
  USGS_gd3 <- USGS[c(79104:79272),]
  Qtable_gd3 <- Qtable[c(272862:273535),]
  peakdate3 <- data.frame(peak_dt = info[10,11], peak_vl = info[10,12])
  peakdate3$peak_dt <- as.POSIXct(peakdate3$peak_dt,format= "%m/%d/%Y")+60*60*11+60*23#add hours and min of peakflow
  peakflow3 <- data.frame(DateTime = USGS_gd3$DateTime)
  peakflow3$peakflow <- 0
  peakflow3$peakflow[84] <- peakdate3$peak_vl
  
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.8))
  plot(Qtable_gd3$DateTime, Qtable_gd3$Q_cfs,type="l", col="black",
       ylab = "Streamflow (cfs)", xlab = "DateTime", ylim = c(0,5000))
  par(new=T)
  par(fig=c(0,0.8,0.35,0.8))
  barplot(USGS_gd3$RF_mm, type = "l", col ="blue", lty =12,
          ylim = rev(c(0, max(USGS_gd3$RF_mm, na.rm = T))), ylab = "", xlab = "", xaxt = "n", yaxt = "n" )
  axis(side = 4, ylim = c(0, max(USGS_gd3$RF_mm, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 3)
  title("Good match 12-28-2018")
  
  #3bad matches
  #1st bad match (rainfall 11/23/2015 and peakflow 7/24/2016)
  USGS_bd_rf1 <- USGS[c(51960:52128),]
  Qtable_bd_rf1 <- Qtable[c(164286:164959),]
  
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.8))
  plot(Qtable_bd_rf1$DateTime, Qtable_bd_rf1$Q_cfs,type="l", col="black",
       ylab = "Streamflow (cfs)", xlab = "DateTime", ylim = c(0,2000))
  par(new=T)
  par(fig=c(0,0.8,0.35,0.8))
  barplot(USGS_bd_rf1$RF_mm, type = "l", col ="blue", lty =12,
          ylim = rev(c(0, max(USGS_bd_rf1$RF_mm, na.rm = T))), ylab = "", xlab = "", xaxt = "n", yaxt = "n" )
  axis(side = 4, ylim = c(0, max(USGS_bd_rf1$RF_mm, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 3)
  title("Bad match rainfall 11-23-2015")
  
  USGS_bd_pk1 <- USGS[c(57816:57984),]
  Qtable_bd_pk1 <- Qtable[c(187712:188385),]
  
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.8))
  plot(Qtable_bd_pk1$DateTime, Qtable_bd_pk1$Q_cfs,type="l", col="black",
       ylab = "Streamflow (cfs)", xlab = "DateTime", ylim = c(0,6000))
  par(new=T)
  par(fig=c(0,0.8,0.35,0.8))
  barplot(USGS_bd_pk1$RF_mm, type = "l", col ="blue", lty =12,
          ylim = rev(c(0, max(USGS_bd_pk1$RF_mm, na.rm = T))), ylab = "", xlab = "", xaxt = "n", yaxt = "n" )
  axis(side = 4, ylim = c(0, max(USGS_bd_pk1$RF_mm, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 3)
  title("Bad match peakflow 7-24-2016")
  
  #2nd bad match (rainfall 5-8-2017 and peakflow 2-11-2017)
  USGS_bd_rf2 <- USGS[c(64728:64896),]
  Qtable_bd_rf2 <- Qtable[c(215361:216033),]
  
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.8))
  plot(Qtable_bd_rf2$DateTime, Qtable_bd_rf2$Q_cfs,type="l", col="black",
       ylab = "Streamflow (cfs)", xlab = "DateTime", ylim = c(0,2000))
  par(new=T)
  par(fig=c(0,0.8,0.35,0.8))
  barplot(USGS_bd_rf2$RF_mm, type = "l", col ="blue", lty =12,
          ylim = rev(c(0, max(USGS_bd_rf2$RF_mm, na.rm = T))), ylab = "", xlab = "", xaxt = "n", yaxt = "n" )
  axis(side = 4, ylim = c(0, max(USGS_bd_rf2$RF_mm, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 3)
  title("Bad match rainfall 5-8-2017")
  
  USGS_bd_pk2 <- USGS[c(62664:62832),]
  Qtable_bd_pk2 <- Qtable[c(207104:207777),]
  
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.8))
  plot(Qtable_bd_pk2$DateTime, Qtable_bd_pk2$Q_cfs,type="l", col="black",
       ylab = "Streamflow (cfs)", xlab = "DateTime", ylim = c(0,2000))
  par(new=T)
  par(fig=c(0,0.8,0.35,0.8))
  barplot(USGS_bd_pk2$RF_mm, type = "l", col ="blue", lty =12,
          ylim = rev(c(0, max(USGS_bd_pk2$RF_mm, na.rm = T))), ylab = "", xlab = "", xaxt = "n", yaxt = "n" )
  axis(side = 4, ylim = c(0, max(USGS_bd_pk2$RF_mm, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 3)
  title("Bad match peakflow 2-11-2017")
  
  #3rd bad match (rainfall 3-23-2018 and peakflow 2-25-2018)
  USGS_bd_rf3 <- USGS[c(72384:72552),]
  Qtable_bd_rf3 <- Qtable[c(245982:246655),]
  
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.8))
  plot(Qtable_bd_rf3$DateTime, Qtable_bd_rf3$Q_cfs,type="l", col="black",
       ylab = "Streamflow (cfs)", xlab = "DateTime", ylim = c(0,4000))
  par(new=T)
  par(fig=c(0,0.8,0.35,0.8))
  barplot(USGS_bd_rf3$RF_mm, type = "l", col ="blue", lty =12,
          ylim = rev(c(0, max(USGS_bd_rf3$RF_mm, na.rm = T))), ylab = "", xlab = "", xaxt = "n", yaxt = "n" )
  axis(side = 4, ylim = c(0, max(USGS_bd_rf3$RF_mm, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 3)
  title("Bad match rainfall 3-23-2018")
  
  USGS_bd_pk3 <- USGS[c(71760:71928),]
  Qtable_bd_pk3 <- Qtable[c(243487:244160),]
  
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.8))
  plot(Qtable_bd_pk3$DateTime, Qtable_bd_pk3$Q_cfs,type="l", col="black",
       ylab = "Streamflow (cfs)", xlab = "DateTime", ylim = c(0,6000))
  par(new=T)
  par(fig=c(0,0.8,0.35,0.8))
  barplot(USGS_bd_pk3$RF_mm, type = "l", col ="blue", lty =12,
          ylim = rev(c(0, max(USGS_bd_pk3$RF_mm, na.rm = T))), ylab = "", xlab = "", xaxt = "n", yaxt = "n" )
  axis(side = 4, ylim = c(0, max(USGS_bd_pk3$RF_mm, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 3)
  title("Bad match peakflow 2-25-2018")
  
  #Test graph
  par(new=T)
  par(mar = c(3,8,1.5,0.1))
  par(fig=c(0,0.8,0,0.8))
  plot(Qtable_gd2$DateTime, Qtable_gd2$Q_cfs,type="l", col="black",
       ylab = "Streamflow (cfs)", xlab = "DateTime", ylim = c(0,5000))
  par(new=T)
  par(fig=c(0,0.8,0.35,0.8))
  barplot(USGS_gd2$RF_mm, type = "l", col ="blue", lty =12,
          ylim = rev(c(0, max(USGS_gd2$RF_mm, na.rm = T))), ylab = "", xlab = "", xaxt = "n", yaxt = "n" )
  axis(side = 4, ylim = c(0, max(USGS_gd2$RF_mm, na.rm = T)))
  mtext("Rainfall (mm)", side = 4, line = 3)
  title("Good match 9-11-2015")
 
###END USGS station 213215157552800 and peakflow station 16296500------------------------------------------- 
  
  
  
  
  
  
  
  
  







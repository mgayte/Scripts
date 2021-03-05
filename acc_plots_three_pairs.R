#############################################
#                                           #
#           Accumulation plots              #
#                  for                      #
#             the three pairs               #
#                                           #
#############################################



#Load library
library(dplyr)
library(naniar)
library(data.table)
library(zoo)
library(lubridate)
library(dataRetrieval)


##########################Kahana 213215157552800/16296500######################################

##Get rainfall accumulation 2 days prior to the peakflow for the three good and three bad events plotted--------------------  

  #Read USGS Kahana station file
  USGS <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/USGS_uv213215157552800.csv", header = T)

  #Get the peakflow dates for station 16296500
  peakdata <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/max_rainfall_table/new_check_updated_date_USGS_uv213215157552800_16296500.csv", header = T)
  
  #Get discharge data
  siteNumbers <- "16296500"
  parameterCd <- "00060"
  startDate <- substr(USGS$DateTime[1],1,10)
  endDate <- substr(USGS$DateTime[length(USGS$DateTime)],1,10)
  Q16296500 <- readNWISuv(siteNumbers, parameterCd, startDate = startDate, endDate = endDate)
  Q16296500$dateTime <- as.POSIXct(Q16296500$dateTime, origin = "%Y-%m-%d %H:%M", tz = "GMT") #datetime format
  Q16296500$dateTime <- as.POSIXct(format(Q16296500$dateTime, tz = "HST")) #Hawaii time zone
  Q16296500 <- Q16296500[,c(3,4)] #keep columns we are interested in
  names(Q16296500)[c(1:2)] <- c("DateTime", "Q_cfs") #rename columns
  
  #Keep only water years I am interested in
  peakdata <- peakdata[c(2,6:10),]
  
  #Calculate rainfall accumulation for each water year
  #2011
  USGS_gd_2011 <- USGS[c(12812:12860),]
  USGS_gd_2011$Rf_acc <- cumsum(USGS_gd_2011$RF_mm)
  #2015
  USGS_gd_2015 <- USGS[c(50250:50298),]
  USGS_gd_2015$Rf_acc <- cumsum(USGS_gd_2015$RF_mm)
  #2016
  USGS_pk_2016 <- USGS[c(57862:57910),]
  USGS_pk_2016$Rf_acc <- cumsum(USGS_pk_2016$RF_mm)
  USGS_rf_2016 <- USGS[c(51990:52038),]
  USGS_rf_2016$Rf_acc <- cumsum(USGS_rf_2016$RF_mm)
  USGS_rf_2016_2 <- USGS[c(51993:52041),]
  USGS_rf_2016_2$Rf_acc <-cumsum(USGS_rf_2016_2$RF_mm)
  #2017
  USGS_pk_2017 <- USGS[c(62701:62749),]
  USGS_pk_2017$Rf_acc <- cumsum(USGS_pk_2017$RF_mm)
  USGS_rf_2017 <- USGS[c(64756:64804),]
  USGS_rf_2017$Rf_acc <- cumsum(USGS_rf_2017$RF_mm)
  USGS_rf_2017_2 <- USGS[c(64757:64805),]
  USGS_rf_2017_2$Rf_acc <-cumsum(USGS_rf_2017_2$RF_mm)
  #2018
  USGS_pk_2018 <- USGS[c(71803:71851),]
  USGS_pk_2018$Rf_acc <- cumsum(USGS_pk_2018$RF_mm)
  USGS_rf_2018 <- USGS[c(72427:72475),]
  USGS_rf_2018$Rf_acc <- cumsum(USGS_rf_2018$RF_mm)
  USGS_rf_2018_2 <- USGS[c(72434:72482),]
  USGS_rf_2018_2$Rf_acc <-cumsum(USGS_rf_2018_2$RF_mm)
  #2019
  USGS_gd_2019 <- USGS[c(79139:79187),]
  USGS_gd_2019$Rf_acc <- cumsum(USGS_gd_2019$RF_mm)

##END OF get rainfall accumulation 2 days prior to the peakflow for the three good and three bad events plotted--------------------  

  
  
##Get the accumulated rainfall on a scale from 0 to 1 (divide each value by max value for each year)---------------
  #2011
  USGS_gd_2011$Rf_acc_std <- (USGS_gd_2011$Rf_acc/max(USGS_gd_2011$Rf_acc))
  #2015
  USGS_gd_2015$Rf_acc_std <- (USGS_gd_2015$Rf_acc/max(USGS_gd_2015$Rf_acc))
  #2016
  USGS_pk_2016$Rf_acc_std <- (USGS_pk_2016$Rf_acc/max(USGS_pk_2016$Rf_acc))
  USGS_rf_2016$Rf_acc_std <- (USGS_rf_2016$Rf_acc/max(USGS_rf_2016$Rf_acc))
  USGS_rf_2016_2$Rf_acc_std <- (USGS_rf_2016_2$Rf_acc/max(USGS_rf_2016_2$Rf_acc))
  #2017
  USGS_pk_2017$Rf_acc_std <- (USGS_pk_2017$Rf_acc/max(USGS_pk_2017$Rf_acc))
  USGS_rf_2017$Rf_acc_std <- (USGS_rf_2017$Rf_acc/max(USGS_rf_2017$Rf_acc))
  USGS_rf_2017_2$Rf_acc_std <- (USGS_rf_2017_2$Rf_acc/max(USGS_rf_2017_2$Rf_acc))
  #2018
  USGS_pk_2018$Rf_acc_std <- (USGS_pk_2018$Rf_acc/max(USGS_pk_2018$Rf_acc))
  USGS_rf_2018$Rf_acc_std <- (USGS_rf_2018$Rf_acc/max(USGS_rf_2018$Rf_acc))
  USGS_rf_2018_2$Rf_acc_std <- (USGS_rf_2018_2$Rf_acc/max(USGS_rf_2018_2$Rf_acc))
  #2019
  USGS_gd_2019$Rf_acc_std <- (USGS_gd_2019$Rf_acc/max(USGS_gd_2019$Rf_acc))
  
##END OF Get the accumulated rainfall on a scale from 0 to 1 (divide each value by max value for each year)-----------
  
  
##Plot the accumulated rainfall for all the water years---------------------------------------------------

  #plot
  plot(USGS_gd_2011$Rf_acc, type = "l", col = "red", xaxt = "n", lty =1,
       ylab = "Accumulated Rainfall (mm)", xlab = "Two days prior peakflow or hourly max rainfall")
  lines(USGS_gd_2015$Rf_acc, type = "l", col = "green", lty =1)
  lines(USGS_gd_2019$Rf_acc, type = "l", col = "darkorange", lty =1)
  lines(USGS_pk_2016$Rf_acc, type = "l", col = "blue", lty =5)
  lines(USGS_pk_2017$Rf_acc, type = "l", col = "deepskyblue", lty =5)
  lines(USGS_pk_2018$Rf_acc, type = "l", col = "chocolate4", lty =5)
  lines(USGS_rf_2016$Rf_acc, type = "l", col = "darkorchid1", lty =3)
  lines(USGS_rf_2017$Rf_acc, type = "l", col = "darkolivegreen4", lty =3)
  lines(USGS_rf_2018$Rf_acc, type = "l", col = "black", lty =3)
  axis(1, at = c(0,25,50), labels = c("2","1","0"))
  title("Accumulated Rainfall (mm) Two Days Prior Peakflow")
  legend("topleft", legend = c("2011", "2015", "pk_2016", "rf_2016", "pk_2017", "rf_2017",
                               "pk_2018", "rf_2018", "2019"),
                                col = c("red","green", "blue", "darkorchid1", "deepskyblue","darkolivegreen4",
                               "chocolate4", "black", "darkorange"), lty = c(1,1,5,3,5,3,5,3,1),
                                title = "Year",
                                ncol = 2, cex = 0.68, bty = "n", text.width = 1.6)
  
  #plot based on the discharge associated to hourly max rainfall
  plot(USGS_gd_2011$Rf_acc, type = "l", col = "red", xaxt = "n", lty =1,
       ylab = "Accumulated Rainfall (mm)", xlab = "Two days prior peakflow or hourly max rainfall")
  lines(USGS_gd_2015$Rf_acc, type = "l", col = "green", lty =1)
  lines(USGS_gd_2019$Rf_acc, type = "l", col = "darkorange", lty =1)
  lines(USGS_pk_2016$Rf_acc, type = "l", col = "blue", lty =5)
  lines(USGS_pk_2017$Rf_acc, type = "l", col = "deepskyblue", lty =5)
  lines(USGS_pk_2018$Rf_acc, type = "l", col = "chocolate4", lty =5)
  lines(USGS_rf_2016_2$Rf_acc, type = "l", col = "darkorchid1", lty =3)
  lines(USGS_rf_2017_2$Rf_acc, type = "l", col = "darkolivegreen4", lty =3)
  lines(USGS_rf_2018_2$Rf_acc, type = "l", col = "black", lty =3)
  axis(1, at = c(0,25,50), labels = c("2","1","0"))
  title("Accumulated Rainfall (mm) Two Days Prior Peakflow")
  legend("topleft", legend = c("2011", "2015", "pk_2016", "rf_2016", "pk_2017", "rf_2017",
                               "pk_2018", "rf_2018", "2019"),
         col = c("red","green", "blue", "darkorchid1", "deepskyblue","darkolivegreen4",
                 "chocolate4", "black", "darkorange"), lty = c(1,1,5,3,5,3,5,3,1),
         title = "Year",
         ncol = 2, cex = 0.68, bty = "n", text.width = 1.6)
  
  
  #With fill symbols
  legend("topleft", legend = c("2011", "2015", "pk_2016", "rf_2016", "pk_2017", "rf_2017",
                               "pk_2018", "rf_2018", "2019"), title = "Year",
                               fill = c("red","green", "blue", "darkorchid1", "deepskyblue","deeppink",
                               "chocolate4", "black", "darkorange"), ncol = 2, cex = 0.68, 
                               bty = "n", text.width = 1.6)
                               
##END OF Plot the accumulated rainfall for all the water years---------------------------------------------------



##Plot the standardized accumulated rainfall for all the years-------------------------------------------------

  #plot based on the max rainfall for bad rainfall match
  plot(USGS_gd_2011$Rf_acc_std, type = "l", col = "red", xaxt = "n", lty =1,
       ylab = "Standardized Accumulated Rainfall", xlab = "Two days prior peakflow or hourly max rainfall")
  lines(USGS_gd_2015$Rf_acc_std, type = "l", col = "green", lty =1)
  lines(USGS_gd_2019$Rf_acc_std, type = "l", col = "darkorange", lty =1)
  lines(USGS_pk_2016$Rf_acc_std, type = "l", col = "blue", lty =5)
  lines(USGS_pk_2017$Rf_acc_std, type = "l", col = "deepskyblue", lty =5)
  lines(USGS_pk_2018$Rf_acc_std, type = "l", col = "chocolate4", lty =5)
  lines(USGS_rf_2016$Rf_acc_std, type = "l", col = "darkorchid1", lty =3)
  lines(USGS_rf_2017$Rf_acc_std, type = "l", col = "darkolivegreen4", lty =3)
  lines(USGS_rf_2018$Rf_acc_std, type = "l", col = "black", lty =3)
  axis(1, at = c(0,25,50), labels = c("2","1","0"))
  title("Standardized Accumulated Rainfall Two Days Prior Peakflow")
  legend("topleft", legend = c("Good 2011", "Good 2015", "pk_2016", "rf_2016", "pk_2017", "rf_2017",
                               "pk_2018", "rf_2018", "Good 2019"),
         col = c("red","green", "blue", "darkorchid1", "deepskyblue","darkolivegreen4",
                 "chocolate4", "black", "darkorange"), lty = c(1,1,5,3,5,3,5,3,1),
         title = "Year",
         ncol = 2, cex = 0.6, bty = "n", text.width = 5)
  
  #plot based on the discharge associated to hourly max rainfall
  plot(USGS_gd_2011$Rf_acc_std, type = "l", col = "red", xaxt = "n", lty =1,
       ylab = "Standardized Accumulated Rainfall", xlab = "Two days prior peakflow or hourly max rainfall")
  lines(USGS_gd_2015$Rf_acc_std, type = "l", col = "green", lty =1)
  lines(USGS_gd_2019$Rf_acc_std, type = "l", col = "darkorange", lty =1)
  lines(USGS_pk_2016$Rf_acc_std, type = "l", col = "blue", lty =5)
  lines(USGS_pk_2017$Rf_acc_std, type = "l", col = "deepskyblue", lty =5)
  lines(USGS_pk_2018$Rf_acc_std, type = "l", col = "chocolate4", lty =5)
  lines(USGS_rf_2016_2$Rf_acc_std, type = "l", col = "darkorchid1", lty =3)
  lines(USGS_rf_2017_2$Rf_acc_std, type = "l", col = "darkolivegreen4", lty =3)
  lines(USGS_rf_2018_2$Rf_acc_std, type = "l", col = "black", lty =3)
  axis(1, at = c(0,25,50), labels = c("2","1","0"))
  title(" Standardized Accumulated Rainfall Two Days Prior Peakflow")
  legend("topleft", legend = c("Good 2011", "Good 2015", "pk_2016", "rf_2016", "pk_2017", "rf_2017",
                               "pk_2018", "rf_2018", "Good 2019"),
         col = c("red","green", "blue", "darkorchid1", "deepskyblue","darkolivegreen4",
                 "chocolate4", "black", "darkorange"), lty = c(1,1,5,3,5,3,5,3,1),
         title = "Year",
         ncol = 2, cex = 0.6, bty = "n", text.width = 5)

##END OF plot the standardized accumulated rainfall for all the years-------------------------------------------------


  
##########################213215157552800/16208000#############################################
  
##Get rainfall accumulation 2 days prior to the peakflow for the three good and three bad events plotted--------------------  
  
  #Read USGS station file
  USGS <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/USGS_uv213215157552800.csv", header = T)
  
  #Get the peakflow dates for station 16208000
  peakdata <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/max_rainfall_table/new_check_updated_date_USGS_uv213215157552800_16208000.csv", header = T)
  
  #Keep only water years I am interested in
  peakdata <- peakdata[c(3,4:6,8),]
  
  #Get discharge data
  siteNumbers <- "16208000"
  parameterCd <- "00060"
  startDate <- substr(USGS$DateTime[1],1,10)
  endDate <- substr(USGS$DateTime[length(USGS$DateTime)],1,10)
  Q16208000 <- readNWISuv(siteNumbers, parameterCd, startDate = startDate, endDate = endDate)
  Q16208000$dateTime <- as.POSIXct(Q16208000$dateTime, origin = "%Y-%m-%d %H:%M", tz = "GMT") #datetime format
  Q16208000$dateTime <- as.POSIXct(format(Q16208000$dateTime, tz = "HST")) #Hawaii time zone
  Q16208000 <- Q16208000[,c(3,4)] #keep columns we are interested in
  names(Q16208000)[c(1:2)] <- c("DateTime", "Q_cfs") #rename columns
  
  #Calculate rainfall accumulation for each water year
  #2014
  USGS_gd_2014 <- USGS[c(40202:40250),]
  USGS_gd_2014$Rf_acc <- cumsum(USGS_gd_2014$RF_mm)
  #2015
  USGS_pk_2015 <- USGS[c(46973:47021),]
  USGS_pk_2015$Rf_acc <- cumsum(USGS_pk_2015$RF_mm)
  USGS_rf_2015 <- USGS[c(50254:50302),]
  USGS_rf_2015$Rf_acc <- cumsum(USGS_rf_2015$RF_mm)
  #2016
  USGS_pk_2016 <- USGS[c(57864:57912),]
  USGS_pk_2016$Rf_acc <- cumsum(USGS_pk_2016$RF_mm)
  USGS_rf_2016 <- USGS[c(51996:52044),]
  USGS_rf_2016$Rf_acc <- cumsum(USGS_rf_2016$RF_mm)
  #2017
  USGS_pk_2017 <- USGS[c(63121:63169),]
  USGS_pk_2017$Rf_acc <- cumsum(USGS_pk_2017$RF_mm)
  USGS_rf_2017 <- USGS[c(64760:64808),]
  USGS_rf_2017$Rf_acc <- cumsum(USGS_rf_2017$RF_mm)
  #2019
  USGS_pk_2019 <- USGS[c(85414:85462),]
  USGS_pk_2019$Rf_acc <- cumsum(USGS_pk_2019$RF_mm)
  USGS_rf_2019 <- USGS[c(79142:79190),]
  USGS_rf_2019$Rf_acc <- cumsum(USGS_rf_2019$RF_mm)
  
##END OF get rainfall accumulation 2 days prior to the peakflow for the three good and three bad events plotted--------------------  
  
  
  
##Get the accumulated rainfall on a scale from 0 to 1 (divide each value by max value for each year)---------------
  #2014
  USGS_gd_2014$Rf_acc_std <- (USGS_gd_2014$Rf_acc/max(USGS_gd_2014$Rf_acc))
  #2015
  USGS_pk_2015$Rf_acc_std <- (USGS_pk_2015$Rf_acc/max(USGS_pk_2015$Rf_acc))
  USGS_rf_2015$Rf_acc_std <- (USGS_rf_2015$Rf_acc/max(USGS_rf_2015$Rf_acc))
  #2016
  USGS_pk_2016$Rf_acc_std <- (USGS_pk_2016$Rf_acc/max(USGS_pk_2016$Rf_acc))
  USGS_rf_2016$Rf_acc_std <- (USGS_rf_2016$Rf_acc/max(USGS_rf_2016$Rf_acc))
  #2017
  USGS_pk_2017$Rf_acc_std <- (USGS_pk_2017$Rf_acc/max(USGS_pk_2017$Rf_acc))
  USGS_rf_2017$Rf_acc_std <- (USGS_rf_2017$Rf_acc/max(USGS_rf_2017$Rf_acc))
  #2019
  USGS_pk_2019$Rf_acc_std <- (USGS_pk_2019$Rf_acc/max(USGS_pk_2019$Rf_acc))
  USGS_rf_2019$Rf_acc_std <- (USGS_rf_2019$Rf_acc/max(USGS_rf_2019$Rf_acc))
  
##END OF Get the accumulated rainfall on a scale from 0 to 1 (divide each value by max value for each year)-----------
  
  
  
##Plot the accumulated rainfall and standardized accumulated rainfall for all the years-------------------------------------------------
  
  #plot accumulated rainfall
  plot(USGS_gd_2014$Rf_acc, type = "l", col = "red", xaxt = "n", lty =1,
       ylab = "Accumulated Rainfall (mm)", xlab = "Two days prior peakflow or hourly max rainfall")
  lines(USGS_pk_2015$Rf_acc, type = "l", col = "blue", lty =5)
  lines(USGS_pk_2016$Rf_acc, type = "l", col = "deepskyblue", lty =5)
  lines(USGS_pk_2017$Rf_acc, type = "l", col = "darkorange", lty = 5)
  lines(USGS_pk_2019$Rf_acc, type = "l", col = "chocolate4", lty =5)
  lines(USGS_rf_2015$Rf_acc, type = "l", col = "darkorchid1", lty =3)
  lines(USGS_rf_2016$Rf_acc, type = "l", col = "darkolivegreen4", lty =3)
  lines(USGS_rf_2017$Rf_acc, type = "l", col = "green", lty = 3 )
  lines(USGS_rf_2019$Rf_acc, type = "l", col = "black", lty =3)
  axis(1, at = c(0,25,50), labels = c("2","1","0"))
  title("Accumulated Rainfall (mm) Two Days Prior Peakflow")
  legend("topleft", legend = c("2014", "pk_2015", "pk_2016", "pk_2017", "pk_2019", "rf_2015", 
                               "rf_2016", "rf_2017", "rf_2019"),
         col = c("red","blue", "deepskyblue", "darkorange", "chocolate4", "darkorchid1","darkolivegreen4",
                 "green", "black"), lty = c(1,5,5,5,5,3,3,3,3),
         title = "Year",
         ncol = 2, cex = 0.68, bty = "n", text.width = 1.6)
  
  #plot standardized accumulated rainfall
  plot(USGS_gd_2014$Rf_acc_std, type = "l", col = "red", xaxt = "n", lty =1,
       ylab = "Standardized Accumulated Rainfall (mm)", xlab = "Two days prior peakflow or hourly max rainfall")
  lines(USGS_pk_2015$Rf_acc_std, type = "l", col = "blue", lty =5)
  #lines(USGS_pk_2016$Rf_acc_std, type = "l", col = "deepskyblue", lty =5)
  lines(USGS_pk_2017$Rf_acc_std, type = "l", col = "darkorange", lty = 5)
  lines(USGS_pk_2019$Rf_acc_std, type = "l", col = "chocolate4", lty =5)
  #lines(USGS_rf_2015$Rf_acc_std, type = "l", col = "darkorchid1", lty =3)
  lines(USGS_rf_2016$Rf_acc_std, type = "l", col = "darkolivegreen4", lty =3)
  lines(USGS_rf_2017$Rf_acc_std, type = "l", col = "green", lty = 3 )
  lines(USGS_rf_2019$Rf_acc_std, type = "l", col = "black", lty =3)
  axis(1, at = c(0,25,50), labels = c("2","1","0"))
  title("Standardized Accumulated Rainfall (mm) Two Days Prior Peakflow")
  legend("topleft", legend = c("2014", "pk_2015", "pk_2016", "pk_2017", "pk_2019", "rf_2015", 
                               "rf_2016", "rf_2017", "rf_2019"),
         col = c("red","blue", "deepskyblue", "darkorange", "chocolate4", "darkorchid1","darkolivegreen4",
                 "green", "black"), lty = c(1,5,5,5,5,3,3,3,3),
         title = "Year",
         ncol = 2, cex = 0.68, bty = "n", text.width = 1.6)

##END OF plot the accumulated rainfall and standardized accumulated rainfall for all the years-------------------------------------------------  

  
  
##########################MNLH1/16240500#######################################################    

##Get rainfall accumulation 2 days prior to the peakflow for the three good and three bad events plotted--------------------  
  
  #Read USGS station file
  MNLH1 <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/MNLH1.csv", header = T)
  
  #Get the peakflow dates for station 16208000
  peakdata <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/max_rainfall_table/new_check_updated_date_MNLH1_16240500.csv", header = T)
  
  #Keep only water years I am interested in
  peakdata <- peakdata[c(3,11,12,14,15,18),]
  
  #Get discharge data
  siteNumbers <- "16240500"
  parameterCd <- "00060"
  startDate <- "1998-04-01"
  endDate <- "2019-07-01"
  Q16240500 <- readNWISuv(siteNumbers, parameterCd, startDate = startDate, endDate = endDate)
  Q16240500$dateTime <- as.POSIXct(Q16240500$dateTime, origin = "%Y-%m-%d %H:%M", tz = "GMT") #datetime format
  Q16240500$dateTime <- as.POSIXct(format(Q16240500$dateTime, tz = "HST")) #Hawaii time zone
  Q16240500 <- Q16240500[,c(3,4)] #keep columns we are interested in
  names(Q16240500)[c(1:2)] <- c("DateTime", "Q_cfs") #rename columns
  
  #Calculate rainfall accumulation for each water year
  #2000
  MNLH1_pk_2000 <- MNLH1[c(15765:15813),]
  MNLH1_pk_2000$Rf_acc <- cumsum(MNLH1_pk_2000$RF)
  MNLH1_rf_2000 <- MNLH1[c(14596:14644),]
  MNLH1_rf_2000$Rf_acc <- cumsum(MNLH1_rf_2000$RF)
  #2008
  MNLH1_pk_2008 <- MNLH1[c(84631:84679),]
  MNLH1_pk_2008$Rf_acc <- cumsum(MNLH1_pk_2008$RF)
  MNLH1_rf_2008 <- MNLH1[c(90274:90322),]
  MNLH1_rf_2008$Rf_acc <- cumsum(MNLH1_rf_2008$RF)
  #2009
  MNLH1_pk_2009 <- MNLH1[c(93727:93775),]
  MNLH1_pk_2009$Rf_acc <- cumsum(MNLH1_pk_2009$RF)
  MNLH1_rf_2009 <- MNLH1[c(95956:96004),]
  MNLH1_rf_2009$Rf_acc <- cumsum(MNLH1_rf_2009$RF)
  #2011
  MNLH1_gd_2011 <- MNLH1[c(111444:111492),]
  MNLH1_gd_2011$Rf_acc <- cumsum(MNLH1_gd_2011$RF)
  #2012
  MNLH1_gd_2012 <- MNLH1[c(120049:120097),]
  MNLH1_gd_2012$Rf_acc <- cumsum(MNLH1_gd_2012$RF)
  #2017
  MNLH1_gd_2017 <- MNLH1[c(170128:170176),]
  MNLH1_gd_2017$RF[42] <- 0.000
  MNLH1_gd_2017$Rf_acc <- cumsum(MNLH1_gd_2017$RF)
  
##END OF get rainfall accumulation 2 days prior to the peakflow for the three good and three bad events plotted--------------------  
  
  
  
##Get the accumulated rainfall on a scale from 0 to 1 (divide each value by max value for each year)---------------
  #2000
  MNLH1_pk_2000$RF_acc_std <- (MNLH1_pk_2000$Rf_acc/max(MNLH1_pk_2000$Rf_acc))
  MNLH1_rf_2000$RF_acc_std <- (MNLH1_rf_2000$Rf_acc/max(MNLH1_rf_2000$Rf_acc))
  #2008
  MNLH1_pk_2008$RF_acc_std <- (MNLH1_pk_2008$Rf_acc/max(MNLH1_pk_2008$Rf_acc))
  MNLH1_rf_2008$RF_acc_std <- (MNLH1_rf_2008$Rf_acc/max(MNLH1_rf_2008$Rf_acc))
  #2009
  MNLH1_pk_2009$RF_acc_std <- (MNLH1_pk_2009$Rf_acc/max(MNLH1_pk_2009$Rf_acc))
  MNLH1_rf_2009$RF_acc_std <- (MNLH1_rf_2009$Rf_acc/max(MNLH1_rf_2009$Rf_acc))
  #2011
  MNLH1_gd_2011$Rf_acc_std <- (MNLH1_gd_2011$Rf_acc/max(MNLH1_gd_2011$Rf_acc))
  #2012
  MNLH1_gd_2012$Rf_acc_std <- (MNLH1_gd_2012$Rf_acc/max(MNLH1_gd_2012$Rf_acc))
  #2017
  MNLH1_gd_2017$Rf_acc_std <- (MNLH1_gd_2017$Rf_acc/max(MNLH1_gd_2017$Rf_acc))
  
##END OF Get the accumulated rainfall on a scale from 0 to 1 (divide each value by max value for each year)-----------
 
  
  
##Plot the accumulated rainfall and standardized accumulated rainfall for all the years-------------------------------------------------
  
  #plot accumulated rainfall
  plot(MNLH1_gd_2011$Rf_acc, type = "l", col = "red", xaxt = "n", lty =1,
       ylab = "Accumulated Rainfall (mm)", 
       xlab = "Two days prior peakflow or hourly max rainfall", ylim = c(0,200))
  lines(MNLH1_gd_2012$Rf_acc, type = "l", col = "deepskyblue", lty =1)
  lines(MNLH1_gd_2017$Rf_acc, type = "l", col = "darkorange", lty = 1)
  lines(MNLH1_pk_2000$Rf_acc, type = "l", col = "chocolate4", lty =5)
  lines(MNLH1_rf_2000$Rf_acc, type = "l", col = "darkolivegreen4", lty =3)
  lines(MNLH1_pk_2008$Rf_acc, type = "l", col = "green", lty = 5)
  lines(MNLH1_rf_2008$Rf_acc, type = "l", col = "blue", lty =3)
  lines(MNLH1_pk_2009$Rf_acc, type = "l", col = "darkorchid1", lty = 5)
  lines(MNLH1_rf_2009$Rf_acc, type = "l", col = "black", lty =3)
  axis(1, at = c(0,25,50), labels = c("2","1","0"))
  title("Accumulated Rainfall (mm) Two Days Prior Peakflow")
  legend("topleft", legend = c("2011", "2012", "2017", "pk_2000", "rf_2000", "pk_2008", 
                               "rf_2008", "pk_2009", "rf_2009"),
         col = c("red","deepskyblue", "darkorange", "chocolate4", "darkolivegreen4", "green","blue",
                 "darkorchid1", "black"), lty = c(1,1,1,5,3,5,3,5,3),
         title = "Year",
         ncol = 2, cex = 0.68, bty = "n", text.width = 1.6)
  
  #plot standardized accumulated rainfall
  plot(MNLH1_gd_2011$Rf_acc_std, type = "l", col = "red", xaxt = "n", lty =1,
       ylab = "Accumulated Rainfall (mm)", 
       xlab = "Two days prior peakflow or hourly max rainfall", ylim = c(0,1))
  lines(MNLH1_gd_2012$Rf_acc_std, type = "l", col = "deepskyblue", lty =1)
  lines(MNLH1_gd_2017$Rf_acc_std, type = "l", col = "darkorange", lty = 1)
  lines(MNLH1_pk_2000$RF_acc_std, type = "l", col = "chocolate4", lty =5)
  lines(MNLH1_rf_2000$RF_acc_std, type = "l", col = "darkolivegreen4", lty =3)
  lines(MNLH1_pk_2008$RF_acc_std, type = "l", col = "green", lty = 5)
  lines(MNLH1_rf_2008$RF_acc_std, type = "l", col = "blue", lty =3)
  lines(MNLH1_pk_2009$RF_acc_std, type = "l", col = "darkorchid1", lty = 5)
  lines(MNLH1_rf_2009$RF_acc_std, type = "l", col = "black", lty =3)
  axis(1, at = c(0,25,50), labels = c("2","1","0"))
  title("Standardized Accumulated Rainfall (mm) Two Days Prior Peakflow")
  legend("topleft", legend = c("2011", "2012", "2017", "pk_2000", "rf_2000", "pk_2008", 
                               "rf_2008", "pk_2009", "rf_2009"),
         col = c("red","deepskyblue", "darkorange", "chocolate4", "darkolivegreen4", "green","blue",
                 "darkorchid1", "black"), lty = c(1,1,1,5,3,5,3,5,3),
         title = "Year",
         ncol = 2, cex = 0.68, bty = "n", text.width = 1.6)
  
##END OF plot the accumulated rainfall and standardized accumulated rainfall for all the years-------------------------------------------------  
  
  
  
  
  

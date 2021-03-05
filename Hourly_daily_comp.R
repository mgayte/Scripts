#############################################
#                                           #
#          Transpose hourly to daily        #
#                    for                    #
#                comparisons                #
#                                           #
#############################################


#Read hourly files for the three studied stations
MNLH1 <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data/MNLH1.csv", header = T)
USGS <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data/USGS_uv213215157552800.csv")

#Have datetime column in a datetime format
MNLH1$DateTime <- as.Date(MNLH1$DateTime, format = "%m/%d/%Y")
USGS$DateTime <- as.Date(USGS$DateTime, format = "%Y-%m-%d")

#Tranform datetime from hourly to daily data
Daily_MNLH1<- MNLH1 %>%
  mutate(Date = as.Date(DateTime, format="%Y-%m-%d")) %>%
  group_by(Date) %>%
  summarise(RF_mm=sum(RF))

Daily_USGS<- USGS %>%
  mutate(Date = as.Date(DateTime, format="%Y-%m-%d")) %>%
  group_by(Date) %>%
  summarise(RF_mm=sum(RF_mm))

#Download USGS hourly data and get MNLH1 daily data from Yufen
install.packages("dataRetrieval")
library(dataRetrieval)

siteNumber <- "213215157552800"
parameterCd <- "00045"
startDate <- ""
endDate <- ""

USGS_download <- readNWISdv(siteNumber, parameterCd, startDate, endDate, statCd="00006")

#Make downloaded USGS daily data with no gap in day and time and right period of time
USGS_download <- USGS_download[,c(3,6)]
names(USGS_download)[2] <- "RF_mm"
USGS_download$RF_mm <- USGS_download$RF_mm * 25.4
USGS_download <- USGS_download[c(13370:17056),]
seq_time_daily_USGS <- seq(as.Date(USGS_download$Date[1]), as.Date(USGS_download$Date[length(USGS_download$Date)]), by = "day")
df_time_daily_USGS <- data.frame(Date = seq_time_daily_USGS)
USGS_download <- merge(df_time_daily_USGS, USGS_download, by = "Date", all.x = TRUE)

#Get Yufen daily data and make it match dates of daily_MNLH1 and make perfect seq of time
NCDC <- read.csv("C:/Users/mgayt/Documents/NCDC_daily_precip_USC00517810.csv")
NCDC <- NCDC[c(17223:24714),]
NCDC$Date <- as.Date(NCDC$Date)

seq_time_daily <- seq(as.Date(NCDC$Date[1]), as.Date(NCDC$Date[length(NCDC$Date)]), by = "day")
df_time_daily <- data.frame(Date = seq_time_daily)
NCDC_daily <- merge(df_time_daily, NCDC, by = "Date", all.x = TRUE)

#Make plot for MNLH1 and NCDC
plot(Daily_MNLH1$RF_mm, type = "l", pch = 20, col = "red", xaxt = "n", xlab = "Year",
     ylab = "RF_mm")
lines(NCDC_daily$Preciptation_mm, type = "l", pch = 20, col = "blue")

plot(NCDC_daily$Preciptation_mm, type = "l", pch = 20, col = "blue", xaxt = "n", xlab = "Year",
     ylab = "RF_mm")
lines(Daily_MNLH1$RF_mm, type = "l", pch = 20, col = "red")

#Make plot for USGS
plot(Daily_USGS$RF_mm, type = "l", pch = 20, col = "red", xaxt = "n", xlab = "Year",
     ylab = "RF_mm")
lines(USGS_download$RF_mm, type = "l", pch = 20, col = "blue")

plot(USGS_download$RF_mm, type = "l", pch = 20, col = "blue", xaxt = "n", xlab = "Year",
     ylab = "RF_mm")
lines(Daily_USGS$RF_mm, type = "l", pch = 20, col = "red")

#Two plot into one graph MNLH1 and NCDC
barplot(Daily_MNLH1$RF_mm,
        space = c(0,1),width = 0.5,
        ylim=rev(c(0,300)), ylab="Rainfall_MNLH1(mm)",
        xlab="",
        axes=TRUE,las=1,xaxt="n", col="light blue", border="light blue")
par(new=T)
plot(NCDC_daily$Preciptation_mm,type="l",pch=21, col="red", lty=12,lwd=1.5,
     yaxt="n", ylim=c(0,300),
     xlab="",ylab="",axes=T)

#Two plot into one graph USGS
barplot(Daily_USGS$RF_mm,
        space = c(0,1),width = 0.5,
        ylim=rev(c(0,300)), ylab="Rainfall_USGS(mm)",
        xlab="",
        axes=TRUE,las=1,xaxt="n", col="light blue", border="light blue")
par(new=T)
plot(USGS_download$RF_mm,type="l",pch=21, col="red", lty=12,lwd=1.5,
     yaxt="n", ylim=c(0,300),
     xlab="",ylab="",axes=T)

#Scatterplot MNLH1
plot(MNLH1$DateTime, MNLH1$RF, pch = 16, col = "red")
points(NCDC_daily$Date, NCDC_daily$Preciptation_mm, pch = 20, col = "blue")

#Scatterplot USGS
plot(Daily_USGS$Date, Daily_USGS$RF_mm, pch = 16, col = "red")
points(USGS_download$Date, USGS_download$RF_mm, pch = 16, col = "blue")

plot(USGS_download$Date, USGS_download$RF_mm, pch = 16, col = "blue")
points(Daily_USGS$Date, Daily_USGS$RF_mm, pch = 16, col = "red")

plot(USGS_download$RF_mm, Daily_USGS$RF_mm, xlim = c(0,50), ylim = c(0,50))
















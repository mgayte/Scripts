#############################################
#                                           #          
#                                           #
#                QC Protocol                #
#                                           #
#                                           #
#############################################


#Load library
library(dplyr)
library(naniar)
library(data.table)
library(zoo)


##Quality Control for rainfall data--------------------------------------------------------------------------------------------------------

  #List our hourly rainfall files
  hourly_files <- list.files("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC", pattern = "csv", recursive = TRUE)
  hourly_files <- gsub(pattern = "\\.csv$", "", hourly_files)
  hourly_files <- hourly_files[c(15:172,276:283)] #only ecohydrology lab now (166 files)

  #Loop for all the files
  for (i in 1:length(hourly_files)){
     
    #Read hourly files
    station <- read.csv(paste("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/", hourly_files[i], ".csv", sep = ""), stringsAsFactors = F)
    #station <- station[,-c(5,6)] Only for COOP files
  
    ###Create an extra column for flag code and for corrected data
    flag_code <- c("flag_code")
    corrected_data <- c("corrected_data")
    station[, flag_code] <- NA
    station[, corrected_data] <- NA
    station$flag_code <- as.numeric(station$flag_code)
    station$corrected_data <- as.numeric(station$corrected_data)
  
    ###Delete beginning rows if NA
    No_NA <- which(!is.na(station$RF_mm))
    first_value <- is.na(station[1,2])
    if (first_value == TRUE) station <- station[-c(1:No_NA[1]-1),]
  
    ###Control tests (Format, Tolerance, Variability)
    ##DateTime format as %Y%m%d %H:%M:%S and HST time zone (UTC-10) only for NCEI ISD stations
    #station$DateTime <- as.POSIXct(station$DateTime, origin = "1960-01-01 00:00:00", tz = "GMT")
    #station$DateTime <- format(station$DateTime, tz = "HST")
  
    ##Original data
    station$flag_code[station$RF_mm >= 0 & !is.na(station$RF_mm)] <- 0
    station$corrected_data[station$flag_code == 0 & !is.na(station$flag_code)] <- station$RF_mm[station$RF_mm >= 0 & !is.na(station$RF_mm)] 
  
    ##Trace precipitation
    station$flag_code[station$RF_mm == -0.1 & !is.na(station$RF_mm)] <- 1
    station$corrected_data[station$flag_code == 1 & !is.na(station$flag_code)] <- 0
  
    ##Other negative values
    station$flag_code[station$RF_mm < -0.1 & !is.na(station$RF_mm)] <- 2.1
    station$corrected_data[station$flag_code == 2.1 & !is.na(station$flag_code)] <- NA
  
    ##Variability test (create previous and after RF vectors and dataframe to find variability. dataframe was used to check if the shift function was used correctly)
    standard_deviation <- sd(station$RF_mm, na.rm = TRUE)
    max_variability_no_NA <- 8*standard_deviation
    max_variability_NA <- 4*standard_deviation
  
    #Create previous, actual, after rainfall vectors
    RF <- station$RF_mm
    df_RF <- data.frame(RF = RF)
    df_RF$RF[df_RF$RF == -0.1] <- 0
    previous_RF <- shift(df_RF$RF, n=1, fill=NA, type = "lag", give.names = FALSE)
    df_previous_RF <- data.frame(previous_RF = previous_RF)
    after_RF <- shift(df_RF$RF, n=1, fill=NA, type = "lead", give.names = FALSE)
    df_after_RF <- data.frame(after_RF = after_RF)
  
    #Create previous, actual, after rainfall vectors with replacing NA by 0 
    RF_no_NA <- df_RF$RF
    Df_RF_no_NA <- data.frame(RF_no_NA = RF_no_NA)
    Df_RF_no_NA$RF_no_NA[is.na(Df_RF_no_NA$RF_no_NA)] <- 0
    Df_previous_RF_no_NA <- df_previous_RF
    Df_previous_RF_no_NA$previous_RF[is.na(Df_previous_RF_no_NA$previous_RF)] <- 0
    Df_after_RF_no_NA <- df_after_RF
    Df_after_RF_no_NA$after_RF[is.na(Df_after_RF_no_NA$after_RF)] <- 0
  
    #Calculate variability with NA values
    variability <- abs(previous_RF-df_RF$RF) + abs(after_RF-df_RF$RF)
    df_variability <- data.frame(variability = variability)
  
    #Calculate variability without NA values
    variability_no_NA <- abs(Df_previous_RF_no_NA$previous_RF-Df_RF_no_NA$RF) + abs(Df_after_RF_no_NA$after_RF-Df_RF_no_NA$RF)
    Df_variability_no_NA <- data.frame(variability_no_NA = variability_no_NA)
  
    #Identify where we have at least NA values for each variability and set up max_variabiity column
    df <- data.frame(Previous = previous_RF, rf = df_RF$RF, after = after_RF, NA_or_Not = NA, max_variability = NA )
    df$NA_or_Not[is.na(df$Previous)|is.na(df$rf)|is.na(df$after)] <- TRUE
  
    #Compare calculated variability with maximum variability based on std
    df$max_variability....NA[df$NA_or_Not == TRUE] <- max_variability_NA
    df$max_variability....NA[is.na(df$NA_or_Not)] <- max_variability_no_NA
  
    station$flag_code[df$max_variability....NA < Df_variability_no_NA & station$RF_mm != -0.1] <- 2.3
  
    ##Threshold test
    station$flag_code[station$RF_mm < -0.1 & !is.na(station$RF_mm) | station$RF_mm > 150 & !is.na(station$RF_mm)] <- 2.2  
  
    ##Missing data
    station$flag_code[is.na(station$RF_mm)] <- 3

    ##Export files  
    write.csv(station, file = paste("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/", hourly_files[i], ".csv", sep = ""), row.names = FALSE)
  
}







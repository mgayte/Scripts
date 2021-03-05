#############################################
#                                           #          
#                                           #
#        Exceedence Probability Map         #
#                                           #
#                                           #
#############################################

#Load library
library(dplyr)
library(lubridate)
library(EnvStats)

##Identify stations with data from 1990 to 2020 having at least 20 years of data------------------------------------------------------------------------------

  #Load lab metadata table
  lab_metadata <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Table/Metadata_Lab.csv", header = T)

  #Start/End dates in dateTime format
  lab_metadata$Start_Date <- as.POSIXct(lab_metadata$Start_Date, format = "%m/%d/%Y %H:%M")
  lab_metadata$End_Date <- as.POSIXct(lab_metadata$End_Date, format = "%m/%d/%Y %H:%M")
  lab_metadata$difftime <- (difftime(lab_metadata$End_Date, lab_metadata$Start_Date, units = "days")/365)
  
  #Keep rows with data from 1990 to 2019 and at least 20 years of data
  data <- lab_metadata[lab_metadata$difftime >= 20,]
  data <- data[data$End_Date >= "2010-01-01",]#Make sure the file is at least from 1990 to 2010!!
  
  #Get filenames 
  filenames <- data$Filename

  #Create and export dataframe with files having wrong datetime format for use in the future
  #file_format <- data.frame(Filenames = filenames_format)
  #write.csv(file_format, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Table/filenames_format.csv", row.names = F)
  

##END OF Identify stations with data from 1990 to 2020 having at least 20 years of data------------------------------------------------------------------------

  

##Check files haven't got more than a week of continuous missing data------------------------------------------------------ 
  
  #Empty vector for loop
  v_week_NA <- c()
  v_ID <- c()
  
  #Loop
  for (i in 1:length(filenames)){
    
    #Read file
    rf_hourly <- read.csv(paste("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/",filenames[i], ".csv", sep = ""), stringsAsFactors = F)
    
    #DateTime format
    rf_hourly$DateTime <- as.POSIXct(rf_hourly$DateTime, format = "%Y-%m-%d %H:%M")
    
    #Get the right time period for each files
    rf_hourly <- rf_hourly[c(which(rf_hourly$DateTime >= "1990-01-01 00:00:00")):length(rf_hourly$DateTime),]
    
    #Check the maximum number of consecutive NAs
    cons_NA <- rle(is.na(rf_hourly$RF_mm))
    max_NA_cons <- max(cons_NA$lengths[cons_NA$values==TRUE])
    week_NA <- max_NA_cons > 672
    v_week_NA <- append(v_week_NA, week_NA)
    
    #ID of the files
    ID <- filenames[i]
    v_ID <- append(v_ID, ID)
  }

  #Create dataframe
  df <- data.frame(Filename = v_ID, Week_NA = v_week_NA)
  
##END OF check files haven't got more than a week of continuous missing data-----------------------------------------

  
##Check how many files have less than 20% of missing data------------------------------------------------------------------
  
  #Empty vector for loop
  v_NA <- c()
  v_ID <- c()
  
  #Loop
  for (i in 1:length(filenames)){
    i=2
    #Read file
    rf_hourly <- read.csv(paste("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/",filenames[i], ".csv", sep = ""), stringsAsFactors = F)
    
    #DateTime format
    rf_hourly$DateTime <- as.POSIXct(rf_hourly$DateTime, format = "%Y-%m-%d %H:%M")
    #rf_hourly$DateTime <- as.POSIXct(rf_hourly$DateTime, format = "%m/%d/%Y %H:%M")
    #write.csv(rf_hourly, file = paste("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/", filenames[i], ".csv", sep = "" ), row.names = F)
    
    
    #Get the right time period for each files
    rf_hourly <- rf_hourly[c(which(rf_hourly$DateTime >= "1990-01-01 00:00:00")):length(rf_hourly$DateTime),]
    
    #Percentage of missing data
    nrow <- NROW(rf_hourly$RF_mm)
    numb_NA <- length(which(is.na(rf_hourly$RF_mm)))
    percentage_NA <- (numb_NA/nrow)*100
    v_NA <- append(v_NA, percentage_NA)
    
    #Files ID
    ID <- filenames[i]
    v_ID <- append(v_ID, ID)
    
  }
  
  #Create dataframe
  df_NA <- data.frame(Filename = v_ID, Percentage_NA = v_NA)
  
  #Keep only files with less than 20% of missing data
  df_NA <- df_NA[df_NA$Percentage_NA <20,]
  
##END OF check how many files have less than 20% of missing data---------------------------------------------------  

  
  
##Check in files one month of continuing missing data each year----------------------------------------------------  
  
  
  
  
  
  
##Calculate exceedance probability (Not working as the data follow a Weibell distribution)-----------------------------------------------------------------------------------

  #Empty vectors for loop
  v_ID <- c()
  v_exceedance <- c()
  v_rainfall <- c()
  v_return_period_hr <- c()
  v_return_period_day <- c()
  
  #Filenames
  files <- df_NA$Filename
  
  #Loop to determine exceedance probability without NA and 0s for previously determined files
  
  for (i in 1:length(files)){
    
    #Read file
    rf_hourly <- read.csv(paste("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/",files[i], ".csv", sep = ""), stringsAsFactors = F)
    
    #DateTime format
    rf_hourly$DateTime <- as.POSIXct(rf_hourly$DateTime, format = "%Y-%m-%d %H:%M")
    
    #Get the right time period for each files
    rf_hourly <- rf_hourly[c(which(rf_hourly$DateTime >= "1990-01-01 00:00:00")):length(rf_hourly$DateTime),]
    
    #Remove rows with NA values and 0s
    rf_hourly <- rf_hourly[!is.na(rf_hourly$RF_mm),]
    rf_hourly <- rf_hourly[!rf_hourly$RF_mm==0,]
    
    #Rank the rainfall values
    rf_hourly$rank <- rank(-rf_hourly$RF_mm)
    #rf_hourly$ranklog <- rank(-rf_hourly$log_rf)
    
    #Exceedance prob
    rf_hourly$excprob <- rf_hourly$rank/(length(rf_hourly$RF_mm)+1)
    
    #Determine 99th percentile for rainfall values and associated exceedance probability
    exceedance_prob <- rf_hourly$excprob[quantile(rf_hourly$RF_mm, 0.99)]
    v_exceedance <- append(v_exceedance, exceedance_prob)
    quantile_99th <- quantile(rf_hourly$RF_mm, 0.99)
    v_rainfall <- append(v_rainfall, quantile_99th)
    
    #Calculate return period
    return_period_hr <- 1/exceedance_prob
    v_return_period_hr <- append(v_return_period_hr, return_period_hr)
    return_period_day <- return_period_hr/24
    v_return_period_day <- append(v_return_period_day, return_period_day)
    
    #Get filenames
    ID <- filenames[i]
    v_ID <- append(v_ID, ID)
    
  }

  #Create dataframe
  df_exc_prob <- data.frame(Filename = v_ID, Quantile_99th = v_rainfall, Exceedance_Prob = v_exceedance, Return_period_hr = v_return_period_hr,
                            Return_period_day =v_return_period_day)
  
  #Add lat/long, start/end dates, and months of the data into the table from the metadata
  df_exc_prob <- merge(df_exc_prob, lab_metadata, all.x = T, by = "Filename")
  df_exc_prob <- df_exc_prob[,c(6,7,1:5,8:24)]
  
  #Export table
  write.csv(df_exc_prob, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Table/exceedance_prob.csv", row.names = F)
  
##END OF Calculate exceedance probability-------------------------------------------------------------------  
  
  
  
##Determine Probability Density Function (PDF) and Cumulative Density Function (CDF)--------------------------------------------------------------  
  
  #Remove rows with NA values and 0s
  rf_hourly <- rf_hourly[!is.na(rf_hourly$RF_mm),]
  rf_hourly <- rf_hourly[!rf_hourly$RF_mm==0,]
  
  #Probability Density Function
  test <- density((rf_hourly$RF_mm))
  sum(test$y)*diff(test$x[1:2])
  hist(rf_hourly$RF_mm, breaks = 50)
  hist(rf_hourly$RF_mm, probability=TRUE, breaks = 50)
  lines(density(rf_hourly$RF_mm),col="red")
  
  #Cumulative distribution function
  cdf <- cumsum(test$y * diff(test$x[1:2]))
  cdf <- cdf / max(cdf) #to correct for the rounding errors
  plot(test$x,cdf,type="l")
  
  cdf_2 <- ecdf(rf_hourly$RF_mm)

 
 
 
  

  

  
  
  
#############################################
#                                           #          
#                                           #
#           Rainfall Intensity Map          #
#                                           #
#                                           #
#############################################



##Identify stations with data from 2000 to 2020------------------------------------------------------------------------------

  #Load lab metadata table
  lab_metadata <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Table/Metadata_Lab.csv", header = T)

  #Start/End dates in dateTime format
  lab_metadata$Start_Date <- as.POSIXct(lab_metadata$Start_Date, format = "%m/%d/%Y %H:%M")
  lab_metadata$End_Date <- as.POSIXct(lab_metadata$End_Date, format = "%m/%d/%Y %H:%M")
  
  #Keep rows with data from 2000 to 2020
  data <- lab_metadata[lab_metadata$Start_Date <= "2000-01-01" & lab_metadata$End_Date >= "2019-01-01",]
  
  #Get filenames with and without good format to use in the loop later (different dateTime format)
  filenames <- data$Filename
  filenames <- data$Filename[-c(4,5,13,20,27,38,41,44,46,48,49,50,61,63,64,66,67,72,80,82,83,84,85,
                                96,110,111,112:119,121:126)]
  filenames_format <- data$Filename[c(4,5,13,20,27,38,41,44,46,48,49,50,61,63,64,66,67,72,80,82,83,84,85,
                                      96,110,111,112:119,121:126)]

##END OF identify stations with data from 2000 to 2020------------------------------------------------------------------------

  
  
##Identify Maximum rainfall for each stations during the 20 years period------------------------------------------------------

  #Make sure that all the files in the QC folder have the same columns names (ecohydrology)
  #files <- list.files("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC")
  #files <- files[c(15:172,276:283)]
  #files <- gsub(files, pattern=".csv$", replacement="")
  #for(i in 1:length(files)){
    #rf <- read.csv(paste("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/",files[i], ".csv", sep = ""), stringsAsFactors = F)
    #names(rf)[1] <- "DateTime"
    #names(rf)[2] <- "RF_mm"
    #write.csv(rf, file = paste("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/", files[i], ".csv", sep = ""), row.names = F)
  #}
  
  #Loop to import files data and get the maximum hourly rainfall for each stations except stations with different format (No more issue with files format)
  v_max <- c()
  v_ID <- c()
  v_date <- c()
  for(i in 1:length(filenames)){
    rf <- read.csv(paste("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/", filenames[i], ".csv", sep = ""), stringsAsFactors = F)
    rf <- rf[c(which(rf$DateTime == "2000-01-01 00:00:00"):which(rf$DateTime == "2019-05-06 23:00:00")),]
    max_rf <- max(rf$RF_mm, na.rm = T)
    v_max <- append(v_max, max_rf)
    ID <- filenames[i]
    v_ID <- append(v_ID, ID)
    date <- rf$DateTime[which(rf$RF_mm==max(rf$RF_mm,na.rm = T))]
    v_date <- append(v_date, date)
  }

  #Create dataframe that will be used on arcgis pro later. Need to add the stations with different format
  # as well as lat/long from the metadata)
  df_intensity <- data.frame(Filename = v_ID, max_rf_mm = v_max, Datetime = v_date)
  
  #Loop to get max hourly rainfall for the stations with different dateTime format
  #v_max_format <- c()
  #v_ID_format <- c()
  #v_date_format <- c()
  #for(i in 1:length(filenames_format)){
    #rf <- read.csv(paste("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/", filenames_format[i], ".csv", sep = ""), stringsAsFactors = F)
    #rf$DateTime <- as.POSIXct(rf$DateTime, format = "%m/%d/%Y %H:%M" )
    #rf <- rf[c(which(rf$DateTime == "2000-01-01 00:00:00"):which(rf$DateTime == "2019-05-06 23:00:00")),]
    #max_rf <- max(rf$RF_mm, na.rm = T)
    #v_max_format <- append(v_max_format, max_rf)
    #ID_format <- filenames_format[i]
    #v_ID_format <- append(v_ID_format, ID_format)
    #date_format <- rf$DateTime[which(rf$RF_mm==max(rf$RF_mm,na.rm = T))]
    #v_date_format <- append(v_date_format, date_format)
  #}
  v_date_format <- v_date_format[-27] #two dates for 1 max. Removed the october and keep the december
  #Dataframe for the different format station. Will be merged to the previous one
  df_intensity_format <- data.frame(Filename = v_ID_format, max_rf_mm = v_max_format, Datetime = v_date_format)
  
  #Merge the two dataframes together
  df_intensity_format$Datetime <- as.character(df_intensity_format$Datetime)
  df_intensity <- rbind(df_intensity, df_intensity_format)
  
  #Add lat/long, start/end dates, and months of the data into the table from the metadata
  df_intensity <- merge(df_intensity, lab_metadata, all.x = T, by = "Filename")
  df_intensity <- df_intensity[,c(3,4,1,2,5:20)]
  df_intensity$month <- substr(df_intensity$Datetime,6,7)
  df_intensity$month <- as.numeric(df_intensity$month)
  df_intensity$month <- month.abb[df_intensity$month]
  df_intensity <- df_intensity[,c(2,3,5,4,1,21,6:20)]
  
  #Export table
  write.csv(df_intensity, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Table/rainfall_intensity_map.csv", row.names = F)
  

##END OF identify Maximum rainfall for each stations during the 20 years period-----------------------------------------



############################SAME BUT NEW DEFINITION#######################################################


##Identify stations with data from 1990 to 2020 having at least 20 years of data and less than 20% of NA------------------------------------------------------------------------------
  
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
  
  #Empty vector for loop
  v_NA <- c()
  v_ID <- c()
  
  #Loop for NA percentage
  for (i in 1:length(filenames)){
    
    #Read file
    rf_hourly <- read.csv(paste("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/",filenames[i], ".csv", sep = ""), stringsAsFactors = F)
    
    #DateTime format
    rf_hourly$DateTime <- as.POSIXct(rf_hourly$DateTime, format = "%Y-%m-%d %H:%M")
    
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
  
##END OF Identify stations with data from 1990 to 2020 having at least 20 years of data and less than 20% of NA------------------------------------------------------------------------
  

  
##Identify Maximum rainfall for each stations during the 20 years period------------------------------------------------------
  
  #Loop to import files data and get the maximum hourly rainfall for each stations
  v_max <- c()
  v_ID <- c()
  v_date <- c()
  filenames <- df_NA$Filename
  
  for(i in 1:length(filenames)){
    rf <- read.csv(paste("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/", filenames[i], ".csv", sep = ""), stringsAsFactors = F)
    rf$DateTime <- as.POSIXct(rf$DateTime, format = "%Y-%m-%d %H:%M")
    rf <- rf[c(which(rf$DateTime >= "1990-01-01 00:00:00")):length(rf$DateTime),]
    max_rf <- max(rf$RF_mm, na.rm = T)
    v_max <- append(v_max, max_rf)
    ID <- filenames[i]
    v_ID <- append(v_ID, ID)
    date <- rf$DateTime[which(rf$RF_mm==max(rf$RF_mm,na.rm = T))]
    v_date <- append(v_date, date)
  }
  
  #Create dataframe 
  df_intensity <- data.frame(Filename = v_ID, max_rf_mm = v_max, Datetime = v_date)

  #Add lat/long, start/end dates, and months of the data into the table from the metadata
  df_intensity <- merge(df_intensity, lab_metadata, all.x = T, by = "Filename")
  df_intensity <- df_intensity[,c(4,5,1,2,3,6:22)]
  df_intensity$month <- substr(df_intensity$Datetime,6,7)
  df_intensity$month <- as.numeric(df_intensity$month)
  df_intensity$month <- month.abb[df_intensity$month]
  df_intensity <- df_intensity[,c(1,2,3,4,5,23,6:22)]
  
  #Export table
  write.csv(df_intensity, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Table/rainfall_intensity_map2.csv", row.names = F)

##END OF identify Maximum rainfall for each stations during the 20 years period------------------------------------------------------  


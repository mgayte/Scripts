#############################################
#                                           #
#                                           #
#          Sum from 7 to 12 hours           #
#                                           #
#                                           #
#############################################

#Load library
library(dplyr)
library(naniar)
library(data.table)
library(zoo)
library(lubridate)
library(dataRetrieval)


##Organize and create dataframe for 7,...,12 hours sum---------------------------------------------------
  
  #Read USGS Kahana station file
  USGS <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Rainfall_data/Results/Hourly_to_Daily_data/Hourly_data_QC/USGS_uv213215157552800.csv", header = T)

  #Keep only DateTime and corrected value columns. Make DateTime column in a datetime format
  USGS <- USGS[, c(1,4)]
  USGS$DateTime <- as.POSIXct(USGS$DateTime, format = "%Y-%m-%d %H:%M")
  
  #Create 7 hour file
  df_RF_7hr <- add_row(.data = USGS, RF_mm = c(NA, NA, NA, NA, NA, NA), 
                       DateTime = c(USGS$DateTime[1] - 6*3600,
                                    USGS$DateTime[1] - 5*3600, 
                                    USGS$DateTime[1]- 4*3600, 
                                    USGS$DateTime[1]- 3*3600,
                                    USGS$DateTime[1] - 2*3600,
                                    USGS$DateTime[1] - 3600), .before = 1)
  
  sum_7hr <- rollapply(df_RF_7hr$RF_mm, 7, sum, by = 1, na.rm = T) #Calculate sum of 7 hours rainfall
  USGS_hrs <- cbind(USGS, sum_7hr) #Bind sum to rainfall dataset
  
  #Create 8 hour file
  df_RF_8hr <- add_row(.data = USGS, RF_mm = c(NA, NA, NA, NA, NA, NA, NA), 
                       DateTime = c(USGS$DateTime[1] - 7*3600,
                                    USGS$DateTime[1] - 6*3600,
                                    USGS$DateTime[1] - 5*3600, 
                                    USGS$DateTime[1]- 4*3600, 
                                    USGS$DateTime[1]- 3*3600,
                                    USGS$DateTime[1] - 2*3600,
                                    USGS$DateTime[1] - 3600), .before = 1)
  
  sum_8hr <- rollapply(df_RF_8hr$RF_mm, 8, sum, by = 1, na.rm = T) #Calculate sum of 8 hours rainfall
  USGS_hrs <- cbind(USGS_hrs, sum_8hr) #Bind sum to rainfall dataset
  
  #Create 9 hour file
  df_RF_9hr <- add_row(.data = USGS, RF_mm = c(NA, NA, NA, NA, NA, NA, NA, NA), 
                       DateTime = c(USGS$DateTime[1] - 8*3600,
                                    USGS$DateTime[1] - 7*3600,
                                    USGS$DateTime[1] - 6*3600,
                                    USGS$DateTime[1] - 5*3600, 
                                    USGS$DateTime[1]- 4*3600, 
                                    USGS$DateTime[1]- 3*3600,
                                    USGS$DateTime[1] - 2*3600,
                                    USGS$DateTime[1] - 3600), .before = 1)
  
  sum_9hr <- rollapply(df_RF_9hr$RF_mm, 9, sum, by = 1, na.rm = T) #Calculate sum of 9 hours rainfall
  USGS_hrs <- cbind(USGS_hrs, sum_9hr) #Bind sum to rainfall dataset
  
  #Create 10 hour file
  df_RF_10hr <- add_row(.data = USGS, RF_mm = c(NA, NA, NA, NA, NA, NA, NA, NA, NA), 
                       DateTime = c(USGS$DateTime[1] - 9*3600,
                                    USGS$DateTime[1] - 8*3600,
                                    USGS$DateTime[1] - 7*3600,
                                    USGS$DateTime[1] - 6*3600,
                                    USGS$DateTime[1] - 5*3600, 
                                    USGS$DateTime[1]- 4*3600, 
                                    USGS$DateTime[1]- 3*3600,
                                    USGS$DateTime[1] - 2*3600,
                                    USGS$DateTime[1] - 3600), .before = 1)
  
  sum_10hr <- rollapply(df_RF_10hr$RF_mm, 10, sum, by = 1, na.rm = T) #Calculate sum of 10 hours rainfall
  USGS_hrs <- cbind(USGS_hrs, sum_10hr) #Bind sum to rainfall dataset
  
  #Create 11 hour file
  df_RF_11hr <- add_row(.data = USGS, RF_mm = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
                        DateTime = c(USGS$DateTime[1] - 10*3600,
                                     USGS$DateTime[1] - 9*3600,
                                     USGS$DateTime[1] - 8*3600,
                                     USGS$DateTime[1] - 7*3600,
                                     USGS$DateTime[1] - 6*3600,
                                     USGS$DateTime[1] - 5*3600, 
                                     USGS$DateTime[1]- 4*3600, 
                                     USGS$DateTime[1]- 3*3600,
                                     USGS$DateTime[1] - 2*3600,
                                     USGS$DateTime[1] - 3600), .before = 1)
  
  sum_11hr <- rollapply(df_RF_11hr$RF_mm, 11, sum, by = 1, na.rm = T) #Calculate sum of 11 hours rainfall
  USGS_hrs <- cbind(USGS_hrs, sum_11hr) #Bind sum to rainfall dataset
  
  #Create 12 hour file
  df_RF_12hr <- add_row(.data = USGS, RF_mm = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), 
                        DateTime = c(USGS$DateTime[1] - 11*3600,
                                     USGS$DateTime[1] - 10*3600,
                                     USGS$DateTime[1] - 9*3600,
                                     USGS$DateTime[1] - 8*3600,
                                     USGS$DateTime[1] - 7*3600,
                                     USGS$DateTime[1] - 6*3600,
                                     USGS$DateTime[1] - 5*3600, 
                                     USGS$DateTime[1]- 4*3600, 
                                     USGS$DateTime[1]- 3*3600,
                                     USGS$DateTime[1] - 2*3600,
                                     USGS$DateTime[1] - 3600), .before = 1)
  
  sum_12hr <- rollapply(df_RF_12hr$RF_mm, 12, sum, by = 1, na.rm = T) #Calculate sum of 12 hours rainfall
  USGS_hrs <- cbind(USGS_hrs, sum_12hr) #Bind sum to rainfall dataset
  
##END OF Organize and create dataframe for 7,...,12 hours sum---------------------------------------------------

  
  
##Add water year to the USGS_RF data frame----------------------------------------------------------------------
  USGS_hrs$Water_Year <- ifelse(month(USGS_hrs$DateTime) < 10, USGS_hrs$DateTime, USGS_hrs$DateTime + years(1))
  USGS_hrs$Water_Year <- as.POSIXct(USGS_hrs$Water_Year, origin="1970-01-01", tz = "HST")
  USGS_hrs$Water_Year <- substr(USGS_hrs$Water_Year,1,4)
  
##END OF add water year to the USGS_RF data frame---------------------------------------------------------------
  
  
##Identify date of maximum sum amount of rainfall per year and per day from 7 to 12hrs--------------------------
  
  #Max RF per year for sum 7hr
  max_RF_year_sum_7hr <- USGS_hrs %>%
    group_by(Water_Year) %>%
    summarise(max_RF = max(sum_7hr, na.rm = T))
  
  ##Max RF per day for sum 7hr with extra column year
  max_RF_per_day_sum_7hr <- USGS_hrs %>%
    mutate(Date = substr(DateTime,1,10)) %>%
    group_by(Date) %>%
    summarise(max_RF = max(sum_7hr, na.rm = T))
  max_RF_per_day_sum_7hr$Date <- as.POSIXct(max_RF_per_day_sum_7hr$Date, origin="1970-01-01", tz = "HST")
  max_RF_per_day_sum_7hr$Water_Year <- ifelse(month(max_RF_per_day_sum_7hr$Date) < 10, 
                              max_RF_per_day_sum_7hr$Date, max_RF_per_day_sum_7hr$Date + years(1))
  max_RF_per_day_sum_7hr$Water_Year <- as.POSIXct(max_RF_per_day_sum_7hr$Water_Year, 
                                                  origin="1970-01-01", tz = "HST")
  max_RF_per_day_sum_7hr$Water_Year <- substr(max_RF_per_day_sum_7hr$Water_Year,1,4)
  
  #Max RF per year for sum 8hr
  max_RF_year_sum_8hr <- USGS_hrs %>%
    group_by(Water_Year) %>%
    summarise(max_RF = max(sum_8hr, na.rm = T))
  
  ##Max RF per day for sum 8hr with extra column year
  max_RF_per_day_sum_8hr <- USGS_hrs %>%
    mutate(Date = substr(DateTime,1,10)) %>%
    group_by(Date) %>%
    summarise(max_RF = max(sum_8hr, na.rm = T))
  max_RF_per_day_sum_8hr$Date <- as.POSIXct(max_RF_per_day_sum_8hr$Date, origin="1970-01-01", tz = "HST")
  max_RF_per_day_sum_8hr$Water_Year <- ifelse(month(max_RF_per_day_sum_8hr$Date) < 10, 
                                  max_RF_per_day_sum_8hr$Date, max_RF_per_day_sum_8hr$Date + years(1))
  max_RF_per_day_sum_8hr$Water_Year <- as.POSIXct(max_RF_per_day_sum_8hr$Water_Year, 
                                                  origin="1970-01-01", tz = "HST")
  max_RF_per_day_sum_8hr$Water_Year <- substr(max_RF_per_day_sum_8hr$Water_Year,1,4)
  
  #Max RF per year for sum 9hr
  max_RF_year_sum_9hr <- USGS_hrs %>%
    group_by(Water_Year) %>%
    summarise(max_RF = max(sum_9hr, na.rm = T))
  
  ##Max RF per day for sum 9hr with extra column year
  max_RF_per_day_sum_9hr <- USGS_hrs %>%
    mutate(Date = substr(DateTime,1,10)) %>%
    group_by(Date) %>%
    summarise(max_RF = max(sum_9hr, na.rm = T))
  max_RF_per_day_sum_9hr$Date <- as.POSIXct(max_RF_per_day_sum_9hr$Date, origin="1970-01-01", tz = "HST")
  max_RF_per_day_sum_9hr$Water_Year <- ifelse(month(max_RF_per_day_sum_9hr$Date) < 10, 
                                              max_RF_per_day_sum_9hr$Date, max_RF_per_day_sum_9hr$Date + years(1))
  max_RF_per_day_sum_9hr$Water_Year <- as.POSIXct(max_RF_per_day_sum_9hr$Water_Year, 
                                                  origin="1970-01-01", tz = "HST")
  max_RF_per_day_sum_9hr$Water_Year <- substr(max_RF_per_day_sum_9hr$Water_Year,1,4)
  
  #Max RF per year for sum 10hr
  max_RF_year_sum_10hr <- USGS_hrs %>%
    group_by(Water_Year) %>%
    summarise(max_RF = max(sum_10hr, na.rm = T))
  
  ##Max RF per day for sum 10hr with extra column year
  max_RF_per_day_sum_10hr <- USGS_hrs %>%
    mutate(Date = substr(DateTime,1,10)) %>%
    group_by(Date) %>%
    summarise(max_RF = max(sum_10hr, na.rm = T))
  max_RF_per_day_sum_10hr$Date <- as.POSIXct(max_RF_per_day_sum_10hr$Date, origin="1970-01-01", tz = "HST")
  max_RF_per_day_sum_10hr$Water_Year <- ifelse(month(max_RF_per_day_sum_10hr$Date) < 10, 
                                              max_RF_per_day_sum_10hr$Date, max_RF_per_day_sum_10hr$Date + years(1))
  max_RF_per_day_sum_10hr$Water_Year <- as.POSIXct(max_RF_per_day_sum_10hr$Water_Year, 
                                                  origin="1970-01-01", tz = "HST")
  max_RF_per_day_sum_10hr$Water_Year <- substr(max_RF_per_day_sum_10hr$Water_Year,1,4)
  
  #Max RF per year for sum 11hr
  max_RF_year_sum_11hr <- USGS_hrs %>%
    group_by(Water_Year) %>%
    summarise(max_RF = max(sum_11hr, na.rm = T))
  
  ##Max RF per day for sum 8hr with extra column year
  max_RF_per_day_sum_11hr <- USGS_hrs %>%
    mutate(Date = substr(DateTime,1,10)) %>%
    group_by(Date) %>%
    summarise(max_RF = max(sum_11hr, na.rm = T))
  max_RF_per_day_sum_11hr$Date <- as.POSIXct(max_RF_per_day_sum_11hr$Date, origin="1970-01-01", tz = "HST")
  max_RF_per_day_sum_11hr$Water_Year <- ifelse(month(max_RF_per_day_sum_11hr$Date) < 10, 
                                              max_RF_per_day_sum_11hr$Date, max_RF_per_day_sum_11hr$Date + years(1))
  max_RF_per_day_sum_11hr$Water_Year <- as.POSIXct(max_RF_per_day_sum_11hr$Water_Year, 
                                                  origin="1970-01-01", tz = "HST")
  max_RF_per_day_sum_11hr$Water_Year <- substr(max_RF_per_day_sum_11hr$Water_Year,1,4)
  
  #Max RF per year for sum 8hr
  max_RF_year_sum_12hr <- USGS_hrs %>%
    group_by(Water_Year) %>%
    summarise(max_RF = max(sum_12hr, na.rm = T))
  
  ##Max RF per day for sum 8hr with extra column year
  max_RF_per_day_sum_12hr <- USGS_hrs %>%
    mutate(Date = substr(DateTime,1,10)) %>%
    group_by(Date) %>%
    summarise(max_RF = max(sum_12hr, na.rm = T))
  max_RF_per_day_sum_12hr$Date <- as.POSIXct(max_RF_per_day_sum_12hr$Date, origin="1970-01-01", tz = "HST")
  max_RF_per_day_sum_12hr$Water_Year <- ifelse(month(max_RF_per_day_sum_12hr$Date) < 10, 
                                              max_RF_per_day_sum_12hr$Date, max_RF_per_day_sum_12hr$Date + years(1))
  max_RF_per_day_sum_12hr$Water_Year <- as.POSIXct(max_RF_per_day_sum_12hr$Water_Year, 
                                                  origin="1970-01-01", tz = "HST")
  max_RF_per_day_sum_12hr$Water_Year <- substr(max_RF_per_day_sum_12hr$Water_Year,1,4)
  
  #Vectors that will be used in the following loops 
  years <- unique(max_RF_year_sum_7hr$Water_Year)
  max_RF_sum7hr <- max_RF_year_sum_7hr$max_RF
  max_RF_sum8hr <- max_RF_year_sum_8hr$max_RF
  max_RF_sum9hr <- max_RF_year_sum_9hr$max_RF
  max_RF_sum10hr <- max_RF_year_sum_10hr$max_RF
  max_RF_sum11hr <- max_RF_year_sum_11hr$max_RF
  max_RF_sum12hr <- max_RF_year_sum_12hr$max_RF
  v_index_sum7hr <- c()
  v_index_sum8hr <- c()
  v_index_sum9hr <- c()
  v_index_sum10hr <- c()
  v_index_sum11hr <- c()
  v_index_sum12hr <- c()
  v_date_max_RF_sum7hr <- c()
  v_date_max_RF_sum8hr <- c()
  v_date_max_RF_sum9hr <- c()
  v_date_max_RF_sum10hr <- c()
  v_date_max_RF_sum11hr <- c()
  v_date_max_RF_sum12hr <- c()
  v_max_Rf_sum7hr <- c()
  v_max_Rf_sum8hr <- c()
  v_max_Rf_sum9hr <- c()
  v_max_Rf_sum10hr <- c()
  v_max_Rf_sum11hr <- c()
  v_max_Rf_sum12hr <- c()
  
  #Loop to get position of max_RF in the max_RF_per_day dataframe
  for (i in 1:length(years)){
    #sum 7hr
    index_sum_7hr <- which(max_RF_per_day_sum_7hr$max_RF == max_RF_sum7hr[i] & max_RF_per_day_sum_7hr$Water_Year == years[i])
    v_index_sum7hr <- append(v_index_sum7hr, index_sum_7hr)
    
    #sum 8hr
    index_sum_8hr <- which(max_RF_per_day_sum_8hr$max_RF == max_RF_sum8hr[i] & max_RF_per_day_sum_8hr$Water_Year == years[i])
    v_index_sum8hr <- append(v_index_sum8hr, index_sum_8hr)
    
    #sum 9hr
    index_sum_9hr <- which(max_RF_per_day_sum_9hr$max_RF == max_RF_sum9hr[i] & max_RF_per_day_sum_9hr$Water_Year == years[i])
    v_index_sum9hr <- append(v_index_sum9hr, index_sum_9hr)
    
    #sum 10hr
    index_sum_10hr <- which(max_RF_per_day_sum_10hr$max_RF == max_RF_sum10hr[i] & max_RF_per_day_sum_10hr$Water_Year == years[i])
    v_index_sum10hr <- append(v_index_sum10hr, index_sum_10hr)
    
    #sum 11hr
    index_sum_11hr <- which(max_RF_per_day_sum_11hr$max_RF == max_RF_sum11hr[i] & max_RF_per_day_sum_11hr$Water_Year == years[i])
    v_index_sum11hr <- append(v_index_sum11hr, index_sum_11hr)
    
    #sum 12hr
    index_sum_12hr <- which(max_RF_per_day_sum_12hr$max_RF == max_RF_sum12hr[i] & max_RF_per_day_sum_12hr$Water_Year == years[i])
    v_index_sum12hr <- append(v_index_sum12hr, index_sum_12hr)
  }
  
  #Remove in the index the extra row number for 9,10,11, and 12 hours. This is due to a same max in the
  #year 2010. The two values are next to each other and I keep the 06/04/2010
  v_index_sum9hr <- v_index_sum9hr[-3]
  v_index_sum10hr <- v_index_sum10hr[-3]
  v_index_sum11hr <- v_index_sum11hr[-3]
  v_index_sum12hr <- v_index_sum12hr[-3]
  
  #Loop to get values of max_RF for avg and sum and their associated dates
  for (n in 1:length(years)){ 
    #sum 7hr
    date_max_sum_7hr_RF <- max_RF_per_day_sum_7hr$Date[v_index_sum7hr[n]]
    v_date_max_RF_sum7hr <- append(v_date_max_RF_sum7hr, date_max_sum_7hr_RF)
    max_sum_7hr_Rf <- max_RF_per_day_sum_7hr$max_RF[v_index_sum7hr[n]]
    v_max_Rf_sum7hr <- append(v_max_Rf_sum7hr,max_sum_7hr_Rf)
    
    #sum 8hr
    date_max_sum_8hr_RF <- max_RF_per_day_sum_8hr$Date[v_index_sum8hr[n]]
    v_date_max_RF_sum8hr <- append(v_date_max_RF_sum8hr, date_max_sum_8hr_RF)
    max_sum_8hr_Rf <- max_RF_per_day_sum_8hr$max_RF[v_index_sum8hr[n]]
    v_max_Rf_sum8hr <- append(v_max_Rf_sum8hr,max_sum_8hr_Rf)
    
    #sum 9hr
    date_max_sum_9hr_RF <- max_RF_per_day_sum_9hr$Date[v_index_sum9hr[n]]
    v_date_max_RF_sum9hr <- append(v_date_max_RF_sum9hr, date_max_sum_9hr_RF)
    max_sum_9hr_Rf <- max_RF_per_day_sum_9hr$max_RF[v_index_sum9hr[n]]
    v_max_Rf_sum9hr <- append(v_max_Rf_sum9hr,max_sum_9hr_Rf)
    
    #sum 10hr
    date_max_sum_10hr_RF <- max_RF_per_day_sum_10hr$Date[v_index_sum10hr[n]]
    v_date_max_RF_sum10hr <- append(v_date_max_RF_sum10hr, date_max_sum_10hr_RF)
    max_sum_10hr_Rf <- max_RF_per_day_sum_10hr$max_RF[v_index_sum10hr[n]]
    v_max_Rf_sum10hr <- append(v_max_Rf_sum10hr,max_sum_10hr_Rf)
    
    #sum 11hr
    date_max_sum_11hr_RF <- max_RF_per_day_sum_11hr$Date[v_index_sum11hr[n]]
    v_date_max_RF_sum11hr <- append(v_date_max_RF_sum11hr, date_max_sum_11hr_RF)
    max_sum_11hr_Rf <- max_RF_per_day_sum_11hr$max_RF[v_index_sum11hr[n]]
    v_max_Rf_sum11hr <- append(v_max_Rf_sum11hr,max_sum_11hr_Rf)
    
    #sum 12hr
    date_max_sum_12hr_RF <- max_RF_per_day_sum_12hr$Date[v_index_sum12hr[n]]
    v_date_max_RF_sum12hr <- append(v_date_max_RF_sum12hr, date_max_sum_12hr_RF)
    max_sum_12hr_Rf <- max_RF_per_day_sum_12hr$max_RF[v_index_sum12hr[n]]
    v_max_Rf_sum12hr <- append(v_max_Rf_sum12hr,max_sum_12hr_Rf)
  }
  
##END OF identify date of maximum sum amount of rainfall per year and per day from 7 to 12hrs------------  
  
  
##Create dataframe with the results and add water year column--------------------------------------------
  
  df_max_RF <- data.frame(date_rainMax_sum7hr = v_date_max_RF_sum7hr, RF_max_sum7hr = v_max_Rf_sum7hr,
                          date_rainMax_sum8hr = v_date_max_RF_sum8hr, RF_max_sum8hr = v_max_Rf_sum8hr,
                          date_rainMax_sum9hr = v_date_max_RF_sum9hr, RF_max_sum9hr = v_max_Rf_sum9hr,
                          date_rainMax_sum10hr = v_date_max_RF_sum10hr, RF_max_sum10hr = v_max_Rf_sum10hr,
                          date_rainMax_sum11hr = v_date_max_RF_sum11hr, RF_max_sum11hr = v_max_Rf_sum11hr,
                          date_rainMax_sum12hr = v_date_max_RF_sum12hr, RF_max_sum12hr = v_max_Rf_sum12hr)
  
  df_max_RF$Water_Year <- df_max_RF$date_rainMax_sum7hr 
  df_max_RF$Water_Year <- ifelse(month(df_max_RF$Water_Year) < 10, df_max_RF$Water_Year, df_max_RF$Water_Year + years(1))
  df_max_RF$Water_Year <- as.POSIXct(df_max_RF$Water_Year, origin="1970-01-01", tz = "HST")
  df_max_RF$Water_Year <- substr(df_max_RF$Water_Year,1,4)
  df_max_RF <- df_max_RF[,c(13,1:12)]
  
##END OF create dataframe with the results and add water year column-------------------------------------
  
 

##Load peakflow data and merge to dataframe
  
  peakdata <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/new_max_avg_sum_USGS_uv213215157552800_16296500.csv", header = T)
  peakdata <- peakdata[,c(1,10:12)]
  RF_peakflow <- merge(df_max_RF, peakdata, by = "Water_Year", all.x = T)
  RF_peakflow <- RF_peakflow[-11,]

## END OF load peakflow data and merge to dataframe------------------------------------------------------


  
##Calculate difference in days between peakflow and max_RF sum-------------------------------------------
  
  #First change format of peakflow date from factor to POSIXct
  RF_peakflow$peak_dt <- as.character(RF_peakflow$peak_dt)
  RF_peakflow$peak_dt <- as.POSIXct(RF_peakflow$peak_dt, format = "%m/%d/%Y")
  
  #Difference in days
  RF_peakflow$Different_day_sum7hr <- difftime(RF_peakflow$date_rainMax_sum7hr, RF_peakflow$peak_dt, units = "days")
  RF_peakflow$Different_day_sum7hr <- round(RF_peakflow$Different_day_sum7hr,0)
  RF_peakflow$Different_day_sum8hr <- difftime(RF_peakflow$date_rainMax_sum8hr, RF_peakflow$peak_dt, units = "days")
  RF_peakflow$Different_day_sum8hr <- round(RF_peakflow$Different_day_sum8hr,0)
  RF_peakflow$Different_day_sum9hr <- difftime(RF_peakflow$date_rainMax_sum9hr, RF_peakflow$peak_dt, units = "days")
  RF_peakflow$Different_day_sum9hr <- round(RF_peakflow$Different_day_sum9hr,0)
  RF_peakflow$Different_day_sum10hr <- difftime(RF_peakflow$date_rainMax_sum10hr, RF_peakflow$peak_dt, units = "days")
  RF_peakflow$Different_day_sum10hr <- round(RF_peakflow$Different_day_sum10hr,0)
  RF_peakflow$Different_day_sum11hr <- difftime(RF_peakflow$date_rainMax_sum11hr, RF_peakflow$peak_dt, units = "days")
  RF_peakflow$Different_day_sum11hr <- round(RF_peakflow$Different_day_sum11hr,0)
  RF_peakflow$Different_day_sum12hr <- difftime(RF_peakflow$date_rainMax_sum12hr, RF_peakflow$peak_dt, units = "days")
  RF_peakflow$Different_day_sum12hr <- round(RF_peakflow$Different_day_sum12hr,0)
  
  #Export table
  write.csv(RF_peakflow, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/sum_7_to_12hrs_16296500.csv", row.names = F)
  
  
  
  
  
  
  
  
  


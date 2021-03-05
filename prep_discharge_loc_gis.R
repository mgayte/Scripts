#############################################
#                                           #
#           Prep discharge data             #
#                  for                      #
#               map arcgis                  #
#                                           #
#############################################

#Read discharge table and delete rainfall stations
discharge_table <- read.csv("C:/Users/mgayt/Documents/Rainfall_peakflow_project/Discharge_information/Discharge_stations.csv", header = T)
discharge_table <- discharge_table[-c(162:189),]

##Get stations with only data from 2000-01-01 to 2019-12-31 and with at least three years of data---------------

  #Change format of start end end dates to date format
  discharge_table$start_date <- as.Date(discharge_table$start_date, format = "%m/%d/%Y")
  discharge_table$end_date <- as.Date(discharge_table$end_date, format = "%m/%d/%Y")

  #Stations with data for three years at least
  discharge_table <- discharge_table[discharge_table$length_date >= 365*3,]
  
  #Stations with end date later than 12-31-2003 (at least three years after 2000)
  discharge_table <- discharge_table[!is.na(discharge_table$length_date),]
  discharge_table <- discharge_table[discharge_table$end_date >= "2003-12-31",]
  
##END OF get stations with only data from 2000-01-01 to 2019-12-31 and with at least three years of data----------------------- 

  
  
#Export the table to use in gis
  write.csv(discharge_table, "C:/Users/mgayt/Documents/Rainfall_peakflow_project/Discharge_information/2000_discharge_stations.csv", row.names = F)
  










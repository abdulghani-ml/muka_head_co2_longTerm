#-----------------------------------------------------------------------#
#
# Title:  Merging Sat Data with Sat Salinity Data
#
# Authors: Yusri Yusup, Ph.D.
#
# Affiliation:  Environmental Technology, School of Industrial Technology,
#               Universiti Sains Malaysia
#
# Datasets used:
#   1. df_salinity
#   2. df_sat
#
# Dataset period:
# No. of rows: 
# No. of columns:
# Objective: To import and process data
#
#
#-----------------------------------------------------------------------#

#### ADJUSTING THE TIME STAMP OF SALINITY DATA ####
salinity_temp <- timeAverage(df_salinity, avg.time = "hour")

#### ADJUSTING THE TIME STAMP OF SAT DATA ###
df_sat_temp <- timeAverage(df_sat,avg.time = "1 hour")

date <- df_sat_temp$date

date <- format(round(date, units="hours"), format="%Y-%m-%d %H:%M:%S")

date <- as.POSIXct(date,"%Y-%m-%d %H:%M:%S", tz='Asia/Kuala_Lumpur')

df_sat_temp <- cbind(date,df_sat_temp)

df_sat_temp <- df_sat_temp[,-2]

df_sat_temp <- timeAverage(df_sat_temp,avg.time = "1 hour")

#### MERGE THE SALINITY DATA WITH THE SAT DATA ####
df_salinity_sat_temp <- merge(salinity_temp, df_sat_temp,by=c('date'))

df_merged_salinity_sat <- timeAverage(df_salinity_sat_temp, avg.time='month')

#### END ####
rm(date, df_sat_temp, salinity_temp, df_salinity_sat_temp)


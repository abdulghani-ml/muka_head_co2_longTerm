#-----------------------------------------------------------------------#
#
# Title:  The Outlier-Removed Data Import and Process Script
#
# Authors: Yusri Yusup, Ph.D.
#
# Affiliation:  Environmental Technology, School of Industrial Technology,
#               Universiti Sains Malaysia
#
# Datasets used:
#   1. merged_data.csv (the outlier-removed dataset by Ehsan).
#
# Dataset period:
# No. of rows: 
# No. of columns:
# Objective: To import and process data
#
#
#-----------------------------------------------------------------------#

#### FILTERED DATA IMPORT #####

df_filtered <- read.csv('data/station/merged_data.csv')
df_filtered$datetime <- as.character(df_filtered$datetime)
date <- strptime(df_filtered$datetime,
                 format = "%d/%m/%Y %H:%M",
                 tz = "Asia/Kuala_Lumpur")
df_filtered <- cbind(date, df_filtered)
colnames(df_filtered)[3] <- 'TS_ema'
colnames(df_filtered)[4] <- 'TA_ema'
colnames(df_filtered)[5] <- 'RH_ema'
df_filtered <- df_filtered[,-c(2)]

#### END ####
rm(date)
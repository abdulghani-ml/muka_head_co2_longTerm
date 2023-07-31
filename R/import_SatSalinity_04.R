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
#   1. salinity.csv (Data retrieved by Fikri).
#
# Dataset period:
# No. of rows: 
# No. of columns:
# Objective: To import and process data
#
#
#-----------------------------------------------------------------------#
#### PACKAGES ####
library(openair)

#### SALINITY DATA IMPORT ####
df_salinity <- read.csv('data/station/salinity.csv',sep = ',')

df_salinity$date <- as.POSIXct(df_salinity$date,"%d/%m/%Y %H:%M",tz='Asia/Kuala_Lumpur')


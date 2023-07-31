#-----------------------------------------------------------------------#
#
# Title:  The Satellite Data Import and Process Script
#
# Authors: Yusri Yusup, Ph.D.
#
# Affiliation:  Environmental Technology, School of Industrial Technology,
#               Universiti Sains Malaysia
#
# Datasets used:
#   1. MODIS-Aqua
#   2. EUMETSAT Wave
#   
#
# Dataset period:
# No. of rows: 
# No. of columns:
# Objective: To import and process data
#
#
#-----------------------------------------------------------------------#

#### PACKAGES #########################################

require(openair)    # For time averaging


#### SATELLITE DATA IMPORT ####
##### Import sat data ###############

# Biological Data (MODIS-Aqua)
chl <- read.csv('data/satellite/5year_chlor_Aqua.csv')
sst <- read.csv('data/satellite/5year_SST_Aqua.csv')
poc <- read.csv('data/satellite/5year_POC_Aqua.csv')
pic <- read.csv('data/satellite/5year_PIC_Aqua.csv')
par <- read.csv('data/satellite/5year_PAR_Aqua.csv')

# # OPTIONAL Wave Data (EUMETSAT)
# wave <-read.csv('data/station/DWave.csv')
# date <- as.POSIXct(wave$time1, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
# wave <- cbind(date,wave)
# wave <- wave[,-5]
# wave_30 <- timeAverage(wave,avg.time = "30 min")


##### Merge variables sat data #####

x1<- merge(chl,sst)
x2<- merge(poc,pic)
x3<- merge(x1,x2)
df_sat<- merge(x3,par)
rm(chl,par,pic,poc,sst,x1,x2,x3)

# Remove letters in date

df_sat$DATE <- gsub("T"," ", df_sat$DATE)           # This is to remove the letter T
df_sat$DATE <- strptime(df_sat$DATE, 
                        format = "%Y-%m-%d %H:%M:%OS", 
                        tz = "Europe/London")

df_sat$DATE <- as.POSIXct.POSIXlt(df_sat$DATE)    #Change to POSIXct format

df_sat<- df_sat[which(df_sat$DATE >= as.POSIXct(as.Date("2016-01-01")) 
                      & df_sat$DATE <= as.POSIXct(as.Date("2020-12-31"))),]

date <- df_sat$DATE
date <- strptime(date,format="%Y-%m-%d %H:%M:%S", tz='Asia/Kuala_Lumpur')
df_sat <- cbind(date,df_sat)
df_sat <- df_sat[,c(-2)]
rm(date)

# Changing all the '-32767.0' or '-32767' (missing data) to NA
for (i in 4:8){                                   #This means column number 4 to coloumn number 8
  df_sat[i][df_sat[i] == '-32767' | df_sat[i] == '-32767.0'] <- NA
}
rm(i)

#### END ####
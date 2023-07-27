#-----------------------------------------------------------------------#
#
# Title:  Long-Term Biological and Physical Controls of CO2 Fluxes 
#         in A Tropical Coastal Ocean 
#
# Authors:Abdulghani Swesi,a,c Yusri Yusup,a,b Mardiana Idayu Ahmad,a Haitem M Almdhun,a
#         Ehsan Jolous Jamshidi,a Anis Ibrahim, John Stephen Kayode
#
# Affiliation:  Environmental Technology, School of Industrial Technology,
#               Universiti Sains Malaysia
#
# Datasets used:
#   1. Atmosfera (https://atmosfera.usm.my/)
#   2. MODIS-Aqua
#   3. SOCAT (https://www.socat.info)
#
# Dataset period: 2016-01 to 2017-10 (2 years)
# No. of rows: 
# No. of columns:
# Objective: To combine EC data from Muka Head with Significant 
# Wave Height (swh) data from EUMETSAT, and precipitation data 
# from Wunderground.
#
#-----------------------------------------------------------------------#

#### PACKAGES #########################################
require(openair)    # For time averaging
require(dplyr)
require(lubridate)  # For daily cumulative rain calculation
source('R/tools/tool_convert_magic.R')
source('R/tools/tool_charactersNumeric.R')
source('R/tools/tool_trapezium_intg_2.R')
source('R/tools/tool_trapezium_intg_3.R')
source('R/tools/f2pCO2.R')
source('R/tools/tool_salinity_weiss.R')

#### SATELLITE DATA ANALYSIS ####
##### Import sat data ###############
chl <- read.csv('data/satellite/5year_chlor_Aqua.csv')
sst <- read.csv('data/satellite/5year_SST_Aqua.csv')
poc <- read.csv('data/satellite/5year_POC_Aqua.csv')
pic <- read.csv('data/satellite/5year_PIC_Aqua.csv')
par <- read.csv('data/satellite/5year_PAR_Aqua.csv')

wave <-read.csv('data/station/DWave.csv')
# Wave Data (wave)
date <- as.POSIXct(wave$time1, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
wave <- cbind(date,wave)
wave <- wave[,-5]
wave_30 <- timeAverage(wave,avg.time = "30 min")

##### Merge variables sat data #####
#df1 <- merge(chl,sst,par,poc,pic,by= c('DATE'),all=TRUE)

x1<- merge(chl,sst)
x2<- merge(poc,pic)
x3<- merge(x1,x2)
df_sat<- merge(x3,par)
rm(chl,par,pic,poc,sst,x1,x2,x3)

# Remove letters in date

df_sat$DATE <- gsub("T"," ", df_sat$DATE)         # This is to remove the letter T
df_sat$DATE <- strptime(df_sat$DATE, 
                        format = "%Y-%m-%d %H:%M:%OS", 
                        tz = "Europe/London")
#class (df_sat$DATE)                              #To check the format, make sure is POSIXlt POSIXt
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

##### Averaging sat data #####

# Need to change name DATE to date
colnames(df_sat)[1] <- "date"

#Average data to monthly intervals
df_sat_month <- timeAverage(df_sat, avg.time = "1 month")

#Average data to yearly intervals
df_sat_year <- timeAverage(df_sat, avg.time = "1 year")



#### EC DATA ANALYSIS ####

##### Import EC data ######
df_ec<-read.csv('data/station/MCO-MUKA21_full_output copy.csv')
df_biomet<- read.csv('data/station/biomet data 21.csv')

# Delete unnecessary columns and rows in EC data files
df_ec<- df_ec[-1,]  #remove the 1st row 


# Delete unnecessary columns and rows in biomet data files
df_biomet<- df_biomet[,c(-6,-7)] #remove (DOY,unamed)


#Rename the date
colnames(df_ec)[1] <- "DATE"
colnames(df_biomet)[7] <- "DATE"

df_biomet$DATE <- strptime(df_biomet$DATE, format="%Y-%m-%d %H:%M", tz='Asia/Kuala_Lumpur')
df_ec$DATE <- strptime(df_ec$DATE, format="%Y-%m-%d %H:%M", tz='Asia/Kuala_Lumpur')

##### Merge df with df_bimet ####
df <- merge(df_ec,df_biomet,by= c('DATE'))
DATE <- df$DATE # Save the date to a variable

# Using convert_magic to convert all columns to 'character' first
df <- convert_magic(df[,c(seq(2,ncol(df)))],c(rep('character',times = ncol(df))))

# Changing all the '-9999.0' or '-9999' (missing data) to NA
for (i in 1:length(df)){
  df[i][df[i] == '-9999' | df[i] == '-9999.0' | df[i] == '--'] <- NA
}
rm(i)

# Change all non-factors (or characters) to numeric 
df[,-1] <- charactersNumeric(df[,-1])


# Rebind DATE to df
df <- cbind(DATE, df)

#class(df$DATE)
df$DATE <- as.POSIXlt(df$DATE)

#Change name DATE to date
colnames(df)[1] <- "date"
rm(DATE)

# extracting related variables from raw variables
df_all <- df
df <- data.frame(df$date,df$DOY,df$WS,df$WD,
                 df$FCO2,df$RN_1_1_1,df$RH_1_1_1,df$H2O,
                 df$H,df$LE,df$ZL,df$SH,df$SLE,
                 df$P_RAIN_1_1_1,df$TA_1_1_1,df$ET,
                 df$PPFD_1_1_1,df$USTAR,df$RG_1_1_1,
                 df$TS_1_1_1,df$H_QC,df$LE_QC,df$FCO2_QC,df$TAU_QC,
                 df$CO2,df$PA,df$Correlation)


#Rename variables
colnames(df) <- c("date","DOY","WS","WD",
                  "FCO2","RN","RH","H2O",
                  "H","LE","ZL","SH","SLE",
                  "P_RAIN","TA","ET","PPFD",
                  "USTAR","RG","TS","H_QC","LE_QC","FCO2_QC","TAU_QC",
                  "co2_mole_fraction", "air_pressure","correlation")

# Convert TA, TS from K to Celcius
df$TA<- df$TA - 273.15
df$TS<- df$TS - 273.15

##### Convert FCO2 from micro-mole per second to milli-mole per day ####
FCO2_mmol <- df$FCO2 * 86.4
df <- cbind(df, FCO2_mmol)
rm(FCO2_mmol)

#### DATA FILTERING ####
df$LE[which(df$LE_QC == 2)] <- NA 

df$H[which(df$H_QC == 2)] <- NA 

df$FCO2[which(df$FCO2_QC == 2)] <- NA
#df$FCO2[which(df$FCO2_QC == 1)] <- NA       # Stricter QC

df$FCO2_mmol[which(df$FCO2_QC == 2)] <- NA
#df$FCO2_mmol[which(df$FCO2_QC == 1)] <- NA       # Stricter QC

df$TAU_QC[which(df$TAU_QC == 2)] <- NA

##### Remove FCO2 from land ######

# Note: Did not filter based on WD because insufficient data for gap-filling
# Could be re-added if the focus is on water surfaces.

#df$FCO2[df$WD > 45 & df$WD < 315] <- NA
#df$FCO2_mmol[df$WD > 45 & df$WD < 315] <- NA
#df$co2_mole_fraction[df$WD > 45 & df$WD < 315] <- NA

# Remove FCO2 values that are too high or low. The upper limit is set to 50 because
# of the recommendation of the REddyProc package.
df$FCO2[df$FCO2 > 50 | df$FCO2 < -50] <- NA


##### Remove all improbable values of T #####
df$TA[which(df$TA < 0 | df$TA > 100 )] <- NA
df$TS[which(df$TS < 0 )] <- NA

df$TA[df$TA < 20 | df$TA > 40] <- NA
df$TS[df$TS < 20 | df$TS > 40] <- NA

df$RH[df$RH < 25 | df$RH > 100] <- NA

df$RN[df$RN < -100] <- NA

df$RN[df$RN > 400] <- NA

df$RG[df$RG < 0] <- NA

df$WS[df$WS > 100] <- NA

df$ZL[abs(df$ZL)>10] <- NA

##### Filter data for rain #####
df$P_RAIN[df$P_RAIN > 0.1 | df$P_RAIN < 0] <- NA

df$USTAR[which(df$USTAR > 10)] <- NA

#df$FCO2[which(df$correlation <= -0.7)] <- NA

#### CALCULATE CD AND ADD TO DF ####
CD <- (df$USTAR/df$WS)^2
df <- cbind(df,CD)
rm(CD)

#### ADD OUTLIER-REMOVED DATA #####

temp <- read.csv('data/station/merged_data.csv')
temp$datetime <- as.character(temp$datetime)
date <- strptime(temp$datetime,
                 format = "%d/%m/%Y %H:%M",
                 tz = "Asia/Kuala_Lumpur")
temp <- cbind(date, temp)
colnames(temp)[3] <- 'EMA'
colnames(temp)[4] <- 'TA_ema'
colnames(temp)[5] <- 'RH_ema'
temp <- temp[,-c(2)]

df <- merge(df, temp, by = "date")
rm(temp,date)

# ##### Add TS EMA data without data before 10:00 AM ####
# 
# ts_ema <- read.csv('data/station/TS_ema.csv')
# ts_ema$datetime <- as.character(ts_ema$datetime)
# date <- strptime(ts_ema$datetime,
#                  format = "%Y-%m-%d %H:%M:%OS",
#                  tz = "Asia/Kuala_Lumpur")
# ts_ema <- cbind(date, ts_ema)
# colnames(ts_ema)[1] <- 'date'
# ts_ema <- ts_ema[,-c(2,3)]
# 
# df <- merge(df, ts_ema, by = "date")
# rm(ts_ema)
# 
# ##### Add TA EMA data without data before 10:00 AM ####
# 
# TA_ema <- read.csv('data/station/TA_ema.csv')
# TA_ema$date <- as.character(TA_ema$date)
# date <- strptime(TA_ema$date,
#                  format = "%d/%m/%Y %H:%M",
#                  tz = "Asia/Kuala_Lumpur")
# TA_ema <- cbind(date, TA_ema)
# colnames(TA_ema)[4] <- 'TA_ema'
# TA_ema <- TA_ema[,-c(2,3)]
# 
# df <- merge(df, TA_ema, by = "date")
# rm(TA_ema)
# 
# ##### Add RH EMA data without data before 10:00 AM ####
# 
# RH_ema <- read.csv('data/station/RH_ema.csv')
# RH_ema$date <- as.character(RH_ema$date)
# date <- strptime(RH_ema$date,
#                  format = "%d/%m/%Y %H:%M",
#                  tz = "Asia/Kuala_Lumpur")
# RH_ema <- cbind(date, RH_ema)
# colnames(RH_ema)[4] <- 'RH_ema'
# RH_ema <- RH_ema[,-c(2,3)]
# 
# df <- merge(df, RH_ema, by = "date")
# rm(RH_ema)

#### CALCULATE DELTA T ####

delT <- df$TS - df$TA
df <- cbind(df,delT)
rm(delT)

#### CALCULATE DELTA T (EMA DATA) ####

delT_ema <- df$EMA - df$TA_ema
df <- cbind(df,delT_ema)
rm(delT_ema)

# Calculation of partial pressure of CO2 in air (Pa)
# mole fraction (mol/mol) = partial pressure(Pa)/ total pressure(Pa)
# convert Pa to kPa = 1 Pa * 0.001
# convert mole fraction in ppm to mol/mol (1 ppm * 10^-6)
PP_air_30 <- NA
co2_mF_30 <- df$co2_mole_fraction * 10^-6  # convert into mol/mol
PP_air_30 <- co2_mF_30 * df$air_pressure
PP_air_30 <- PP_air_30 * 0.001 # convert to kPa
# convert PP_air into micro-atm
PP_air_30 <- (PP_air_30/101.325) * 10^6
df <- cbind(df, PP_air_30)
rm(PP_air_30,co2_mF_30)


#### ADD SALINITY DATA ####
df_salinity <- read.csv('data/station/salinity.csv',sep = ',')

df_salinity$date <- as.POSIXct(df_salinity$date,"%d/%m/%Y %H:%M",tz='Asia/Kuala_Lumpur')

df_salinity_temp <- timeAverage(df_salinity, avg.time = "hour")

# Reusing Sat Data
df_sat_temp <- timeAverage(df_sat,avg.time = "1 hour")

date <- df_sat_temp$date

date <- format(round(date, units="hours"), format="%Y-%m-%d %H:%M:%S")

date <- as.POSIXct(date,"%Y-%m-%d %H:%M:%S", tz='Asia/Kuala_Lumpur')

df_sat_temp <- cbind(date,df_sat_temp)

df_sat_temp <- df_sat_temp[,-2]

df_sat_temp <- timeAverage(df_sat_temp,avg.time = "1 hour")

# Merge the salinity and sat data
df_salinity_sat <- merge(df_salinity_temp,df_sat_temp,by=c('date'))

df_salinity_sat <- timeAverage(df_salinity_sat, avg.time='month')

##### Calculate solubility using the Weiss (1974) equation ####
solub <- solubility(df_salinity_sat$SST,df_salinity_sat$salinity)
# Merging calculated solubility data into salinity and sat data
df_salinity_sat <- cbind(df_salinity_sat,solub)


# Some housekeeping
rm(df_salinity_temp, df_sat_temp, date,solub)



#### CO2 PARTIAL PRESSURE CALCULATIONS ####
##### 30-min CO2 seawater solubility ####

# Solubility of CO2 in seawater
# Reference: Liu, Q., Fukuda, K., Matsuda T. (2004). 
# Study of solubility of carbon dioxide in seawater.Journal of JIME, 39(12), 91-96
S = 0.10615 # [μmol+1 m-2 s-1]. 

##### The 30-min scaled k value using McGillis (2001) ####
# Schmidt number
# Sc = 2116.8 + (-136.25*t) + 4.7353*(t^2) + (-0.092307)*(t^3) + 0.0007555*(t^4)
# t = sea surface temperature

temp <- df$EMA
U <- df$WS
Sc <- 2116.8 + (-136.25*temp) + 4.7353*(temp^2) + (-0.092307)*(temp^3) + 0.0007555*(temp^4)
k_MG2001 <- ((660/Sc)^0.5)*(0.026*(U^3) + 3.3)  ## unit in cm/hr

##### Calculating monthly scaled PCO2,sw ####
conv_fact <- 100 * 3600 * 0.001 # Convert from umol m-2 s-1 h cm-1 L atm mol-1 to uatm
PCO2_sw <- ((df$FCO2/(k_MG2001 * S)) * conv_fact) + df$PP_air_30


df <- cbind(df,PCO2_sw)

rm(S,Sc,temp,U,PCO2_sw,k_MG2001,conv_fact)

# #### Unused: Atmospheric stability classification #####
# 
# unstable <- df$ZL[which(df$ZL < -0.1)]          #unstable, previously y  
# neutral <- df$ZL[(df$ZL > -0.1 & df$ZL < 0.1)]   #neutral, previously z
# stable <- df$ZL[which(df$ZL > 0.1)]             #stable, previously x

# Average to 1 month
df_month <- timeAverage(df, avg.time = "1 month")

# Average to 1 year
df_year <- timeAverage(df, avg.time = "1 year")

#Average data to daily intervals
df_day <- timeAverage(df, avg.time = "1 day")

# Merge satellite and eddy covariance data by month
df_merge_month <- merge(df_month, df_sat_month, by = "date")



# Merge satellite and eddy covariance data by year
df_merge_year <- merge(df_year, df_sat_year, by = "date")

# Random Filter after merge
df_merge_month$CHL[df_merge_month$CHL > 20] <- NA
df_merge_month$TS[df_merge_month$TS > 34] <- NA

rm(df_biomet, df_ec)

# Create Month Variable
df_merge_month$month <- months(df_merge_month$date)
# Create Year Variable
df_merge_month$year <- format(df_merge_month$date,"%Y")


#### CALCULATING DAILY CUMULATIVE RAIN ####
## Cumulative Rainfall, 2015, 2016, 2017 and 2018 Jan-March no data for rainfall

rain_c <- selectByDate(df,year = 2018)[,c(1,14)]
rain_2018 <- aggregate((rain_c$P_RAIN)~month(rain_c$date),
                       data=rain_c,FUN=sum)
rain_c <- selectByDate(df,year = 2019)[,c(1,14)]
rain_2019 <- aggregate((rain_c$P_RAIN)~month(rain_c$date),
                       data=rain_c,FUN=sum)
rain_c <- selectByDate(df,year = 2020)[,c(1,14)]
rain_2020 <- aggregate((rain_c$P_RAIN)~month(rain_c$date),
                       data=rain_c,FUN=sum)



#### NORMALIZING PCO2,SW BY SEAWATER TEMPERATURE ####
temp_year <- c(rep(df_merge_year$EMA[1],12),
               rep(df_merge_year$EMA[2],12),
               rep(df_merge_year$EMA[3],12),
               rep(df_merge_year$EMA[4],12),
               rep(df_merge_year$EMA[5],11))
temp_diff <- temp_year - df_merge_month$EMA

PCO2_sw_T <- df_merge_month$PCO2_sw * (exp(0.0423 * temp_diff))

df_merge_month <- cbind(df_merge_month,temp_diff,PCO2_sw_T)
rm(temp_year,temp_diff,PCO2_sw_T)

# Average to 1 year, again.
df_merge_year_from_month <- timeAverage(df_merge_month, avg.time = '1 year')


#### MONSOON 30-MIN DATA ####
NEM_30 <- selectByDate(df, month = c(12,1,2,3))
SWM_30 <- selectByDate(df, month = c(6,7,8,9))
FTM_30 <- selectByDate(df, month = c(10,11))
STM_30 <- selectByDate(df, month = c(4,5))

#### MONSOON MONTHLY DATA ####

NEM <- selectByDate(df_merge_month, month = c(12,1,2,3))
SWM <- selectByDate(df_merge_month, month = c(6,7,8,9))
FTM <- selectByDate(df_merge_month, month = c(10,11))
STM <- selectByDate(df_merge_month, month = c(4,5))



#### SENSITIVITY ANALYSIS OF FCO2 ####

# Filter the data first according to wind speed range

U1 <- data.frame(date = df$date[df$WS <= 0.5], U = df$WS[df$WS <= 0.5], 
                 FCO2 = df$FCO2[df$WS <= 0.5], 
                 FCO2_mmol = df$FCO2_mmol[df$WS <= 0.5])
U2 <- data.frame(date = df$date[df$WS > 0.5 & df$WS <= 1], 
                 U = df$WS[df$WS > 0.5 & df$WS <= 1], 
                 FCO2 = df$FCO2[df$WS > 0.5 & df$WS <= 1], 
                 FCO2_mmol = df$FCO2_mmol[df$WS > 0.5 & df$WS <= 1])
U12 <-  data.frame(date = df$date[df$WS < 1], U = df$WS[df$WS < 1], 
                   FCO2 = df$FCO2[df$WS < 1], 
                   FCO2_mmol = df$FCO2_mmol[df$WS < 1])
U3 <- data.frame(date = df$date[df$WS > 1 & df$WS <= 1.5],
                 U = df$WS[df$WS > 1 & df$WS <= 1.5], 
                 FCO2 = df$FCO2[df$WS > 1 & df$WS <= 1.5], 
                 FCO2_mmol = df$FCO2_mmol[df$WS > 1 & df$WS <= 1.5])
U4 <- data.frame(date = df$date[df$WS > 1.5 & df$WS <= 2],
                 U = df$WS[df$WS > 1.5 & df$WS <= 2], 
                 FCO2 = df$FCO2[df$WS > 1.5 & df$WS <= 2], 
                 FCO2_mmol = df$FCO2_mmol[df$WS > 1.5 & df$WS <= 2])
U5 <- data.frame(date = df$date[df$WS > 2 & df$WS <= 2.5],
                 U = df$WS[df$WS > 2 & df$WS <= 2.5], 
                 FCO2 = df$FCO2[df$WS > 2 & df$WS <= 2.5], 
                 FCO2_mmol = df$FCO2_mmol[df$WS > 2 & df$WS <= 2.5])
U6 <- data.frame(date = df$date[df$WS > 2.5],
                 U = df$WS[df$WS > 2.5], 
                 FCO2 = df$FCO2[df$WS > 2.5], 
                 FCO2_mmol = df$FCO2_mmol[df$WS > 2.5])

# Standard error for all positive-negative fluxes
sd(U1$FCO2,na.rm=T)/sqrt(sum(!is.na(U1$FCO2)))
sd(U12$FCO2,na.rm=T)/sqrt(sum(!is.na(U12$FCO2)))
sd(U2$FCO2,na.rm=T)/sqrt(sum(!is.na(U2$FCO2)))
sd(U3$FCO2,na.rm=T)/sqrt(sum(!is.na(U3$FCO2)))
sd(U4$FCO2,na.rm=T)/sqrt(sum(!is.na(U4$FCO2)))
sd(U5$FCO2,na.rm=T)/sqrt(sum(!is.na(U5$FCO2)))
sd(U6$FCO2,na.rm=T)/sqrt(sum(!is.na(U6$FCO2)))

mean(U12$FCO2,na.rm=T)


# Standard error for all positive fluxes
sd(U1$FCO2[U1$FCO2 > 0 ],na.rm=T)/sqrt(sum(!is.na(U1$FCO2[U1$FCO2 > 0 ])))
sd(U2$FCO2[U2$FCO2 > 0 ],na.rm=T)/sqrt(sum(!is.na(U2$FCO2[U2$FCO2 > 0 ])))
sd(U3$FCO2[U3$FCO2 > 0 ],na.rm=T)/sqrt(sum(!is.na(U3$FCO2[U3$FCO2 > 0 ])))
sd(U4$FCO2[U4$FCO2 > 0 ],na.rm=T)/sqrt(sum(!is.na(U4$FCO2[U4$FCO2 > 0 ])))
sd(U5$FCO2[U5$FCO2 > 0 ],na.rm=T)/sqrt(sum(!is.na(U5$FCO2[U5$FCO2 > 0 ])))
sd(U6$FCO2[U6$FCO2 > 0 ],na.rm=T)/sqrt(sum(!is.na(U6$FCO2[U6$FCO2 > 0 ])))

mean(U2$FCO2[U6$FCO2 > 0 ],na.rm=T)

# Standard error for all negative fluxes
sd(U1$FCO2[U1$FCO2 < 0 ],na.rm=T)/sqrt(sum(!is.na(U1$FCO2[U1$FCO2 < 0 ])))
sd(U2$FCO2[U2$FCO2 < 0 ],na.rm=T)/sqrt(sum(!is.na(U2$FCO2[U2$FCO2 < 0 ])))
sd(U3$FCO2[U3$FCO2 < 0 ],na.rm=T)/sqrt(sum(!is.na(U3$FCO2[U3$FCO2 < 0 ])))
sd(U4$FCO2[U4$FCO2 < 0 ],na.rm=T)/sqrt(sum(!is.na(U4$FCO2[U4$FCO2 < 0 ])))
sd(U5$FCO2[U5$FCO2 < 0 ],na.rm=T)/sqrt(sum(!is.na(U5$FCO2[U5$FCO2 < 0 ])))
sd(U6$FCO2[U6$FCO2 < 0 ],na.rm=T)/sqrt(sum(!is.na(U6$FCO2[U6$FCO2 < 0 ])))

mean(U6$FCO2[U6$FCO2 < 0 ],na.rm=T)

sqrt(((0.0189/0.0398)^2)+((0.0058/0.0724)^2))

sqrt((0.0012^2) + (0.0005^2) + (0.4816^2))

#U12
sqrt(((0.0008/mean(U12$FCO2,na.rm=T))^2) + (0.0005^2) + (0.0012^2)) * 2
#U1
sqrt(((0.0015/mean(U1$FCO2,na.rm=T))^2) + (0.0005^2) + (0.0012^2)) * 2
#U2
sqrt(((0.0010/mean(U2$FCO2,na.rm=T))^2) + (0.0005^2) + (0.0012^2)) * 2
#U3
sqrt(((0.0019/mean(U3$FCO2,na.rm=T))^2) + (0.0005^2) + (0.0012^2)) * 2
#U4
sqrt(((0.0029/mean(U4$FCO2,na.rm=T))^2) + (0.0005^2) + (0.0012^2)) * 2
#U5
sqrt(((0.0046/mean(U5$FCO2,na.rm=T))^2) + (0.0005^2) + (0.0012^2)) * 2
#U6
sqrt(((0.0075/mean(U6$FCO2,na.rm=T))^2) + (0.0005^2) + (0.0012^2)) * 2

#### IMPORT SOCAT DATA ####

socat <- read.csv('data/SOCAT/socat_data.csv', sep=',')

socat$date <- as.character(socat$date)
socat$date <- strptime(socat$date, 
                       format = "%d/%m/%Y %H:%M", 
                       tz = "Asia/Kuala_Lumpur")
socat$date <- as.POSIXct(socat$date)

socat <- timeAverage(socat, avg.time="30 min")

socat_PCO2_sw <- f2pCO2(T=socat$socat_sst,Patm = 1,P=0,socat$socat_fco2)

socat <- cbind(socat,socat_PCO2_sw)

rm(socat_PCO2_sw)

df_socat <- merge(df,socat,by='date')



#### TAKAHASHI (2002) VERIFICATION ####
plot(df_merge_month$date,df_merge_month$PCO2_sw,type='l',lwd=2)
lines(df_merge_month$date,df_merge_month$PCO2_sw_T,col='blue',lwd=2)


##### PCO2 vs TS ####

# The water temperature used for the calculation of the normalized PCO2 
# was TS in EMA.
plot(df_merge_month$TS,df_merge_month$PCO2_sw,pch=19)
points(df_merge_month$TS,df_merge_month$PCO2_sw_T,col='blue',pch=19)

lm_PCO2_sw <- lm(PCO2_sw ~ TS, data = df_merge_month)
cor.test(df_merge_month$TS, df_merge_month$PCO2_sw)
summary(lm_PCO2_sw)

lm_PCO2_sw_T <- lm(PCO2_sw_T ~ TS, data = df_merge_month)
cor.test(df_merge_month$TS, df_merge_month$PCO2_sw_T)
summary(lm_PCO2_sw_T)


##### PCO2 vs SST ####

plot(df_merge_month$SST,df_merge_month$PCO2_sw,pch=19)
points(df_merge_month$SST,df_merge_month$PCO2_sw_T,col='blue',pch=19)

lm_PCO2_sw <- lm(PCO2_sw ~ SST, data = df_merge_month)
cor.test(df_merge_month$SST, df_merge_month$PCO2_sw)
summary(lm_PCO2_sw)

lm_PCO2_sw_T <- lm(PCO2_sw_T ~ SST, data = df_merge_month)
cor.test(df_merge_month$SST, df_merge_month$PCO2_sw_T)
summary(lm_PCO2_sw_T)



  
       
          
         



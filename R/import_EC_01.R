#-----------------------------------------------------------------------#
#
# Title:  The Muka Head Eddy Covariance Data Import and Process Script
#
# Authors: Yusri Yusup, Ph.D.
#
# Affiliation:  Environmental Technology, School of Industrial Technology,
#               Universiti Sains Malaysia
#
# Datasets used:
#   1. Muka Head Eddy Covariance dataset
#   
#
# Dataset period:
# No. of rows: 
# No. of columns:
# Objective: To import and process the Muka Head EC data
#
#
#-----------------------------------------------------------------------#

#### PACKAGES #########################################

library(openair)    # For time averaging
source('R_tools/tool_convert_magic.R')
source('R_tools/tool_charactersNumeric.R')

#### EC DATA IMPORT ####

##### Import EC data ######
df_ec<-read.csv('data/station/MCO-MUKA21_full_output copy.csv')
df_biomet<- read.csv('data/station/biomet data 21.csv')

# Delete unnecessary columns and rows in EC data files
df_ec<- df_ec[-1,]  #remove the 1st row 


# Delete unnecessary columns and rows in biomet data files
df_biomet<- df_biomet[,c(-6,-7)] #remove (DOY,unamed)


# Rename the date
colnames(df_ec)[1] <- "DATE"
colnames(df_biomet)[7] <- "DATE"

df_biomet$DATE <- strptime(df_biomet$DATE, format="%Y-%m-%d %H:%M", tz='Asia/Kuala_Lumpur')
df_ec$DATE <- strptime(df_ec$DATE, format="%Y-%m-%d %H:%M", tz='Asia/Kuala_Lumpur')

##### Merge df with df_biomet ####
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

##### Extracting related variables from raw variables ####
df_all <- df #Create a copy of the data

df <- data.frame(df$date, df$DOY, df$WS, df$WD, 
                 df$ZL, df$USTAR, df$TAU_QC,
                 df$FCO2, df$FCO2_QC, df$H, df$H_QC, df$LE, df$LE_QC,
                 df$RG_1_1_1, df$RN_1_1_1, df$PPFD_1_1_1,
                 df$RH_1_1_1, df$TA_1_1_1, df$TS_1_1_1,
                 df$PA, df$P_RAIN_1_1_1, df$CO2)


#Rename variables
colnames(df) <- c("date", "DOY", "WS", "WD",
                  "ZL", "USTAR", "TAU_QC",
                  "FCO2", "FCO2_QC", "H", "H_QC", "LE", "LE_QC",
                  "RG", "RN", "PPFD", 
                  "RH", "TA", "TS",
                  "PA", "P_RAIN", "CO2")

#### PARAMETERS CONVERSIONS ####
##### Convert TA, TS from K to Celsius ####
df$TA<- df$TA - 273.15
df$TS<- df$TS - 273.15

##### Convert FCO2 from micro-mole per second to milli-mole per day ####
FCO2_mmol <- df$FCO2 * 86.4
df <- cbind(df, FCO2_mmol)
rm(FCO2_mmol)

#### DATA FILTERING ####

##### Quality-control filtering ####
###### QC LE ####
df$LE[which(df$LE_QC == 2)] <- NA 
###### QC H ####
df$H[which(df$H_QC == 2)] <- NA 
###### QC FCO2 ####
df$FCO2[which(df$FCO2_QC == 2)] <- NA
#df$FCO2[which(df$FCO2_QC == 1)] <- NA       # Stricter QC
###### QC USTAR ####
df$USTAR[which(df$TAU_QC == 2)] <- NA

##### Land filtering ####

# Note: Did not filter based on WD because insufficient data for gap-filling
# Could be re-added if the focus is on water surfaces.

#df$FCO2[df$WD > 45 & df$WD < 315] <- NA
#df$FCO2_mmol[df$WD > 45 & df$WD < 315] <- NA
#df$co2_mole_fraction[df$WD > 45 & df$WD < 315] <- NA

##### Rain filtering ####
df$FCO2[df$P_RAIN > 0.1] <- NA
df$LE[df$P_RAIN > 0.1] <- NA
df$H[df$P_RAIN > 0.1] <- NA
df$USTAR[df$P_RAIN > 0.1] <- NA
df$ZL[df$P_RAIN > 0.1] <- NA
df$CO2[df$P_RAIN > 0] <- NA

##### Remove FCO2 values that are too high or low ####
# The upper limit is set to 50 because of the recommendation of the 
# REddyProc package.
df$FCO2[abs(df$FCO2) > 40] <- NA
###### Remove all improbable values of H ####
df$H[abs(df$H) > 150] <- NA
###### Remove all improbable values of LE ####
df$LE[abs(df$LE) > 400] <- NA
###### Remove all improbable values of WS ####
df$WS[df$WS > 50] <- NA
###### Remove all improbable values of ZL ####
df$ZL[abs(df$ZL) > 10] <- NA
###### Remove all improbable values of USTAR ####
df$USTAR[which(df$USTAR > 10)] <- NA
###### Remove all improbable values of TA and TS ####
df$TA[which(df$TA < 0 | df$TA > 100 )] <- NA
df$TS[which(df$TS < 0 )] <- NA
df$TA[df$TA < 20 | df$TA > 40] <- NA
df$TS[df$TS < 20 | df$TS > 40] <- NA
###### Remove all improbable values of RH ####
df$RH[df$RH < 25 | df$RH > 100] <- NA
###### Remove all improbable values of RN ####
df$RN[df$RN < -100] <- NA
df$RN[df$RN > 400] <- NA
###### Remove all improbable values of RG ####
df$RG[df$RG < 0] <- 0
###### Remove all improbable values of RG ####
df$PPFD[df$PPFD < 0] <- 0
###### Remove all improbable values of P_RAIN ####
df$P_RAIN[df$P_RAIN < 0 | df$P_RAIN > 0.1] <- NA

#### END ####
rm(df_all,df_biomet,df_ec,charactersNumeric,convert_magic)

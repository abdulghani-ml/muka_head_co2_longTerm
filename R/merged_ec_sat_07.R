#-----------------------------------------------------------------------#
#
# Title:  Merging Sat Data with Eddy Covariance Data
#
# Authors: Yusri Yusup, Ph.D.
#
# Affiliation:  Environmental Technology, School of Industrial Technology,
#               Universiti Sains Malaysia
#
# Datasets used:
#   1. df_merged_filtered
#   2. df_merged_salinity_sat
#
# Dataset period:
# No. of rows: 
# No. of columns:
# Objective: To import and process data
#
#
#-----------------------------------------------------------------------#

#### ADJUSTING THE TIME STAMP OF EC DATA ####
df_temp <- timeAverage(df, avg.time = "month")

#### MERGE THE EC DATA WITH THE SAT DATA ####
df_merged_ec_sat <- merge(df_temp, df_merged_salinity_sat, by = "date")

#### REMOVE UNNECESSARY COLUMNS ####
df_merged_ec_sat <- data.frame(date = df_merged_ec_sat$date, DOY = df_merged_ec_sat$DOY,
                               WS = df_merged_ec_sat$WS, WD = df_merged_ec_sat$WD,
                               ZL = df_merged_ec_sat$ZL, USTAR = df_merged_ec_sat$USTAR,
                               FCO2 = df_merged_ec_sat$FCO2, H = df_merged_ec_sat$H,
                               LE = df_merged_ec_sat$LE, RG = df_merged_ec_sat$RG,
                               RN = df_merged_ec_sat$RN, PPFD = df_merged_ec_sat$PPFD,
                               RH = df_merged_ec_sat$RH, TA = df_merged_ec_sat$TA,
                               TS = df_merged_ec_sat$TS, PA = df_merged_ec_sat$PA,
                               P_RAIN = df_merged_ec_sat$P_RAIN, CO2 = df_merged_ec_sat$CO2,
                               FCO2_mmol = df_merged_ec_sat$FCO2_mmol,
                               salinity = df_merged_ec_sat$salinity, CHL = df_merged_ec_sat$CHL,
                               SST = df_merged_ec_sat$SST, POC = df_merged_ec_sat$POC,
                               PIC = df_merged_ec_sat$PIC, PAR = df_merged_ec_sat$PAR)


#### END ###
rm(df_temp)



# 2. SOCAT (https://www.socat.info)
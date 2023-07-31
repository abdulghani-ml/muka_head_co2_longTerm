#-----------------------------------------------------------------------#
#
# Title:  Miscellaneous Calculations
#
# Authors: Yusri Yusup, Ph.D.
#
# Affiliation:  Environmental Technology, School of Industrial Technology,
#               Universiti Sains Malaysia
#
# Datasets used:
#   1. df_merged_ec_sat (Satellite)
#   2. df_merged_filtered (EC)
#
# Dataset period:
# No. of rows: 
# No. of columns:
# Objective: To calculate parameters.
#
#
#-----------------------------------------------------------------------#
#### PACKAGES ####
source('R_tools/tool_salinity_weiss.R')

#### CALCULATE SOLUBILITY (WEISS, 1974) ####

solub <- solubility(df_merged_ec_sat$SST, df_merged_ec_sat$salinity)
# Merging calculated solubility data into salinity and sat data
df_merged_ec_sat$salinity <- cbind(df_merged_ec_sat$salinity,solub)

# Some housekeeping
rm(solub, solubility)

#### CALCULATE CD AND ADD TO DF ####
CD <- (df_merged_filtered$USTAR / df_merged_filtered$WS) ^2
df_merged_filtered <- cbind(df_merged_filtered,CD)
# Some housekeeping
rm(CD)

#### CALCULATION OF CO2 PARTIAL PRESSURE IN AIR (PA) ####
# mole fraction (mol/mol) = partial pressure (Pa)/ total pressure (Pa)
# convert Pa to kPa = 1 Pa * 0.001
# convert mole fraction in ppm to mol/mol (1 ppm * 10^-6)

PP_air_30 <- NA
co2_mF_30 <- df_merged_filtered$CO2 * 10^-6     # convert into mol/mol
PP_air_30 <- co2_mF_30 * df_merged_filtered$PA
PP_air_30 <- PP_air_30 * 0.001  # convert to kPa

# convert PP_air into micro-atm
PP_air_30 <- (PP_air_30/101.325) * 10^6
df_merged_filtered <- cbind(df_merged_filtered, PP_air_30)
# Some housekeeping
rm(PP_air_30,co2_mF_30)


#### CO2 SEAWATER SOLUBILITY CONSTANT ####

# Solubility of CO2 in seawater
# Reference: Liu, Q., Fukuda, K., Matsuda T. (2004). 
# Study of solubility of carbon dioxide in seawater.Journal of JIME, 39(12), 91-96
S = 0.10615 # [μmol+1 m-2 s-1]. 


##### Scaled, 30-min k McGillis (2001) ####
# The equation:
# Sc = 2116.8 + (-136.25*t) + 4.7353*(t^2) + (-0.092307)*(t^3) + 0.0007555*(t^4)
# t = sea surface temperature

temp <- df_merged_filtered$TS_ema
U <- df_merged_filtered$WS

# Schmidt number
Sc <- 2116.8 + (-136.25*temp) + 4.7353*(temp^2) + (-0.092307)*(temp^3) + 0.0007555*(temp^4)
k_MG2001 <- ((660/Sc)^0.5)*(0.026*(U^3) + 3.3)  ## unit in cm/hr

##### Estimating monthly scaled PCO2,sw ####
# Convert from umol m-2 s-1 h cm-1 L atm mol-1 to uatm
conv_fact <- 100 * 3600 * 0.001 

PCO2_sw <- ((df_merged_filtered$FCO2/(k_MG2001 * S)) * conv_fact) + 
  df_merged_filtered$PP_air_30

df_merged_filtered <- cbind(df_merged_filtered, PCO2_sw)

# Some housekeeping
rm(S, Sc, temp, U, PCO2_sw, k_MG2001, conv_fact)


#### CALCULATING MONTHLY CUMULATIVE RAIN ####
# Cumulative Rainfall, 2015, 2016, 2017 and 2018 Jan-March no data for rainfall
# The unit is in m (meter).
rain_c <- selectByDate(df_merged_filtered, year = 2018)[,c(1,21)]
rain_2018 <- aggregate((rain_c$P_RAIN) ~ month(rain_c$date),
                       data = rain_c, FUN = sum)
colnames(rain_2018) <- c('month', 'cum.p_rain')

rain_c <- selectByDate(df_merged_filtered, year = 2019)[,c(1,21)]
rain_2019 <- aggregate((rain_c$P_RAIN) ~ month(rain_c$date),
                       data = rain_c,FUN=sum)
colnames(rain_2019) <- c('month', 'cum.p_rain')

rain_c <- selectByDate(df_merged_filtered, year = 2020)[,c(1,21)]
rain_2020 <- aggregate((rain_c$P_RAIN) ~ month(rain_c$date),
                       data = rain_c,FUN=sum)
colnames(rain_2020) <- c('month', 'cum.p_rain')

# Some housekeeping
rm(rain_c)


# #### NORMALIZING PCO2,SW BY SEAWATER TEMPERATURE ####
# temp_year <- c(rep(df_merge_year$EMA[1],12),
#                rep(df_merge_year$EMA[2],12),
#                rep(df_merge_year$EMA[3],12),
#                rep(df_merge_year$EMA[4],12),
#                rep(df_merge_year$EMA[5],11))
# temp_diff <- temp_year - df_merge_month$EMA
# 
# PCO2_sw_T <- df_merge_month$PCO2_sw * (exp(0.0423 * temp_diff))
# 
# df_merge_month <- cbind(df_merge_month,temp_diff,PCO2_sw_T)
# rm(temp_year,temp_diff,PCO2_sw_T)
# 
# # Average to 1 year, again.
# df_merge_year_from_month <- timeAverage(df_merge_month, avg.time = '1 year')


#### SEPARATING THE DATA INTO MONSOONS ####
##### 30-min data ####
NEM_30 <- selectByDate(df, month = c(12,1,2,3))
SWM_30 <- selectByDate(df, month = c(6,7,8,9))
FTM_30 <- selectByDate(df, month = c(10,11))
STM_30 <- selectByDate(df, month = c(4,5))

##### Monsoon monthly data ####

NEM <- selectByDate(df_merge_month, month = c(12,1,2,3))
SWM <- selectByDate(df_merge_month, month = c(6,7,8,9))
FTM <- selectByDate(df_merge_month, month = c(10,11))
STM <- selectByDate(df_merge_month, month = c(4,5))



#### Calculate solubility using the Weiss (1974) equation ####
solub <- solubility(df_salinity_sat$SST,df_salinity_sat$salinity)
# Merging calculated solubility data into salinity and sat data
df_salinity_sat <- cbind(df_salinity_sat,solub)


# Some housekeeping
rm(df_salinity_temp, df_sat_temp, date,solub)

#### CALCULATE CD AND ADD TO DF ####
CD <- (df$USTAR/df$WS)^2
df <- cbind(df,CD)
rm(CD)

#### CALCULATION OF CO2 PARTIAL PRESSURE IN AIR (PA) ####
# mole fraction (mol/mol) = partial pressure(Pa)/ total pressure(Pa)
# convert Pa to kPa = 1 Pa * 0.001
# convert mole fraction in ppm to mol/mol (1 ppm * 10^-6)
PP_air_30 <- NA
co2_mF_30 <- df$CO2 * 10^-6  # convert into mol/mol
PP_air_30 <- co2_mF_30 * df$PA
PP_air_30 <- PP_air_30 * 0.001 # convert to kPa
# convert PP_air into micro-atm
PP_air_30 <- (PP_air_30/101.325) * 10^6
df <- cbind(df, PP_air_30)
rm(PP_air_30,co2_mF_30)



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

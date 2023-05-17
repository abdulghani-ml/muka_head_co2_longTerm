#-----------------------------------------------------------------------#
#
# Title: Import and Process Muka Head, Wave and Precipitation Data 
# Author: Chean Wan Ting, Yusri Yusup
# Affiliation: Environmental Technology, School of Industrial Technology
# Universiti Sains Malaysia
# Datasets used:
#   1. CEMACS (Data in Brief paper doi)
#   2. EUMETSAT (web address)
#   3. Weather Underground (web address)
# Dataset period: 2015-11 to 2017-10 (2 years)
# No. of rows: 
# No. of columns:
# Objective: To combine EC data from Muka Head with Significant 
# Wave Height (swh) data from EUMETSAT, and precipitation data 
# from Wunderground.
#
#-----------------------------------------------------------------------#

#### Load libraries ####
require(openair)
require(dplyr)
require(rgdal) # install package (rdgal) to convert the LatLon to UTM

#### Function to convert lon_site and lat_site into UTC ####
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

#### 1. Import data ####
muka<-read.csv('csv_file/Finalise_Muka_Head_Data.csv')
wave <-read.csv('csv_file/DWave.csv')
rain <-read.csv('csv_file/IPENANGP2_2017-10-12_143327.csv')

chl <- read.csv('csv_file/by_monthly_group_data_hy.csv')
chl <- chl[-25,]
chlor_a <- NA
chlor_a <- chl$Chlorophyll
T_sat <- NA
T_sat <- chl$Sea_Surface_Temp

#### 2. Change the time using POSIXct ####
# Muka Head EC Data (muka)
date <- as.POSIXct(muka$daytime, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
muka <- cbind(date,muka)
muka <- muka[,-2]

# Wave Data (wave)
date <- as.POSIXct(wave$time1, format = "%d/%m/%Y %H:%M", tz = "GMT")
date <- as.POSIXct(wave$time1, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
wave <- cbind(date,wave)
wave <- wave[,-5]

# Rain Data (rain)
date <- as.POSIXct(rain$Time, format = "%Y-%m-%d %H:%M:%S", tz= "Asia/Kuala_Lumpur")
rain <-cbind(date,rain)
rain <-rain[,-2]
rm(date)

#### 3. Filter Data ####
# Remove unwanted columns
## Wave Data
wave <- wave[,-2]

## Rain Data
rain <- rain[,-c(2,3,4,5,6,7,8,9,10,12,13,15,16,17)]

## Muka Head Data
# Remove problematic value in wind speed i.e., U > 5 m s-1
# plot(muka$wind_speed)
muka$wind_speed[muka$wind_speed > 5] <- NA
muka$wind_speed[muka$wind_speed < 0.5] <- NA
# plot(muka$wind_speed)

# Remove problematic value in sea surface temperature (TS)
# i.e., 29 < TS < 33
#plot(muka$TS)
muka$TS[muka$TS > 33 | muka$TS < 29] <- NA
#plot(muka$TS)
 

#### 4. Convert angle to radian ####
# Add wind_dir column to datasheet
muka$dir_90 <- muka$wind_dir
muka$dir_90[muka$dir_90 > 90 & muka$wind_dir < 315] <- NA # only retain the wind_dir from 0 to 90 deg
muka$radian_wd <- muka$dir_90 
muka$radian_wd <- (muka$dir_90 * pi)/180

#### 5. Merge Muka Head, Wave, Rain Data ####
temp <- timeAverage(wave, avg.time = "30 min", start.date = "2015-11-03 11:00:00", interval = "30 min")
temp2 <- timeAverage(rain, avg.time = "30 min", start.date = "2015-11-12 00:00:00", interval = "30 min")
merged_df <- merge(muka, temp, by = c("date","date"))
merged_df <-merge(merged_df,temp2, by =c("date", "date"))
rm(muka,wave,rain,temp,temp2)

#### 6. Quality Control ####
# Remove qc = 2, wind_dir > 90, rain > 0
# Create temp data frame
m2<- merged_df

# Remove qc=2, wind direction > 90 & < 315, rain > 0
# CO2 flux
m2$co2_flux[m2$wind_dir > 90 & m2$wind_dir < 315] <- NA
m2$co2_flux[m2$qc_co2_flux == 2] <- NA
m2$co2_flux[m2$HourlyPrecipMM > 0] <- NA
m2$co2_flux[m2$co2_flux > 3] <- NA
m2$co2_flux[m2$co2_flux < -3] <- NA
m2$u.[m2$wind_dir > 90 & m2$wind_dir < 315] <- NA
m2$u.[m2$HourlyPrecipMM > 0] <- NA
m2$co2_mole_fraction[m2$wind_dir > 90 & m2$wind_dir < 315] <- NA
m2$co2_mixing_ratio[m2$wind_dir > 90 & m2$wind_dir < 315] <- NA
m2$x_90.[m2$wind_dir > 90 & m2$wind_dir < 315] <- NA

m2$co2_flux[m2$wind_speed < 0.5] <- NA
m2$u.[m2$wind_speed < 0.5] <- NA

#Manually remove outlier for CO2 flux
plot(m2$date,m2$co2_flux)
plot(m2$wind_speed[m2$wind_speed >= 0.5], m2$u.[m2$wind_speed >= 0.5], ylim =c(0,0.3))
lm1 <- lm(m2$u.[m2$wind_speed >= 0.5] ~ m2$wind_speed[m2$wind_speed >= 0.5])
abline(lm1, col = 'red', lwd = 2)
summary(lm1)

plot(m2$wind_speed,m2$co2_flux, ylim = c(-1,1))
#which.max(m2$co2_flux)
#which.min(m2$co2_flux)


#### 7. Grouping data monthly ####

by_monthly_grp_data_mean <- m2 %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
            co2_mixing_ratio=mean(co2_mixing_ratio,na.rm=TRUE),
            co2_mole_fraction=mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure=mean(air_pressure,na.rm = TRUE),
            U_insitu =mean(wind_speed,na.rm=TRUE),
            u. =mean(u.,na.rm = TRUE),
            x_90. = mean(x_90.,na.rm = TRUE),
            Site_temp=mean(TS,na.rm=TRUE),
            radian_wd =mean(radian_wd, na.rm = TRUE),
            Precipitation = mean(HourlyPrecipMM,na.rm=TRUE),
            Lat_sat = mean(lat1, na.rm = TRUE),
            Lon_sat = mean(lon1, na.rm = TRUE),
            swh = mean(swh1, na.rm = TRUE),
            U_sat = mean(wind_speed_alt1, na.rm = TRUE))
          
by_monthly_grp_data_sd <- m2 %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm=TRUE),
            co2_mixing_ratio_sd=sd(co2_mixing_ratio,na.rm=TRUE),
            co2_mole_fraction_sd=sd(co2_mole_fraction,na.rm=TRUE),
            air_pressure_sd=sd(air_pressure,na.rm = TRUE),
            U_insitu_sd =sd(wind_speed,na.rm=TRUE),
            radian_wd_sd =sd(radian_wd, na.rm = TRUE),
            u._sd =sd(u.,na.rm = TRUE),
            x_90._sd = sd(x_90.,na.rm = TRUE),
            Site_temp_sd=sd(TS,na.rm=TRUE),
            Precipitation_sd = sd(HourlyPrecipMM,na.rm=TRUE),
            Lat_sat_sd = sd(lat1, na.rm = TRUE),
            Lon_sat_sd = sd(lon1, na.rm = TRUE),
            swh_sd = sd(swh1, na.rm = TRUE),
            U_sat_sd =sd(wind_speed_alt1, na.rm = TRUE))

#Merge the two dataframe
by_monthly_grp_data <- merge(by_monthly_grp_data_mean, by_monthly_grp_data_sd,by='month')
rm(by_monthly_grp_data_mean,by_monthly_grp_data_sd)
month <- paste(by_monthly_grp_data$month,"15"," 00:00:00")
by_monthly_grp_data <- cbind(month,by_monthly_grp_data)
month <- as.POSIXct(month, format = "%Y %m %d %H:%M:%S", tz = 'Asia/Kuala_Lumpur')
by_monthly_grp_data <- cbind(month,by_monthly_grp_data)
by_monthly_grp_data <- by_monthly_grp_data[,-2]

#Create temp data frame
monthly_data <- by_monthly_grp_data

#### 8. Input latlon of Muka Head Station ####
lat_site <- NA
lat_site <- 5.468333
monthly_data <- cbind(monthly_data,lat_site)

lon_site <- NA
lon_site <- 100.2002778
monthly_data <- cbind(monthly_data,lon_site)

## Calculation of coordinate of carbon footprint 
# x = xo + r cos delta
# y = yo + r sin delta
# r = distance, delta = angle of wind direction in radian
# x and y in meter

x <- monthly_data$lon_site
y <- monthly_data$lat_site
#LongLatToUTM(x,y,47)  ## Malaysia in zone 47
monthly_data <- cbind(monthly_data,LongLatToUTM(x,y,47))
names(monthly_data)[34] <- "Easting_Site"
names(monthly_data)[35] <- "Northing_Site"
monthly_data <- monthly_data[,-33]

#Calculate the coordinate
x_cfp <- NA
x_cfp <- monthly_data$Easting_Site + (monthly_data$x_90. * cos(monthly_data$radian_wd))
monthly_data <- cbind(monthly_data, x_cfp)

y_cfp <- NA
y_cfp <- monthly_data$Northing_Site + (monthly_data$x_90. * sin(monthly_data$radian_wd))
monthly_data <- cbind(monthly_data, y_cfp)


x <- monthly_data$Lon_sat - 180
y <- monthly_data$Lat_sat
#LongLatToUTM(x,y,47)  ## Malaysia in zone 47
monthly_data <- cbind(monthly_data,LongLatToUTM(x,y,47))
names(monthly_data)[38] <- "Easting_Sat"
names(monthly_data)[39] <- "Northing_Sat"
monthly_data <- monthly_data[,-37]

#Calculation of the distance between site carbon footprint and satellite
distance <- NA
x1 <- (monthly_data$Easting_Sat - monthly_data$x_cfp)^2
y1 <- (monthly_data$Northing_Sat - monthly_data$y_cfp)^2
distance <- (x1 + y1)^0.5
monthly_data <- cbind(monthly_data, distance)

#### 9. Calculation for Delta Partial Pressure of CO2 monthly average ####
# Calculation of partial pressure of CO2 in air (Pa)
# mole fraction (mol/mol) = partial pressure(Pa)/ total pressure(Pa)
# convert Pa to kPa = 1 Pa * 0.001
# convert mole fraction in ppm to mol/mol (1 ppm * 10^-6)
PP_air <- NA
co2_mF <- monthly_data$co2_mole_fraction * 10^-6  # convert into mol/mol
PP_air <- co2_mF * monthly_data$air_pressure
PP_air <- PP_air * 0.001 # convert to kPa
# convert PP_air into micro-atm
PP_air <- (PP_air/101.325) * 10^6
monthly_data <- cbind(monthly_data, PP_air)

#### 9a. Calculation for Delta Partial Pressure of CO2 30-min average ####
# Calculation of partial pressure of CO2 in air (Pa)
# mole fraction (mol/mol) = partial pressure(Pa)/ total pressure(Pa)
# convert Pa to kPa = 1 Pa * 0.001
# convert mole fraction in ppm to mol/mol (1 ppm * 10^-6)
PP_air_30 <- NA
co2_mF_30 <- m2$co2_mole_fraction * 10^-6  # convert into mol/mol
PP_air_30 <- co2_mF_30 * m2$air_pressure
PP_air_30 <- PP_air_30 * 0.001 # convert to kPa
# convert PP_air into micro-atm
PP_air_30 <- (PP_air_30/101.325) * 10^6
#monthly_data <- cbind(monthly_data, PP_air)


# partial pressure of carbon dioxide in seawater (PCO2) (microatam) 
# Use Zhu's Algorithm to calculate PCO2sw by using T and chlorophyll 
# from AQUA MODIS 
# PCO2_sw = 6.31 SST^2 + 61.9 chlor_a^2 - 365.85 SST - 94.41 chlor_a + 5715.94
# SST in degree celcius
# chlor_a in mg/m3
# combine the dataset 

# Add the chl and SST data into the df
monthly_data <- cbind(monthly_data, chlor_a)
monthly_data <- cbind(monthly_data, T_sat)

SST <- monthly_data$T_sat
chlor_a <- monthly_data$chlor_a
PCO2_sw <- NA

# Zhu et al. (2009) - northern South China Sea in the summer
PCO2_sw_zhu_sat <- (6.31 * (SST^2)) + (61.9 * (chlor_a^2)) - 
  (365.85 * SST) - (94.41 * chlor_a) + 5715.94 
PCO2_sw_zhu_site_temp <- (6.31 * (monthly_data$Site_temp^2)) + (61.9 * (chlor_a^2)) - 
  (365.85 * monthly_data$Site_temp) - (94.41 * chlor_a) + 5715.94 
monthly_data <- cbind(monthly_data, PCO2_sw_zhu_sat)
monthly_data <- cbind(monthly_data, PCO2_sw_zhu_site_temp)

#deltaP in microatm
# delta_p_p <- NA
delta_p_p_sat <- monthly_data$PCO2_sw_zhu_sat - monthly_data$PP_air
delta_p_p_site <- monthly_data$PCO2_sw_zhu_site_temp - monthly_data$PP_air
monthly_data <- cbind(monthly_data, delta_p_p_sat, delta_p_p_site)



## Calculate k value (gas transfer velocity) with the bulk formula ##
# Fco2 = k S delta_p_p
# Fco2 in [μmol+1 m-2 s-1]
# S = solubility of carbon dioxide in water [CITATION???]
# S = 0.10615 [mol+1 L-1 atm-1]
# delta_p_p  [μatm]
# k [m+1 s-1]

#### CO2 seawater solubility ####
Fco2 <- monthly_data$co2_flux
# Solubility of CO2 in seawater
# Reference: Liu, Q., Fukuda, K., Matsuda T. (2004). Study of solubility of carbon dioxide in seawater.Journal of JIME, 39(12), 91-96
S = 0.10615 # [μmol+1 m-2 s-1]. 

delta_p_p <- monthly_data$delta_p_p_site
k_insitu <- NA
k_insitu <- (Fco2 / (S * delta_p_p)) / 1000  # convert from [L] to [m+3]
k_insitu <- k_insitu * 100 * 3600  # [cm+1 hr-1]
monthly_data <- cbind(monthly_data, k_insitu)


#### The k value calculate using Wanninkhof and McGillis (1999) - 30-min average ####
# k calculated only considering turbulence and SST
# k = 1/β * 1/(Sc^n) * u.
# β = 16-11  # 16 is low turbulence, 11 is high turbulence
# n = 0.67 - 0.4   # 0.67 for smooth surface
# Sc = 2116.8 + (-136.25*t) + 4.7353*(t^2) + (-0.092307)*(t^3) + 0.0007555*(t^4)
# t = sea surface temperature

t30 <- m2$TS

Sc30 <- 2116.8 + (-136.25*t30) + 4.7353*(t30^2) + (-0.092307)*(t30^3) + 0.0007555*(t30^4)
#n <- 0.5 # smooth surface
#beta <- 16 # lowest turbulence
u.30 <- m2$u.
#k_theory30 <- (1/beta) * u.30 * (1/Sc30^n) * 100 * 60  # unit in cm/hr from m/s 


#monthly_data<- cbind(monthly_data,k_theory)

#### The k value calculate using Wanninkhof and McGillis (1999) monthly average ####
# k calculated only considering turbulence and SST
# k = 1/β * 1/(Sc^n) * u.
# β = 16-11  # 16 is low turbulence, 11 is high turbulence
# n = 0.67 -0.4   # 0.67 for smooth surface
# Sc = 2116.8 + (-136.25*t) + 4.7353*(t^2) + (-0.092307)*(t^3) + 0.0007555*(t^4)
# t = sea surface temperature

t <- monthly_data$T_sat
t_site <- monthly_data$Site_temp
Sc_site <- 2116.8 + (-136.25*t_site)+4.7353*(t_site^2) + (-0.092307)*(t_site^3) + 0.0007555*(t_site^4)
Sc <- 2116.8 + (-136.25*t)+4.7353*(t^2) + (-0.092307)*(t^3) + 0.0007555*(t^4)
#n <- 0.5 # smooth surface
#beta <- 16 # lowest turbulence
u. <- monthly_data$u.
#k_theory <- 1/beta * 1/(Sc_site^n)
#k_theory <- k_theory * 100 * 60  # unit in cm/hr from m/s
#k_660 <- k_theory * (Sc_site/660)^0.5 # unit in cm/hr

#monthly_data<- cbind(monthly_data,k_theory)


#### k value calculated using equations from literature ####
# k_N2000 based on Nightingale
# k_N2000 = ((660/Sc)^0.5)*(0.212*(U^2) + 0.318U)

U <- monthly_data$U_insitu
k_N2000 <- ((660/Sc_site)^0.5) * (0.212*(U^2) + 0.318*U)  
#k_N2000 <- k_N2000 * 100 * 3600 ## unit in cm/hr

monthly_data <- cbind(monthly_data,k_N2000)

U30 <- m2$wind_speed

k_N2000_30 <- (0.212*(U30^2) + 0.318*U30)  * ((660/Sc30)^0.5)

# k_W2014 based on Wanninkhof 2014
# k = ((660/Sc)^0.5)*(0.251*(U^2))

k_W2014 <- ((660/Sc_site)^0.5)*(0.251*(U^2))   ## unit in cm/hr
monthly_data <- cbind(monthly_data, k_W2014)

k_W2014_30 <- ((660/Sc30)^0.5)*(0.251*(U30^2)) ## unit in cm/hr

#k_MG2001 based on McGillis 2001

k_MG2001 <- ((660/Sc_site)^0.5)*(0.026*(U^3) + 3.3)  ## unit in cm/hr
monthly_data <- cbind(monthly_data,k_MG2001)

k_MG2001_30 <- ((660/Sc30)^0.5)*(0.026*(U30^3) + 3.3)

#k_WMG1999 based on Wanniknof n McGillis 1999

k_WMG1999 <- ((660/Sc_site)^0.5)*(0.0283*(U^3))  ## unit in cm/hr
monthly_data<- cbind(monthly_data,k_WMG1999)

k_WMG1999_30 <- ((660/Sc30)^0.5)*(0.0283*(U30^3))  ## unit in cm/hr


#### Calculate the CO2 water partial pressure ####
conv_fact <- 100 * 3600 * 0.001 # Convert from umol m-2 s-1 h cm-1 L atm mol-1 to uatm
PCO2_sw_30_N2000 <- ((m2$co2_flux / (k_N2000_30 * S)) * conv_fact) + PP_air_30
PCO2_sw_30_WMG1999 <- ((m2$co2_flux / (k_WMG1999_30 * S)) * conv_fact) + PP_air_30
PCO2_sw_30_W2014 <- ((m2$co2_flux / (k_W2014_30 * S)) * conv_fact) + PP_air_30
PCO2_sw_30_MG2001 <- ((m2$co2_flux / (k_MG2001_30 * S)) * conv_fact) + PP_air_30
delta_pco2 <- PCO2_sw_30_MG2001 - PP_air_30

pco2_30 <- data.frame(date = m2$date, PCO2_sw_30_N2000,
                      PCO2_sw_30_WMG1999,PCO2_sw_30_W2014,
                      PCO2_sw_30_MG2001,delta_pco2)

by_monthly_grp_pco2_mean <- pco2_30 %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(PCO2_sw_30_N2000=mean(PCO2_sw_30_N2000,na.rm=TRUE),
            PCO2_sw_30_WMG1999=mean(PCO2_sw_30_WMG1999,na.rm=TRUE),
            PCO2_sw_30_W2014=mean(PCO2_sw_30_W2014,na.rm=TRUE),
            PCO2_sw_30_MG2001=mean(PCO2_sw_30_MG2001,na.rm = TRUE),
            delta_pco2 =mean(delta_pco2,na.rm=TRUE))

by_monthly_grp_pco2_sd <- pco2_30 %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(PCO2_sw_30_N2000_sd=sd(PCO2_sw_30_N2000,na.rm=TRUE),
            PCO2_sw_30_WMG1999_sd=sd(PCO2_sw_30_WMG1999,na.rm=TRUE),
            PCO2_sw_30_W2014_sd=sd(PCO2_sw_30_W2014,na.rm=TRUE),
            PCO2_sw_30_MG2001_sd=sd(PCO2_sw_30_MG2001,na.rm = TRUE),
            delta_pco2_sd =sd(delta_pco2,na.rm=TRUE))

#Merge the two dataframe
by_monthly_grp_pco2 <- merge(by_monthly_grp_pco2_mean, by_monthly_grp_pco2_sd,by='month')
rm(by_monthly_grp_pco2_mean,by_monthly_grp_pco2_sd)
month <- paste(by_monthly_grp_pco2$month,"15"," 00:00:00")
by_monthly_grp_pco2 <- cbind(month,by_monthly_grp_pco2)
month <- as.POSIXct(month, format = "%Y %m %d %H:%M:%S", tz = 'Asia/Kuala_Lumpur')
by_monthly_grp_pco2 <- cbind(month,by_monthly_grp_pco2)
by_monthly_grp_pco2 <- by_monthly_grp_pco2[,-2]

#Create temp data frame
monthly_data_pco2 <- by_monthly_grp_pco2

low_limit <- ((311 - 382) / conv_fact) * (k_N2000_30 * S)
high_limit <- ((408 - 395) / conv_fact) * (k_N2000_30 * S)

summary(PCO2_sw_30_MG2001[which(PCO2_sw_30_MG2001<0)])
summary(m2$co2_flux[which(PCO2_sw_30_MG2001>300 & PCO2_sw_30_MG2001<400)])

co2_flux_sw_300_400 <- m2$co2_flux
co2_flux_sw_300_400[which(PCO2_sw_30_MG2001<300 | PCO2_sw_30_MG2001>400)] <- NA
plot(m2$date,co2_flux_sw_300_400)

#### Use the k value to calculate the co2_flux ####
k_insitu_co2 <- NA
k_insitu_co2 <- (k_insitu/3600/100) * S *1000 * delta_p_p
monthly_data <- cbind(monthly_data, k_insitu_co2)

k_theory_co2 <- NA
k_theory_co2 = (k_theory/3600/1000) * S * 1000 * delta_p_p
monthly_data <- cbind(monthly_data, k_theory_co2)

k_N2000_co2 <- NA
k_N2000_co2 = (k_N2000/3600/100) * S *1000 * delta_p_p
monthly_data <- cbind(monthly_data, k_N2000_co2)

k_W2014_co2 <- NA
k_W2014_co2 = (k_W2014/3600/100) * S* 1000 * delta_p_p
monthly_data<- cbind(monthly_data, k_W2014_co2)

k_MG2001_co2 <- NA
k_MG2001_co2 = (k_MG2001/3600/100) * S*1000 * delta_p_p
monthly_data <- cbind(monthly_data, k_MG2001_co2)

k_WMG1999_co2 <- NA
k_WMG1999_co2 = (k_WMG1999/3600/100) * S* 1000 * delta_p_p
monthly_data<- cbind(monthly_data, k_WMG1999_co2)


#### Windrose ####
jpeg('figs/paper/windrose.jpg',width=16,height=16,res=400, units = 'cm')
par(family='serif', mfrow = c(1,1), mar = c(3.9, 3.9, 1, 1))
windRose(m2, 
         ws = 'wind_speed', wd = 'dir_90',
         ws.int = 1,
         breaks = 6,
         angle = 10,
         cols = 'jet',
         key.position = 'right',
         angle.scale =225, 
         offset = 20,
         width= 4,
         grid.line = 1,
         #type = 'season',
         paddle = T,
         par.settings=list(fontsize=list(text=12))
)
dev.off()

polarPlot(m2, pollutant='co2_flux', x = 'wind_speed', wd = 'dir_90',
          force.positive = F)
### 
#### Descriptive Analysis ####
summary(monthly_data)


#### Correlation Analysis ####
#Hs = significant waves height (swh)
cor.test(monthly_data$swh,monthly_data$co2_flux, method="pearson")
cor.test(monthly_data$swh,monthly_data$U_insitu, method="pearson")
cor.test(monthly_data$swh,monthly_data$U_sat, method="pearson")
cor.test(monthly_data$swh,monthly_data$u., method="pearson")

#co2_flux
cor.test(monthly_data$co2_flux,monthly_data$U_insitu, method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$U_sat, method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$u., method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$PP_air, method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$PCO2_sw, method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$delta_p_p, method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$T_sat, method="pearson")
cor.test(monthly_data$co2_flux,monthly_data$chlor_a, method="pearson")

#U_insitu
cor.test(monthly_data$U_insitu,monthly_data$U_sat, method="pearson")
cor.test(monthly_data$U_insitu,monthly_data$u., method="pearson")

#U_sat
cor.test(monthly_data$U_sat,monthly_data$u., method="pearson")

#PP_air
cor.test(monthly_data$PP_air,monthly_data$PCO2_sw, method="pearson")
cor.test(monthly_data$PP_air,monthly_data$delta_p_p, method="pearson")
cor.test(monthly_data$PP_air,monthly_data$T_sat, method="pearson")
cor.test(monthly_data$PP_air,monthly_data$chlor_a, method="pearson")

#PCO2_sw
cor.test(monthly_data$PCO2_sw,monthly_data$delta_p_p, method="pearson")
cor.test(monthly_data$PCO2_sw,monthly_data$T_sat, method="pearson")
cor.test(monthly_data$PCO2_sw,monthly_data$chlor_a, method="pearson")

#delta_p_p
cor.test(monthly_data$delta_p_p,monthly_data$T_sat, method="pearson")
cor.test(monthly_data$delta_p_p,monthly_data$chlor_a, method="pearson")

#T_sat
cor.test(monthly_data$T_sat,monthly_data$chlor_a, method="pearson")


#### Plot k versus Hs ####
library(Hmisc)
path_fig <- file.path('figs/k_versus_Hs.jpg')
jpeg(file=path_fig,width=16,height=14,res=400, units = 'cm')
par(family='serif', mfrow = c(3,2),
    mar = c(3.9, 3.9, 1, 1))

plot(monthly_data$swh, monthly_data$k_insitu, pch=16, col='darkblue',xlab= '', ylab='')
lm1 <- lm(monthly_data$k_insitu ~ monthly_data$swh)
abline(lm1,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['in-situ'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(monthly_data$swh, monthly_data$k_theory, pch=16, col='darkblue',xlab= '', ylab='')
lm2 <- lm(monthly_data$k_theory ~ monthly_data$swh)
abline(lm2,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['theory'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(monthly_data$swh, monthly_data$k_N2000, pch=16, col='darkblue',xlab= '', ylab='')
lm3 <- lm(monthly_data$k_N2000 ~ monthly_data$swh)
abline(lm3,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['N2000'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(monthly_data$swh, monthly_data$k_W2014, pch=16, col='darkblue',xlab= '', ylab='')
lm4 <- lm(monthly_data$k_W2014 ~ monthly_data$swh)
abline(lm4,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['W2014'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(monthly_data$swh, monthly_data$k_MG2001, pch=16, col='darkblue',xlab= '', ylab='')
lm5 <- lm(monthly_data$k_MG2001 ~ monthly_data$swh)
abline(lm5,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['MG2001'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)

plot(monthly_data$swh, monthly_data$k_WMG1999, pch=16, col='darkblue',xlab= '', ylab='')
lm6 <- lm(monthly_data$k_WMG1999 ~ monthly_data$swh)
abline(lm6,col='red', lwd=3)
minor.tick()
mtext(expression(paste('k'['WMG1999'])),side = 2,line=2)
mtext(expression('H'['s']),side = 1,line=2.3)
dev.off()

#to know the value for slope for k_versus_Hs
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)
summary(lm6)


#### u* vs U plot ####
jpeg(file='figs/paper/friction_velocity.jpg',width=8,height=8,res=400, units = 'cm')
par(mar = c(3.5, 3.5, 0.5, 0.5))
plot(m2$wind_speed,m2$u., ylim = c(0,0.3), pch = 19, cex = 0.2, xlab= '', ylab='')
lm1 <- lm(m2$u. ~ m2$wind_speed)
abline(lm1, lwd=2, lty=2)
minor.tick()
mtext(expression(paste('u'['*'])),side = 2,line=2)
mtext(expression('U'),side = 1,line=2.3)
dev.off()

# Drag coefficient, CD

CD <- m2$u.^2 / m2$wind_speed^2

#### CD vs U plot ####
jpeg(file='figs/paper/CD_vs_U.jpg',width=8,height=8,res=400, units = 'cm')
par(mar = c(3.5, 3.5, 0.6, 0.5))
plot(m2$wind_speed,CD, xlim=c(0.5,5), ylim=c(0,0.02), 
     pch = 19, cex = 0.3, xlab= '', ylab='')
minor.tick()
mtext(expression(paste('C'['D'])),side = 2,line=2)
mtext(expression('U'),side = 1,line=2.3)
dev.off()

#### CD vs swh ####
CD1 <- CD
CD1[CD > 0.5] <- NA
CD1[CD > 0.01] <- NA

CD_swh <- data.frame(CD1, swh = m2$swh1, u. = m2$u., m2$wind_speed, k_MG2001_30,
                     k_N2000_30, k_WMG1999_30, k_W2014_30)
CD_swh <- na.omit(CD_swh)

jpeg(file='figs/paper/CD_vs_swh.jpg',width=8,height=8,res=400, units = 'cm')
par(mar = c(3.5, 3.5, 0.6, 0.5))
plot(CD_swh$swh,CD_swh$CD1, xlim=c(0,2.5), ylim=c(0,0.005), 
     pch = 19, cex = 0.5, xlab= '', ylab='')
minor.tick()
mtext(expression(paste('C'['D'])),side = 2,line=2)
mtext(expression(paste('H'['s'])),side = 1,line=2.3)
dev.off()


plot(CD_swh$swh,CD_swh$k_MG2001_30,pch=19,col='blue',ylim=c(0,8))
points(CD_swh$swh,CD_swh$k_N2000_30,pch=19)
points(CD_swh$swh,CD_swh$k_WMG1999_30,pch=19,col='green')
points(CD_swh$swh,CD_swh$k_W2014_30,pch=19,col='red')

cor.test(CD_swh$swh,CD_swh$k_MG2001_30, na.rm=T)

`#### k vs U plot ####
jpeg(file='figs/paper/k_vs_U.jpg',width=8,height=8,res=400, units = 'cm')
par(mar = c(3.5, 3.5, 0.5, 0.5))
plot(m2$wind_speed,k_N2000_30, xlim=c(0.5,5), ylim=c(0,10),
     pch = 19, cex = 0.5, xlab= '', ylab='')
points(m2$wind_speed, k_WMG1999_30, cex = 0.5, pch=19,ylab='k',col='green')
points(m2$wind_speed, k_MG2001_30, cex = 0.5, pch=19, ylab = 'k', col='blue')
points(m2$wind_speed, k_W2014_30, cex = 0.5, pch=19, ylab = 'k', col='red')

#lm1 <- lm(monthly_data$k_theory ~ monthly_data$U_insitu)
#abline(lm1, lwd=2, lty=2)
minor.tick()
mtext(expression(paste('k')),side = 2,line=2)
mtext(expression('U'),side = 1,line=2.3)
dev.off()

#### Month and year classification ####
Month_class <- NA 
for (i in 1:nrow(m2)) {
  if (m2$date[i] < as.POSIXct('2015-11-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2015-11'
  }
  if (m2$date[i] > as.POSIXct('2015-11-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2015-12-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2015-12'
  }
  if (m2$date[i] > as.POSIXct('2015-12-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2016-01-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2016-01'
  }
  if (m2$date[i] > as.POSIXct('2016-01-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2016-02-29 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2016-02'
  }
  if (m2$date[i] > as.POSIXct('2016-02-29 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2016-03-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2016-03'
  }
  if (m2$date[i] > as.POSIXct('2016-03-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2016-04-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2016-04'
  }
  if (m2$date[i] > as.POSIXct('2016-04-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2016-05-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2016-05'
  }
  if (m2$date[i] > as.POSIXct('2016-05-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2016-06-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2016-06'
  }
  if (m2$date[i] > as.POSIXct('2016-06-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2016-07-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2016-07'
  }
  if (m2$date[i] > as.POSIXct('2016-07-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2016-08-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2016-08'
  }
  if (m2$date[i] > as.POSIXct('2016-08-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2016-09-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2016-09'
  }
  if (m2$date[i] > as.POSIXct('2016-09-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2016-10-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2016-10'
  }
  if (m2$date[i] > as.POSIXct('2016-10-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2016-11-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2016-11'
  }
  if (m2$date[i] > as.POSIXct('2016-11-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2016-12-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2016-12'
  }
  if (m2$date[i] > as.POSIXct('2016-12-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2017-01-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2017-01'
  }
  if (m2$date[i] > as.POSIXct('2017-01-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2017-02-28 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2017-02'
  }
  if (m2$date[i] > as.POSIXct('2017-02-28 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2017-03-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2017-03'
  }
  if (m2$date[i] > as.POSIXct('2017-03-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2017-04-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2017-04'
  }
  if (m2$date[i] > as.POSIXct('2017-04-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2017-05-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2017-05'
  }
  if (m2$date[i] > as.POSIXct('2017-05-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2017-06-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2017-06'
  }
  if (m2$date[i] > as.POSIXct('2017-06-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2017-07-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2017-07'
  }
  if (m2$date[i] > as.POSIXct('2017-07-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2017-08-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2017-08'
  }
  if (m2$date[i] > as.POSIXct('2017-08-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2017-09-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2017-09'
  }
  if (m2$date[i] > as.POSIXct('2017-09-30 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur') &
      m2$date[i] < as.POSIXct('2017-10-31 23:30:00',
                              '%Y-%m-%d %H:%M:%S',
                              tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- '2017-10'
  }
}
Month_class <- as.factor(Month_class)

m2 <- cbind(m2,Month_class)

### Monthly boxplots of CO2 fluxes ####
jpeg(file='figs/paper/boxplot_Fco2_time.jpg',width=16,height=8,res=400, units = 'cm')
par(mar = c(2.1, 4.1, 0.6, 0.5))
boxplot(m2$co2_flux ~ m2$Month_class,
        outline=T, xlab='', ylab='', pch=19, cex=0.5) 
mtext(side = 2, expression(paste('F'['CO'['2']])), line = 2.1, cex = 1.5)
#lines(c(8,8),c(-1,5), lwd = 3, lty = 2)
minor.tick(nx=0)
dev.off()

### Monthly boxplots of U ####
jpeg(file='figs/paper/boxplot_u_time.jpg',width=16,height=8,res=400, units = 'cm')
par(mar = c(2.1, 4.1, 0.6, 0.5))
boxplot(m2$wind_speed ~ m2$Month_class,
        outline=T, xlab='', ylab='', pch=19, cex=0.5) 
mtext(side = 2, 'U', line = 2.1, cex = 1.5)
#lines(c(8,8),c(-1,5), lwd = 3, lty = 2)
minor.tick(nx=0)
dev.off()

### Monthly boxplots of u* ####
jpeg(file='figs/paper/boxplot_ustar_time.jpg',width=16,height=8,res=400, units = 'cm')
par(mar = c(2.1, 4.1, 0.6, 0.5))
boxplot(m2$u. ~ m2$Month_class,
        outline=T, xlab='', ylab='', pch=19, cex=0.5, ylim =c(0,0.3)) 
mtext(side = 2, expression(paste('u'['*'])), line = 2.1, cex = 1.5)
#lines(c(8,8),c(-1,5), lwd = 3, lty = 2)
minor.tick(nx=0)
dev.off()

### Monthly boxplots of swh ####
jpeg(file='figs/paper/boxplot_swh_time.jpg',width=16,height=8,res=400, units = 'cm')
par(mar = c(2.1, 4.1, 0.6, 0.5))
boxplot(m2$swh1 ~ m2$Month_class,
        outline=T, xlab='', ylab='', pch=19, cex=0.5) 
mtext(side = 2, expression(paste('H'['s'])), line = 2.1, cex = 1.5)
#lines(c(8,8),c(-1,5), lwd = 3, lty = 2)
minor.tick(nx=0)
dev.off()


### Monthly boxplots of swh ####
jpeg(file='figs/paper/boxplot_kmg2001_time.jpg',width=16,height=8,res=400, units = 'cm')
par(mar = c(2.1, 4.1, 0.6, 0.5))
boxplot(k_MG2001_30 ~ m2$Month_class,
        outline=T, xlab='', ylab='', pch=19, cex=0.5) 
mtext(side = 2, expression(paste('k')), line = 2.1, cex = 1.5)
#lines(c(8,8),c(-1,5), lwd = 3, lty = 2)
minor.tick(nx=0)
dev.off()

#### Plot flux versus k ####
jpeg(file='figs/paper/flux_vs_pCO2.jpg',width=8,height=8,res=400, units = 'cm')
par(mar = c(3.5, 3.5, 0.6, 0.5))
plot(PCO2_sw_30_MG2001,m2$co2_flux, xlim=c(200,600),ylim=c(-0.5,0.5), col  = 'blue', pch=19, 
     xlab='', ylab='', cex=0.25)
points(PCO2_sw_30_N2000,m2$co2_flux, col='black', pch = 19, cex = 0.25)
points(PCO2_sw_30_W2014,m2$co2_flux, col='red', pch = 19, cex = 0.25)
points(PCO2_sw_30_WMG1999,m2$co2_flux, col='green', pch = 19, cex = 0.25)

points(PP_air_30,m2$co2_flux,xlim=c(200,600), pch=19, cex=0.25, col='purple')

mtext(side = 1, expression(paste('p'['CO'['2']])), line = 2.1, cex = 1.5)
mtext(side = 2, expression(paste('F'['CO'['2']])), line = 2, cex = 1.5)
#lines(c(8,8),c(-1,5), lwd = 3, lty = 2)
minor.tick()
dev.off()

#### Plot FCO2 vs U ####
jpeg(file='figs/paper/flux_vs_U.jpg',width=8,height=8,res=400, units = 'cm')
par(mar = c(3.5, 3.5, 0.6, 0.5))
plot(m2$wind_speed, m2$co2_flux, pch=19, ylim =c(-0.5,0.5), cex = 0.5,
     ylab = '', xlab='')
mtext(side = 1, 'U', line = 2.1, cex = 1.5)
mtext(side = 2, expression(paste('F'['CO'['2']])), line = 2.0, cex = 1.5)
#lines(c(8,8),c(-1,5), lwd = 3, lty = 2)
minor.tick()
dev.off()


jpeg(file='figs/paper/timeSeries_pco2.jpg',width=16,height=8,res=400, units = 'cm')
par(mar = c(3.5, 3.5, 0.6, 0.5))
plot(monthly_data_pco2$month, monthly_data_pco2$PCO2_sw_30_MG2001, 
     ylim = c(250,475), col='blue', type = 'l', ylab='',xlab='',
     xaxt='n', lwd=2)
lines(monthly_data$month, monthly_data$PCO2_sw_zhu_sat, lwd=2)
lines(monthly_data$month, monthly_data$PP_air, col='purple', lwd=2)

mtext(side = 2, expression(paste('pCO'['2'])), line = 2.2, cex = 1) 
mtext(side = 1, 'Month', line = 2.5, cex = 1)

axis(side = 1, at = c(as.POSIXct('2015-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
     labels = c('Dec', 'Feb', 'Apr', 'Jun', 'Aug', 'Oct','Dec',
                'Feb', 'Apr', 'Jun', 'Aug','Oct'), 
     cex.axis = 1)
# 
# lines(c(as.POSIXct('2016-03-31 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#         as.POSIXct('2016-03-31 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
#       c(0,500), lwd = 1, lty = 4)
# lines(c(as.POSIXct('2016-05-30 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#         as.POSIXct('2016-05-30 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
#       c(0,500), lwd = 1, lty = 4)
# lines(c(as.POSIXct('2016-09-30 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#         as.POSIXct('2016-09-30 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
#       c(0,500), lwd = 1, lty = 4)
dev.off()


jpeg(file='figs/paper/timeSeries_pco2.jpg',width=16,height=8,res=400, units = 'cm')
par(mar = c(3.5, 3.5, 0.1, 3.5))
plot(monthly_data_pco2$month, monthly_data_pco2$PCO2_sw_30_MG2001, 
     ylim = c(250,475), col='blue', type = 'l', ylab='',xlab='',
     xaxt='n', lwd=2)
lines(monthly_data$month, monthly_data$PCO2_sw_zhu_sat, lwd=2)
lines(monthly_data$month, monthly_data$PP_air, col='purple', lwd=2)

mtext(side = 2, expression(paste('pCO'['2'])), line = 2.2, cex = 1) 
mtext(side = 1, 'Month', line = 2.5, cex = 1)

axis(side = 1, at = c(as.POSIXct('2015-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
     labels = c('Dec', 'Feb', 'Apr', 'Jun', 'Aug', 'Oct','Dec',
                'Feb', 'Apr', 'Jun', 'Aug','Oct'), 
     cex.axis = 1)
# 
# lines(c(as.POSIXct('2016-03-31 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#         as.POSIXct('2016-03-31 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
#       c(0,500), lwd = 1, lty = 4)
# lines(c(as.POSIXct('2016-05-30 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#         as.POSIXct('2016-05-30 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
#       c(0,500), lwd = 1, lty = 4)
# lines(c(as.POSIXct('2016-09-30 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#         as.POSIXct('2016-09-30 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
#       c(0,500), lwd = 1, lty = 4)
dev.off()


#### Monthly Time Series of SST and CHL A ####
jpeg(file='figs/paper/timeSeries_sst_chl.jpg',width=16,height=8,res=400, units = 'cm')
par(mar = c(3.5, 3.5, 0.1, 3.5))
plot(monthly_data$month, monthly_data$T_sat, 
     ylim = c(23,38), col='blue', type = 'l', ylab='',xlab='',
     xaxt='n', lwd=2)
minor.tick(nx=0)
mtext(side = 2, expression('SST'), line = 2.2, cex = 1) 
mtext(side = 1, 'Month', line = 2.5, cex = 1)

par(new = TRUE)
plot(monthly_data$month, monthly_data$chlor_a, lwd=2, col='green', axes = F, type = 'l',
     ylab= '', xlab='')

axis(4, cex.axis = 1)
mtext(side = 4, expression('chl a'), line = 2.2, cex = 1) 
axis(side = 1, at = c(as.POSIXct('2015-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2017-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
     labels = c('Dec', 'Feb', 'Apr', 'Jun', 'Aug', 'Oct','Dec',
                'Feb', 'Apr', 'Jun', 'Aug','Oct'), 
     cex.axis = 1)
# 
# lines(c(as.POSIXct('2016-03-31 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#         as.POSIXct('2016-03-31 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
#       c(0,500), lwd = 1, lty = 4)
# lines(c(as.POSIXct('2016-05-30 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#         as.POSIXct('2016-05-30 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
#       c(0,500), lwd = 1, lty = 4)
# lines(c(as.POSIXct('2016-09-30 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#         as.POSIXct('2016-09-30 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
#       c(0,500), lwd = 1, lty = 4)
dev.off()

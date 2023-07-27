
# After running co2_longTerm 2.R, this can be run.

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


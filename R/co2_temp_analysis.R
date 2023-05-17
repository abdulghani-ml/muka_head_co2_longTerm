ggplot(df,aes(DOY, TA)) + geom_point()

ggplot(df,aes(date, RH)) + geom_point()

ggplot(df, aes(DOY, EMA)) + geom_point()

ggplot(df_sat, aes(date, SST)) + geom_point()

write.table(data.frame(date=df$date,TA=df$TA,RH=df$RH),"data/station/TA_RH_EMA.csv",sep = ',')

summary(df$delT_ema)
summary(NEM_30$delT_ema)
summary(SWM_30$delT_ema)
summary(STM_30$EMA)
summary(FTM_30$EMA)

summary(NEM_30$CD)
summary(SWM_30$CD)
summary(STM_30$CD)
summary(FTM_30$CD)

t.test(NEM_30$FCO2, SWM_30$FCO2)


NEM_wave <- selectByDate(wave_30, month = c(12,1,2,3))
SWM_wave <- selectByDate(wave_30, month = c(6,7,8,9))
FTM_wave <- selectByDate(wave_30, month = c(10,11))
STM_wave <- selectByDate(wave_30, month = c(4,5))

summary(NEM_30$FCO2)
summary(SWM_30$FCO2)
summary(FTM_30$FCO2)
summary(STM_30$FCO2)

mean(df$FCO2,na.rm = T)
sd(df$FCO2,na.rm=T)
summary(df$FCO2)
summary(df$EMA)
summary(df$ZL)

boxplot(log10(df$CD))
sd(log10(df$CD),na.rm=T)

CD <- df$CD

summary(CD)

CD1 <- df$CD[which(df$CD < 1)]
sd(CD,na.rm = T)
mean(CD,na.rm=T)

df$CD[which(df$CD > 1)] <- NA

# Unit conversion calculations
-0.019 * (1000/1000000) * 60 * 60 * 24 
-0.019 * (1/1000000) * (12/1) * 60 * 60 * 24 * 365
2.6 * 10^9 * 1000 * 1000 / (3.613033414 * 10^14)
7.19 * 250 * 250 / 10^9
0.00045/2.6 * 100

mean(df_pos$FCO2[which(df_pos$ZL < 0)], na.rm=T)
mean(df_pos$FCO2[which(df_pos$ZL >= 0)], na.rm=T)

mean(df_neg$FCO2[which(df_pos$ZL < 0)], na.rm=T)
mean(df_neg$FCO2[which(df_pos$ZL >= 0)], na.rm=T)

boxplot(df_pos$FCO2[which(df_pos$ZL < 0)],df_pos$FCO2[which(df_pos$ZL >= 0)], ylim =c(0,3.2), 
        varwidth = T)

boxplot(df_neg$FCO2[which(df_pos$ZL < 0)],df_neg$FCO2[which(df_pos$ZL >= 0)], ylim=c(-0.3,0), 
        varwidth = T)

length(df_pos_neg$FCO2[which(df_pos_neg$ZL >= 0)])

length(df_pos_neg$FCO2[which(df_pos_neg$ZL < 0)])

mean(df_pos_neg$FCO2[which(df_pos_neg$ZL < 0)], na.rm=T)
mean(df_pos_neg$FCO2[which(df_pos_neg$ZL >= 0)], na.rm=T)

plot(df_pos$WS[which(df_pos$ZL>=0)],df_pos$FCO2[which(df_pos$ZL >= 0)],
     pch=19)

plot(df_neg$WS[which(df_neg$Z>=0)],df_neg$FCO2[which(df_neg$ZL >= 0)],
     pch=19)



summary(df_pos$EMA)
summary(df_neg$EMA)





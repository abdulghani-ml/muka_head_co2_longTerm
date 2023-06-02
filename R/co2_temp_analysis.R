library(tidyverse)
library(ggplot2)

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

#### 30-MIN TIMESCALE - Partitioning to seasons ####

Mar_May <- selectByDate(df, month = c(3,4,5))
Jun_Aug <- selectByDate(df, month = c(6,7,8))
Sep_Nov <- selectByDate(df, month = c(9,10,11))
Dec_Feb <- selectByDate(df, month = c(12,1,2))

summary(Mar_May$FCO2)
summary(Jun_Aug$FCO2)
summary(Sep_Nov$FCO2)
summary(Dec_Feb$FCO2)

summary(df_year$FCO2)

summary(Mar_May$FCO2)[4] * 86.4
summary(Jun_Aug$FCO2)[4] * 86.4
summary(Sep_Nov$FCO2)[4] * 86.4
summary(Dec_Feb$FCO2)[4] * 86.4

summary(df_year$FCO2)[4] * 86.4

#### Convert FCO2 from micro-mole per second to milli-mole per day ####
FCO2_mmol <- df$FCO2 * 86.4

#### Convert FCO2 from micro-mole per second to mole per year ####
(60 * 60 * 24 * 365) / 10^6


plot(df_hour$hour,df_hour$TS, ylim=c(10,40), pch=19, col='blue')
points(df_hour$hour,df_hour$TA, pch=19,col='red')
lines(df_hour$hour,df_hour$H * 15,col='red')

plot(df_hour$hour,df_hour$LE,pch=19,col='blue')

plot(df_hour$hour,df_hour$WS, ylim=c(0,1), pch=19, col='blue')
plot(df_hour$hour,df_hour$WD,pch=19,col='blue')

plot(df_hour$hour, df_hour$FCO2, pch=19,col='orange')

plot(df_hour$hour, df_hour$WS, pch=19,col='orange')


points(df_hour$hour,df_hour$TA, pch=19,col='red')
lines(df_hour$hour,df_hour$H * 15,col='red')


df_NEM_daily <- timeAverage(NEM_30,avg.time='day')
df_SWM_daily <- timeAverage(SWM_30,avg.time='day')


temp_df <- data.frame(FCO2 = df$FCO2, TA = df$TA_ema, TS = df$EMA, delT = df$delT, U = df$WS,
                      PPFD = df$PPFD, RG = df$RG, RN= df$RN)

corPlot(temp_df, method='spearman')

cor.test(temp_df$FCO2,temp_df$TS,method='spearman')

cor.test(temp_df$FCO2,temp_df$TA,method='spearman')
temp <- 29
Sc <- 2116.8 + (-136.25*temp) + 4.7353*(temp^2) + (-0.092307)*(temp^3) + 0.0007555*(temp^4)

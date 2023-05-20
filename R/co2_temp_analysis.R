library(tidyverse)
library(ggplot2)

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


#### Plot: Daily z/L versus DOY ####
jpeg(file='figs/daily_ZL_DOY.jpeg',width=10,height=8,res=350, units = 'cm')
DOY_ZL <- ggplot(df_daily, aes(x=DOY,y=ZL)) + 
  geom_line(alpha=0.2, color = "black") + 
  geom_smooth(aes(color ='#F8766D')) 
# Axes - labels
DOY_ZL <- DOY_ZL + coord_cartesian(ylim=c(-10,10))
DOY_ZL <- DOY_ZL + ylab(expression("z/L"~"[dimensionless]"))
DOY_ZL <- DOY_ZL + xlab(expression("DOY"))
DOY_ZL <- DOY_ZL + scale_color_identity(guide = "legend")
DOY_ZL

# Shaded area for SWM
DOY_ZL <- DOY_ZL + annotate("rect", xmin = 152, xmax = 273, ymin = -11, ymax = 11,
                            alpha = .1,fill = "#F8766D")

# Shaded area for NEM 1
DOY_ZL <- DOY_ZL + annotate("rect", xmin = 1, xmax = 90, ymin = -11, ymax = 11,
                            alpha = .1,fill = "#00BA38")
# Shaded area for NEM 2
DOY_ZL <- DOY_ZL + annotate("rect", xmin = 335, xmax = 365, ymin = -11, ymax = 11,
                            alpha = .1,fill = "#00BA38")

# Theme
DOY_ZL <-  DOY_ZL + theme_bw()
DOY_ZL
dev.off()
rm(DOY_ZL)

#### Plot: Daily Hourly FCO2 V2 ####
jpeg(file='figs/fco2_daily.jpeg',width=16,height=12,res=350, units = 'cm')
# Data 

temp_df_hour_pos <- data.frame(hour = df_hour_pos$hour,FCO2 = df_hour_pos$FCO2)
temp_df_hour_neg <- data.frame(hour = df_hour_neg$hour, FCO2 = df_hour_neg$FCO2)
temp_df_hour_pos_neg <- data.frame(hour = df_hour_pos_neg$hour, FCO2 = df_hour_pos_neg$FCO2)

temp_combined <- setNames(list(temp_df_hour_pos, temp_df_hour_neg, temp_df_hour_pos_neg), 
                          c("df_hour_pos","df_hour_neg","df_hour_pos_neg")) %>% 
  map_df(~ .x %>% gather(key, value, -hour), .id="source")


fco2_hour <- ggplot(temp_combined, aes(hour, value, group=source)) + 
  geom_point(aes(col=source)) + geom_smooth(aes(col=source),se = T, alpha = 0.2) +
  scale_color_manual(values=c("#F8766D","#00BA38","#808080"))


# Axes - limits
fco2_hour <- fco2_hour + coord_cartesian(ylim=c(-0.5,1.5)) 
# + 
#   scale_x_continuous(breaks=seq(0,23,1)) 

fco2_hour <- fco2_hour + ylab(expression("F"~CO[2]~"["*mu*mol~m^{-2}~s^{-1}*"]"))
fco2_hour <- fco2_hour + xlab("Hour")

# Theme
fco2_hour <- fco2_hour + theme_bw()

# Legend
fco2_hour <- fco2_hour + theme(legend.position = c(0.85,0.88)) + 
  scale_color_manual(name = NULL,
                     values = c('df_hour_pos' = '#F8766D',
                                'df_hour_neg' = "#00BA38",
                                'df_hour_pos_neg' = "#808080"),
                     labels = c(expression("–"*"F"~CO[2]), 
                                expression("+"*"F"~CO[2]),
                                expression("F"~CO[2])))

# Plot
fco2_hour

dev.off()
rm(fco2_hour, temp_combined, temp_df_hour_neg, temp_df_hour_pos, temp_df_hour_pos_neg)

#### Plot: Daily FCO2 with U  V1 ####
jpeg(file='figs/fco2_U_daily.jpeg',width=16,height=12,res=350, units = 'cm')
# Data 
fco2_hour <- ggplot(df_hour_pos, aes(as.integer(hour), FCO2)) + 
  geom_point(col="#F8766D") +
  geom_smooth(col = "#F8766D", se = F) 

fco2_hour <- fco2_hour +
  geom_point(data=df_hour_neg,aes(as.integer(hour),FCO2), col="#00BA38") +
  geom_smooth(data=df_hour_neg,aes(as.integer(hour),FCO2), col = "#00BA38", se=F)

fco2_hour <- fco2_hour +
  geom_point(data=df_hour_pos_neg, aes(as.integer(hour), FCO2), color="#808080", alpha=0.5) + 
  geom_smooth(data=df_hour_pos_neg, aes(as.integer(hour), FCO2), color="#808080", se=F)

fco2_hour <- fco2_hour + 
  geom_point(data=df_hour_pos,aes(as.integer(hour), WS), color='#F8766D',alpha=0.5) + 
  geom_line(stat='smooth',method='loess', linetype = "dashed",
            data=df_hour_pos,aes(as.integer(hour), WS), color = "#F8766D", se = FALSE,alpha=0.5)

fco2_hour <- fco2_hour + 
  geom_point(data=df_hour_neg,aes(as.integer(hour), WS), color='#00BA38',alpha=0.5) + 
  geom_line(stat='smooth',method='loess',linetype = "dashed",
            data=df_hour_neg,aes(as.integer(hour), WS), color = "#00BA38", se = FALSE,alpha=0.5)

fco2_hour <- fco2_hour +
  geom_point(data=df_hour_pos_neg, aes(as.integer(hour), WS), color="#808080", alpha=0.5) +
  geom_smooth(data=df_hour_pos_neg, aes(as.integer(hour), WS), color="#808080", se=F, alpha=0.5,
              linetype = "dashed")

# Axes - limits
fco2_hour <- fco2_hour + coord_cartesian(ylim=c(-0.5,2)) + 
  scale_x_continuous(breaks=seq(0,23,1)) 
# Axes - labels
fco2_hour <- fco2_hour + 
  scale_y_continuous(breaks = seq(-0.5,2,0.5), labels=seq(-0.5,2,0.5),
                     sec.axis = sec_axis(~.* 1,
                                         name=expression("U"~"["*m~s^{-1}*"]"),
                                         labels=seq(-0.5,2,0.5)))


fco2_hour <- fco2_hour + ylab(expression("F"~CO[2]~"["*mu*mol~m^{-2}~s^{-1}*"]"))
fco2_hour <- fco2_hour + xlab("Hour")


# Theme
fco2_hour <- fco2_hour + theme_bw()
fco2_hour

# Legend
# fco2_hour <- fco2_hour + scale_color_discrete(name = NULL,
#                                         breaks = c('Negative','Positive'),
#                                         labels = c(expression("–"*"F"~CO[2]), 
#                                                    expression("F"~CO[2]))) +
#   theme(legend.position = c(0.88,0.88))

# Plot
fco2_hour

dev.off()
rm(fco2_hour)

#### Plot: Daily FCO2 with z/L V1 ####
jpeg(file='figs/fco2_zL_daily.jpeg',width=16,height=12,res=350, units = 'cm')
# Data 
fco2_hour <- ggplot(df_hour_pos, aes(as.integer(hour), FCO2)) + 
  geom_point(col="#F8766D") +
  geom_smooth(col = "#F8766D", se = F) 

fco2_hour <- fco2_hour +
  geom_point(data=df_hour_neg,aes(as.integer(hour),FCO2), col="#00BA38") +
  geom_smooth(data=df_hour_neg,aes(as.integer(hour),FCO2), col = "#00BA38", se=F)

fco2_hour <- fco2_hour +
  geom_point(data=df_hour_pos_neg, aes(as.integer(hour), FCO2), color="#808080", alpha=0.5) + 
  geom_smooth(data=df_hour_pos_neg, aes(as.integer(hour), FCO2), color="#808080", se=F)

fco2_hour <- fco2_hour + 
  geom_point(data=df_hour_pos,aes(as.integer(hour), ZL*0.5),color='#F8766D',alpha=0.5) + 
  geom_line(stat='smooth',method='loess',linetype = "dashed",
            data=df_hour_pos,aes(as.integer(hour), ZL*0.5),col = "#F8766D", se = FALSE,alpha=0.5)

fco2_hour <- fco2_hour + 
  geom_point(data=df_hour_neg,aes(as.integer(hour), ZL*0.5),color='#00BA38',alpha=0.5) + 
  geom_line(stat='smooth',method='loess',linetype = "dashed",
            data=df_hour_neg,aes(as.integer(hour), ZL*0.5),col = "#00BA38", se = FALSE,alpha=0.5)

fco2_hour <- fco2_hour +
  geom_point(data=df_hour_pos_neg, aes(as.integer(hour), ZL*0.5),color="#808080",alpha=0.5) +
  geom_smooth(data=df_hour_pos_neg, aes(as.integer(hour), ZL*0.5),color="#808080",se=F,
              linetype = "dashed",)

# Axes - limits
fco2_hour <- fco2_hour + coord_cartesian(ylim=c(-2,2)) + 
  scale_x_continuous(breaks=seq(0,23,1)) 
# Axes - labels
fco2_hour <- fco2_hour + 
  scale_y_continuous(breaks = seq(-2,2,0.5), labels=seq(-2,2,0.5),
                     sec.axis = sec_axis(~.* 0.5, 
                                         name="z/L [dimensionless]",
                                         labels=seq(-4,4,2)))


fco2_hour <- fco2_hour + ylab(expression("F"~CO[2]~"["*mu*mol~m^{-2}~s^{-1}*"]"))
fco2_hour <- fco2_hour + xlab("Hour")


# Theme
fco2_hour <- fco2_hour + theme_bw()
fco2_hour

# Legend
# fco2_hour <- fco2_hour + scale_color_discrete(name = NULL,
#                                         breaks = c('Negative','Positive'),
#                                         labels = c(expression("–"*"F"~CO[2]), 
#                                                    expression("F"~CO[2]))) +
#   theme(legend.position = c(0.88,0.88))

# Plot
fco2_hour

dev.off()
rm(fco2_hour)

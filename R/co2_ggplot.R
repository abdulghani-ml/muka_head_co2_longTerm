#### PACKAGES ####
library(ggplot2)
library(dplyr)

#### COLOR ####
col_sample <- c('#F8766D','#00BA38')

#### AVERAGING ####
df_daily <- timeAverage(df,avg.time='day')

df_2016 <- selectByDate(df_daily,year=2016)
df_2017 <- selectByDate(df_daily,year=2017)
df_2018 <- selectByDate(df_daily,year=2018)
df_2019 <- selectByDate(df_daily,year=2019)
df_2020 <- selectByDate(df_daily,year=2020)

df_weekly <- timeAverage(df,avg.time='week')

df_day <- selectByDate(df,hour = c(7:19))
df_night <- selectByDate(df,hour = c(20,6))

pos_or_neg <- vector(length = nrow(df))
# Categorizing into positive or negative flux
for (i in 1:nrow(df)){
  if (is.na(df$FCO2[i])) {
    pos_or_neg[i] <- NA
  } else if (df$FCO2[i] >= 0) {
    pos_or_neg[i] <- 'Positive'
  } else if (df$FCO2[i] < 0) {
    pos_or_neg[i] <- 'Negative'
  } 
}
rm(i)

df <- cbind(df,pos_or_neg)
rm(pos_or_neg)


#### CATEGORIZE ACCORDING TO WIND CLASSES ####
cat_WS <- cut(df$WS,
              breaks=c(0,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2,2.25,2.5,2.75,3,4))

cat_WS1 <- cut(df$WS,
              breaks=c(0,1,2,3,4))

df <- cbind(df,cat_WS,cat_WS1)
rm(cat_WS,cat_WS1)

#### GROUP ACCORDING TO HOUR ####

# Overall #
df_hour <- df %>% 
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WD = mean(WD,na.rm=TRUE),
            RN = mean(RN,na.rm=TRUE), RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
            EMA = mean(EMA,na.rm=TRUE),delT = mean(delT,na.rm=TRUE))


df_pos <- filter(df, pos_or_neg == "Positive")
df_neg <- filter(df, pos_or_neg == "Negative")
df_pos_neg <- filter(df, pos_or_neg == "Positive" | pos_or_neg == "Negative")

# Positive FCO2 #
df_hour_pos <- df_pos %>% 
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WD = mean(WD,na.rm=TRUE),
            RN = mean(RN,na.rm=TRUE), RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
            EMA = mean(EMA,na.rm=TRUE),delT = mean(delT,na.rm=TRUE))


# Negative FCO2 #
df_hour_neg <- df_neg %>% 
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WD = mean(WD,na.rm=TRUE),
            RN = mean(RN,na.rm=TRUE), RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
            EMA = mean(EMA,na.rm=TRUE),delT = mean(delT,na.rm=TRUE))

# Positive and Negative FCO2 #
df_hour_pos_neg <- df_pos_neg %>% 
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WD = mean(WD,na.rm=TRUE),
            RN = mean(RN,na.rm=TRUE), RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
            EMA = mean(EMA,na.rm=TRUE),delT = mean(delT,na.rm=TRUE))

# Daily trend of the stable stability parameter
df_stable <- filter(df_pos_neg, ZL >= 0)
df_hour_stable <- df_stable %>% 
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WD = mean(WD,na.rm=TRUE),
            RN = mean(RN,na.rm=TRUE), RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
            EMA = mean(EMA,na.rm=TRUE),delT = mean(delT,na.rm=TRUE))

# Daily trend of the unstable stability parameter
df_unstable <- filter(df_pos_neg, ZL < 0)
df_hour_unstable <- df_unstable %>% 
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WD = mean(WD,na.rm=TRUE),
            RN = mean(RN,na.rm=TRUE), RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
            EMA = mean(EMA,na.rm=TRUE),delT = mean(delT,na.rm=TRUE))


#### Group and analyze solubility and salinity data ####
NEM_solub <- selectByDate(df_salinity_sat, month = c(12,1,2,3))
SWM_solub <- selectByDate(df_salinity_sat, month = c(6,7,8,9))
FTM_solub <- selectByDate(df_salinity_sat, month = c(10,11))
STM_solub <- selectByDate(df_salinity_sat, month = c(4,5))

summary(NEM_solub$solub)
summary(SWM_solub$solub)
summary(FTM_solub$solub)
summary(STM_solub$solub)

summary(NEM_solub$SST)
summary(SWM_solub$SST)
summary(FTM_solub$SST)
summary(STM_solub$SST)

summary(NEM_solub$salinity)
mean(NEM_solub$salinity,na.rm = T)
sd(NEM_solub$salinity,na.rm = T)
summary(STM_solub$salinity)
mean(STM_solub$salinity,na.rm = T)
sd(STM_solub$salinity,na.rm = T)
summary(SWM_solub$salinity)
mean(SWM_solub$salinity,na.rm = T)
sd(SWM_solub$salinity,na.rm = T)
summary(FTM_solub$salinity)
mean(FTM_solub$salinity,na.rm = T)
sd(FTM_solub$salinity,na.rm = T)


summary(NEM_solub$solub)
mean(NEM_solub$solub,na.rm = T)
sd(NEM_solub$solub,na.rm = T)
summary(STM_solub$solub)
mean(STM_solub$solub,na.rm = T)
sd(STM_solub$solub,na.rm = T)
summary(SWM_solub$solub)
mean(SWM_solub$solub,na.rm = T)
sd(SWM_solub$solub,na.rm = T)
summary(FTM_solub$solub)
mean(FTM_solub$solub,na.rm = T)
sd(FTM_solub$solub,na.rm = T)

####

# #### Plot: Daily FCO2 with delta T V1 ####
# jpeg(file='figs/fco2_delT_daily.jpeg',width=16,height=12,res=350, units = 'cm')
# # Data 
# fco2_hour <- ggplot(df_hour_pos, aes(as.integer(hour), FCO2)) + 
#   geom_point(col="#F8766D") +
#   geom_smooth(col = "#F8766D", se = F) 
# 
# fco2_hour <- fco2_hour +
#   geom_point(data=df_hour_neg,aes(as.integer(hour),FCO2), col="#00BA38") +
#   geom_smooth(data=df_hour_neg,aes(as.integer(hour),FCO2), col = "#00BA38", se=F)
# 
# fco2_hour <- fco2_hour +
#   geom_point(data=df_hour_pos_neg, aes(as.integer(hour), FCO2), color="#808080", alpha=0.5) + 
#   geom_smooth(data=df_hour_pos_neg, aes(as.integer(hour), FCO2), color="#808080", se=F)
# 
# fco2_hour <- fco2_hour + 
#   geom_point(data=df_hour_pos,aes(as.integer(hour), delT), color='#F8766D',alpha=0.5) + 
#   geom_line(stat='smooth',method='loess', linetype = "dashed",
#             data=df_hour_pos,aes(as.integer(hour), delT), color = "#F8766D", se = FALSE,alpha=0.5)
# 
# fco2_hour <- fco2_hour + 
#   geom_point(data=df_hour_neg,aes(as.integer(hour), delT), color='#00BA38',alpha=0.5) + 
#   geom_line(stat='smooth',method='loess',linetype = "dashed",
#             data=df_hour_neg,aes(as.integer(hour), delT), color = "#00BA38", se = FALSE,alpha=0.5)
# 
# fco2_hour <- fco2_hour +
#   geom_point(data=df_hour_pos_neg, aes(as.integer(hour), delT), color="#808080", alpha=0.5) +
#   geom_smooth(data=df_hour_pos_neg, aes(as.integer(hour), delT), color="#808080", se=F, alpha=0.5,
#               linetype = "dashed")
# 
# # Axes - limits
# fco2_hour <- fco2_hour + coord_cartesian(ylim=c(-0.5,3)) + 
#   scale_x_continuous(breaks=seq(0,23,1)) 
# # Axes - labels
# fco2_hour <- fco2_hour + 
#   scale_y_continuous(breaks = seq(-0.5,3,0.5), labels=seq(-0.5,3,0.5),
#                      sec.axis = sec_axis(~.* 1,
#                                          name=expression(paste(ΔT," ","[","\u{00B0}","C]")),
#                                          breaks = seq(-0.5,3,0.5),
#                                          labels=seq(-0.5,3,0.5)))
# 
# fco2_hour <- fco2_hour + ylab(expression("F"~CO[2]~"["*mu*mol~m^{-2}~s^{-1}*"]"))
# fco2_hour <- fco2_hour + xlab("Hour")
# 
# 
# # Theme
# fco2_hour <- fco2_hour + theme_bw()
# fco2_hour
# 
# # Legend
# # fco2_hour <- fco2_hour + scale_color_discrete(name = NULL,
# #                                         breaks = c('Negative','Positive'),
# #                                         labels = c(expression("–"*"F"~CO[2]), 
# #                                                    expression("F"~CO[2]))) +
# #   theme(legend.position = c(0.88,0.88))
# 
# # Plot
# fco2_hour
# 
# dev.off()
# rm(fco2_hour)
# 
# #### Plot: Daily FCO2 with U  V1 ####
# jpeg(file='figs/fco2_U_daily.jpeg',width=16,height=12,res=350, units = 'cm')
# # Data 
# fco2_hour <- ggplot(df_hour_pos, aes(as.integer(hour), FCO2)) + 
#   geom_point(col="#F8766D") +
#   geom_smooth(col = "#F8766D", se = F) 
# 
# fco2_hour <- fco2_hour +
#   geom_point(data=df_hour_neg,aes(as.integer(hour),FCO2), col="#00BA38") +
#   geom_smooth(data=df_hour_neg,aes(as.integer(hour),FCO2), col = "#00BA38", se=F)
# 
# fco2_hour <- fco2_hour +
#   geom_point(data=df_hour_pos_neg, aes(as.integer(hour), FCO2), color="#808080", alpha=0.5) + 
#   geom_smooth(data=df_hour_pos_neg, aes(as.integer(hour), FCO2), color="#808080", se=F)
# 
# fco2_hour <- fco2_hour + 
#   geom_point(data=df_hour_pos,aes(as.integer(hour), WS), color='#F8766D',alpha=0.5) + 
#   geom_line(stat='smooth',method='loess', linetype = "dashed",
#             data=df_hour_pos,aes(as.integer(hour), WS), color = "#F8766D", se = FALSE,alpha=0.5)
# 
# fco2_hour <- fco2_hour + 
#   geom_point(data=df_hour_neg,aes(as.integer(hour), WS), color='#00BA38',alpha=0.5) + 
#   geom_line(stat='smooth',method='loess',linetype = "dashed",
#             data=df_hour_neg,aes(as.integer(hour), WS), color = "#00BA38", se = FALSE,alpha=0.5)
# 
# fco2_hour <- fco2_hour +
#   geom_point(data=df_hour_pos_neg, aes(as.integer(hour), WS), color="#808080", alpha=0.5) +
#   geom_smooth(data=df_hour_pos_neg, aes(as.integer(hour), WS), color="#808080", se=F, alpha=0.5,
#               linetype = "dashed")
# 
# # Axes - limits
# fco2_hour <- fco2_hour + coord_cartesian(ylim=c(-0.5,2)) + 
#   scale_x_continuous(breaks=seq(0,23,1)) 
# # Axes - labels
# fco2_hour <- fco2_hour + 
#   scale_y_continuous(breaks = seq(-0.5,2,0.5), labels=seq(-0.5,2,0.5),
#                                             sec.axis = sec_axis(~.* 1,
#                                                                 name=expression("U"~"["*m~s^{-1}*"]"),
#                                                                 labels=seq(-0.5,2,0.5)))
#                                                                 
#                                                                 
# fco2_hour <- fco2_hour + ylab(expression("F"~CO[2]~"["*mu*mol~m^{-2}~s^{-1}*"]"))
# fco2_hour <- fco2_hour + xlab("Hour")
# 
# 
# # Theme
# fco2_hour <- fco2_hour + theme_bw()
# fco2_hour
# 
# # Legend
# # fco2_hour <- fco2_hour + scale_color_discrete(name = NULL,
# #                                         breaks = c('Negative','Positive'),
# #                                         labels = c(expression("–"*"F"~CO[2]), 
# #                                                    expression("F"~CO[2]))) +
# #   theme(legend.position = c(0.88,0.88))
# 
# # Plot
# fco2_hour
# 
# dev.off()
# rm(fco2_hour)
# 
# #### Plot: Daily FCO2 with z/L V1 ####
# jpeg(file='figs/fco2_zL_daily.jpeg',width=16,height=12,res=350, units = 'cm')
# # Data 
# fco2_hour <- ggplot(df_hour_pos, aes(as.integer(hour), FCO2)) + 
#   geom_point(col="#F8766D") +
#   geom_smooth(col = "#F8766D", se = F) 
# 
# fco2_hour <- fco2_hour +
#   geom_point(data=df_hour_neg,aes(as.integer(hour),FCO2), col="#00BA38") +
#   geom_smooth(data=df_hour_neg,aes(as.integer(hour),FCO2), col = "#00BA38", se=F)
# 
# fco2_hour <- fco2_hour +
#   geom_point(data=df_hour_pos_neg, aes(as.integer(hour), FCO2), color="#808080", alpha=0.5) + 
#   geom_smooth(data=df_hour_pos_neg, aes(as.integer(hour), FCO2), color="#808080", se=F)
# 
# fco2_hour <- fco2_hour + 
#   geom_point(data=df_hour_pos,aes(as.integer(hour), ZL*0.5),color='#F8766D',alpha=0.5) + 
#   geom_line(stat='smooth',method='loess',linetype = "dashed",
#             data=df_hour_pos,aes(as.integer(hour), ZL*0.5),col = "#F8766D", se = FALSE,alpha=0.5)
# 
# fco2_hour <- fco2_hour + 
#   geom_point(data=df_hour_neg,aes(as.integer(hour), ZL*0.5),color='#00BA38',alpha=0.5) + 
#   geom_line(stat='smooth',method='loess',linetype = "dashed",
#             data=df_hour_neg,aes(as.integer(hour), ZL*0.5),col = "#00BA38", se = FALSE,alpha=0.5)
# 
# fco2_hour <- fco2_hour +
#   geom_point(data=df_hour_pos_neg, aes(as.integer(hour), ZL*0.5),color="#808080",alpha=0.5) +
#   geom_smooth(data=df_hour_pos_neg, aes(as.integer(hour), ZL*0.5),color="#808080",se=F,
#               linetype = "dashed",)
# 
# # Axes - limits
# fco2_hour <- fco2_hour + coord_cartesian(ylim=c(-2,2)) + 
#   scale_x_continuous(breaks=seq(0,23,1)) 
# # Axes - labels
# fco2_hour <- fco2_hour + 
#   scale_y_continuous(breaks = seq(-2,2,0.5), labels=seq(-2,2,0.5),
#                      sec.axis = sec_axis(~.* 0.5, 
#                                          name="z/L [dimensionless]",
#                                          labels=seq(-4,4,2)))
# 
# 
# fco2_hour <- fco2_hour + ylab(expression("F"~CO[2]~"["*mu*mol~m^{-2}~s^{-1}*"]"))
# fco2_hour <- fco2_hour + xlab("Hour")
# 
# 
# # Theme
# fco2_hour <- fco2_hour + theme_bw()
# fco2_hour
# 
# # Legend
# # fco2_hour <- fco2_hour + scale_color_discrete(name = NULL,
# #                                         breaks = c('Negative','Positive'),
# #                                         labels = c(expression("–"*"F"~CO[2]), 
# #                                                    expression("F"~CO[2]))) +
# #   theme(legend.position = c(0.88,0.88))
# 
# # Plot
# fco2_hour
# 
# dev.off()
# rm(fco2_hour)
# 

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

#### Plot: Daily Hourly z/L V2 ####
jpeg(file='figs/zL_daily.jpeg',width=16,height=12,res=350, units = 'cm')
# Data 

temp_df_hour_pos <- data.frame(hour = df_hour_pos$hour, ZL = df_hour_pos$ZL)
temp_df_hour_neg <- data.frame(hour = df_hour_neg$hour, ZL = df_hour_neg$ZL)
temp_df_hour_pos_neg <- data.frame(hour = df_hour_pos_neg$hour, ZL = df_hour_pos_neg$ZL)

temp_combined <- setNames(list(temp_df_hour_pos, temp_df_hour_neg, temp_df_hour_pos_neg), 
                          c("df_hour_pos","df_hour_neg","df_hour_pos_neg")) %>% 
  map_df(~ .x %>% gather(key, value, -hour), .id="source")


fco2_hour <- ggplot(temp_combined, aes(hour, value, group=source)) + 
  geom_point(aes(col=source)) + geom_smooth(aes(col=source),se = T, alpha = 0.2) +
  scale_color_manual(values=c("#F8766D","#00BA38","#808080"))


# Axes - limits
fco2_hour <- fco2_hour + coord_cartesian(ylim=c(-3.5,0.5)) 


fco2_hour <- fco2_hour + ylab('z/L [dimensionless]')
fco2_hour <- fco2_hour + xlab("Hour")

# Theme
fco2_hour <- fco2_hour + theme_bw()

# Legend
fco2_hour <- fco2_hour + theme(legend.position = c(0.85,0.18)) + 
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

#### Plot: Daily Hourly U V2 ####
jpeg(file='figs/U_daily.jpeg',width=16,height=12,res=350, units = 'cm')
# Data 

temp_df_hour_pos <- data.frame(hour = df_hour_pos$hour, U = df_hour_pos$WS)
temp_df_hour_neg <- data.frame(hour = df_hour_neg$hour, U = df_hour_neg$WS)
temp_df_hour_pos_neg <- data.frame(hour = df_hour_pos_neg$hour, U = df_hour_pos_neg$WS)

temp_combined <- setNames(list(temp_df_hour_pos, temp_df_hour_neg, temp_df_hour_pos_neg), 
                          c("df_hour_pos","df_hour_neg","df_hour_pos_neg")) %>% 
  map_df(~ .x %>% gather(key, value, -hour), .id="source")


fco2_hour <- ggplot(temp_combined, aes(hour, value, group=source)) + 
  geom_point(aes(col=source)) + geom_smooth(aes(col=source),se = T, alpha = 0.2) +
  scale_color_manual(values=c("#F8766D","#00BA38","#808080"))


# Axes - limits
fco2_hour <- fco2_hour + coord_cartesian(ylim=c(0,2)) 


fco2_hour <- fco2_hour + ylab(expression("U"~"["*m~s^{-1}*"]"))
fco2_hour <- fco2_hour + xlab("Hour")

# Theme
fco2_hour <- fco2_hour + theme_bw() + guides(color="none")

# Plot
fco2_hour

dev.off()
rm(fco2_hour, temp_combined, temp_df_hour_neg, temp_df_hour_pos, temp_df_hour_pos_neg)

#### Plot: Daily Hourly deltaT V2 ####
jpeg(file='figs/delT_daily.jpeg',width=16,height=12,res=350, units = 'cm')
# Data 

temp_df_hour_pos <- data.frame(hour = df_hour_pos$hour, delT = df_hour_pos$delT)
temp_df_hour_neg <- data.frame(hour = df_hour_neg$hour, delT = df_hour_neg$delT)
temp_df_hour_pos_neg <- data.frame(hour = df_hour_pos_neg$hour, delT = df_hour_pos_neg$delT)

temp_combined <- setNames(list(temp_df_hour_pos, temp_df_hour_neg, temp_df_hour_pos_neg), 
                          c("df_hour_pos","df_hour_neg","df_hour_pos_neg")) %>% 
  map_df(~ .x %>% gather(key, value, -hour), .id="source")


fco2_hour <- ggplot(temp_combined, aes(hour, value, group=source)) + 
  geom_point(aes(col=source)) + geom_smooth(aes(col=source),se = T, alpha = 0.2) +
  scale_color_manual(values=c("#F8766D","#00BA38","#808080"))


# Axes - limits
fco2_hour <- fco2_hour + coord_cartesian(ylim=c(0,3)) 


fco2_hour <- fco2_hour + ylab(expression(paste(ΔT," ","[","\u{00B0}","C]")))
fco2_hour <- fco2_hour + xlab("Hour")

# Theme
fco2_hour <- fco2_hour + theme_bw() + guides(color="none")


# Plot
fco2_hour

dev.off()
rm(fco2_hour, temp_combined, temp_df_hour_neg, temp_df_hour_pos, temp_df_hour_pos_neg)

#### Correlation test ####
cor.test(df_co2_neg$WS[neg_05],df_co2_neg$FCO2[neg_05])
cor.test(df_co2_pos$WS[pos_05],df_co2_pos$FCO2[pos_05])
cor.test(df$WS[which(df$WS >= 2)],abs(df$FCO2[which(df$WS >= 2)]))

cor.test(df$ZL[which(df$ZL <=0)],abs(df$FCO2[which(df$ZL<=0)]))

cor.test(df$ZL[which(df$ZL >=0)],abs(df$FCO2[which(df$ZL>=0)]))

cor.test(df_night$WS[which(df_night$WS>0.1)],abs(df_night$FCO2[which(df_night$WS>0.1)]))

cor.test(df_daylight$WS[which(df_daylight$WS>0.1)],abs(df_daylight$FCO2[which(df_daylight$WS>0.1)]))

cor.test(df$air_pressure,df$FCO2)

cor.test(df$TS,df$FCO2)

cor.test(df$TA,df$FCO2)
cor.test(df$EMA,df$ZL)

mean(df$FCO2[which(df$ZL < 0)], na.rm=TRUE)
mean(df$FCO2[which(df$ZL >= 0)], na.rm=TRUE)

#### Plot: FCO2 versus U ####
jpeg(file='figs/fco2_u.jpeg',width=12,height=12,res=350, units = 'cm')
# Data 
FCO2_U <- ggplot(df, aes(x=WS,y=abs(FCO2),color=pos_or_neg)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess", level = 0.95)

# Data
FCO2_U <- FCO2_U + geom_smooth(method="loess",level=0.95,data=df, color ='black')

# Axes - limits
FCO2_U <- FCO2_U + coord_cartesian(xlim = c(0,3.0), ylim = c(0,2)) + 
  scale_x_continuous(breaks=seq(0,3,0.5)) 
# Axes - labels
FCO2_U <- FCO2_U + scale_y_continuous(breaks=seq(0,2,0.2))
FCO2_U <- FCO2_U + ylab(expression("F"~CO[2]~"["*mu*mol~m^{-2}~s^{-1}*"]"))
FCO2_U <- FCO2_U + xlab(expression("U"~"["*m~s^{-1}*"]"))
# Theme
FCO2_U <- FCO2_U + theme_bw()

# Legend
FCO2_U <- FCO2_U + scale_color_discrete(name = NULL,
                                        breaks = c('Negative','Positive'),
                                        labels = c(expression("–"*"F"~CO[2]), 
                                        expression("F"~CO[2]))) +
  theme(legend.position = c(0.88,0.88))

# Plot
FCO2_U

dev.off()
rm(FCO2_U)
####

#### Plot: FCO2 versus z/L ####
jpeg(file='figs/fco2_ZL.jpeg',width=12,height=12,res=350, units = 'cm')
# Data
FCO2_ZL <- ggplot(df, aes(ZL,abs(FCO2), color = pos_or_neg)) + geom_point(alpha = 0.2) +
  geom_smooth(method="loess", level = 0.95)
# Data
FCO2_ZL <- FCO2_ZL + geom_smooth(method="loess",level=0.95,data=df, color ='black')

# Color palette
FCO2_ZL <- FCO2_ZL + scale_color_discrete(name = NULL,
                                          breaks = c('Negative','Positive'),
                                          labels = c(expression("–"*"F"~CO[2]), 
                                                     expression("F"~CO[2])))

# Axes - limits
FCO2_ZL <- FCO2_ZL + coord_cartesian(ylim = c(0,2),xlim = c(-5,5)) + 
  scale_x_continuous(breaks=seq(-5,5,1))

# Axes - labels
FCO2_ZL <- FCO2_ZL + ylab(expression("F"~CO[2]~"["*mu*mol~m^{-2}~s^{-1}*"]"))
FCO2_ZL <- FCO2_ZL + xlab(expression("z/L [dimensionless]"))
# Theme
FCO2_ZL <- FCO2_ZL + theme_bw()

# Legend
FCO2_ZL <- FCO2_ZL + theme(legend.position = c(0.85,0.20))

FCO2_ZL
dev.off()
rm(FCO2_ZL)
####




#### Plot: delT versus z/L ####
jpeg(file='figs/delT_ZL.jpeg',width=12,height=12,res=350, units = 'cm')
# Data
delT_ZL <- ggplot(df_pos_neg, aes(ZL,delT, color = pos_or_neg)) + geom_point(alpha = 0.2) +
  geom_smooth(method="loess", level = 0.95)

# Data
delT_ZL <- delT_ZL + geom_smooth(method="loess",level=0.95,data=df_pos_neg, color ='black')

# Color palette
delT_ZL <- delT_ZL + scale_color_discrete(name = NULL,
                                          breaks = c('Negative','Positive'),
                                          labels = c(expression("–"*"F"~CO[2]), 
                                                     expression("F"~CO[2])))

# Axes - limits
delT_ZL <- delT_ZL + coord_cartesian(ylim = c(-2,2.5),xlim = c(-5,5)) + 
  scale_x_continuous(breaks=seq(-5,5,1))

# Axes - labels
delT_ZL <- delT_ZL + ylab(expression(paste("ΔT"," ","[","\u{00B0}","C]")))
delT_ZL <- delT_ZL + xlab(expression("z/L [dimensionless]"))
# Theme
delT_ZL <- delT_ZL + theme_bw()

# Legend
delT_ZL <- delT_ZL + theme(legend.position = c(0.85,0.20))

delT_ZL
dev.off()
rm(delT_ZL)
####
#### Plot: U versus z/L ####
library(dplyr)
jpeg(file='figs/WS_ZL.jpeg',width=12,height=12,res=350, units = 'cm')
WS_ZL <- ggplot(filter(df, pos_or_neg != "NA"), aes(ZL,WS,color=pos_or_neg)) + geom_point(alpha=0.2) + 
  geom_smooth()

# Color palette
WS_ZL <- WS_ZL + scale_color_discrete(name = NULL,
                                          breaks = c('Negative','Positive'),
                                          labels = c(expression("–"*"F"~CO[2]), 
                                                     expression("F"~CO[2])))

# Axes - limits
WS_ZL <- WS_ZL + coord_cartesian(ylim = c(-0.1,3),xlim = c(-5,5)) + 
  scale_x_continuous(breaks=seq(-5,5,1))

# Axes - labels
WS_ZL <- WS_ZL + ylab(expression("U"~"[m"~s^{-1}*"]"))
WS_ZL <- WS_ZL + xlab(expression("z/L [dimensionless]"))
# Theme
WS_ZL <- WS_ZL + theme_bw()

# Legend
WS_ZL <- WS_ZL + theme(legend.position = c(0.85,0.85))



WS_ZL
dev.off()
rm(WS_ZL)


#### Plot: FCO2 versus CD ####
jpeg(file='figs/fco2_CD.jpeg',width=12,height=12,res=350, units = 'cm')
FCO2_CD <- ggplot(filter(df, pos_or_neg != "NA"), aes(log10(CD),abs(FCO2),color = pos_or_neg)) + 
  geom_point(alpha=0.2) + 
  geom_smooth()

# Data
FCO2_CD <- FCO2_CD + geom_smooth(method="loess",level=0.95,data=df, color ='black')

# Color palette
FCO2_CD <- FCO2_CD + scale_color_discrete(name = NULL,
                                          breaks = c('Negative','Positive'),
                                          labels = c(expression("–"*"F"~CO[2]), 
                                                     expression("F"~CO[2])))
# Axes - limits
FCO2_CD <- FCO2_CD + coord_cartesian(ylim=c(0,1),xlim=c(-4,-1)) 

# Axes - labels
FCO2_CD <- FCO2_CD + ylab(expression("F"~CO[2]~"["*mu*mol~m^{-2}~s^{-1}*"]"))
FCO2_CD <- FCO2_CD + xlab(expression(log[10]*"("*C[D]*")"~"[dimensionless]"))

# Theme
FCO2_CD <-  FCO2_CD + theme_bw()

# Legend
FCO2_CD <- FCO2_CD + theme(legend.position = c(0.15,0.85))

FCO2_CD
dev.off()
rm(FCO2_CD)
####

#### Plot: FCO2 versus TS ####
jpeg(file='figs/fco2_ts.jpeg',width=12,height=12,res=350, units = 'cm')
# Data 
FCO2_TS <- ggplot(df, aes(x=EMA,y=abs(FCO2),color=pos_or_neg)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess", level = 0.95)
FCO2_TS
# Data
FCO2_TS <- FCO2_TS + geom_smooth(method="loess",level=0.95,data=df, color ='black')
FCO2_TS
# Axes - limits
FCO2_TS <- FCO2_TS + coord_cartesian( ylim = c(0,1)) + 
  scale_x_continuous() 
FCO2_TS
# Axes - labels
FCO2_TS <- FCO2_TS + scale_y_continuous(breaks=seq(0,2,0.2))
FCO2_TS <- FCO2_TS + ylab(expression("F"~CO[2]~"["*mu*mol~m^{-2}~s^{-1}*"]"))
FCO2_TS <- FCO2_TS + xlab(expression(paste(T[S]," ","[","\u{00B0}","C]")))
# Theme
FCO2_TS <- FCO2_TS + theme_bw()

# Legend
FCO2_TS <- FCO2_TS + scale_color_discrete(name = NULL,
                                        breaks = c('Negative','Positive'),
                                        labels = c(expression("–"*"F"~CO[2]), 
                                                   expression("F"~CO[2]))) +
  theme(legend.position = c(0.88,0.88))

# Plot
FCO2_TS

dev.off()
rm(FCO2_TS)
####

#### Plot: FCO2 versus DOY ####
jpeg(file='figs/fco2_DOY.jpeg',width=16,height=10,res=350, units = 'cm')
DOY_FCO2 <- ggplot(df, aes(x=DOY,y=abs(FCO2))) + 
  geom_point(alpha=0.1) + 
  geom_smooth(method="loess",level=0.95, data=df, color ='orange') 
# Axes - labels
DOY_FCO2 <- DOY_FCO2 + coord_cartesian(ylim=c(0,1))
DOY_FCO2 <- DOY_FCO2 + ylab(expression("F"~CO[2]~"["*mu*mol~m^{-2}~s^{-1}*"]"))
DOY_FCO2 <- DOY_FCO2 + xlab(expression("DOY"))

# Theme
DOY_FCO2 <-  DOY_FCO2 + theme_bw()
DOY_FCO2
dev.off()
rm(DOY_FCO2)
####

#### Plot: z/L versus DOY ####
jpeg(file='figs/ZL_DOY.jpeg',width=16,height=10,res=350, units = 'cm')
DOY_ZL <- ggplot(df, aes(x=DOY,y=ZL)) + 
  geom_point(alpha=0.1) + 
  geom_smooth(color ='orange') 
# Axes - labels
DOY_ZL <- DOY_ZL + coord_cartesian(ylim=c(-5,5))
DOY_ZL <- DOY_ZL + ylab(expression("z/L"~"[dimensionless]"))
DOY_ZL <- DOY_ZL + xlab(expression("DOY"))

# Theme
DOY_ZL <-  DOY_ZL + theme_bw()
DOY_ZL
dev.off()
rm(DOY_ZL)
####
#### Plot: U versus DOY ####
jpeg(file='figs/U_DOY.jpeg',width=16,height=10,res=350, units = 'cm')
DOY_U <- ggplot(df, aes(x=DOY,y=WS)) + 
  geom_point(alpha=0.1) + 
  geom_smooth(color ='orange') 
# Axes - labels
DOY_U <- DOY_U + coord_cartesian(ylim=c(0,4))
DOY_U <- DOY_U + ylab(expression("U"~"[m"~s^{-1}*"]"))
DOY_U <- DOY_U + xlab(expression("DOY"))

# Theme
DOY_U <-  DOY_U + theme_bw()
DOY_U
dev.off()
rm(DOY_U)
####

#### Plot: CD versus DOY ####
jpeg(file='figs/CD_DOY.jpeg',width=16,height=10,res=350, units = 'cm')
DOY_CD <- ggplot(df, aes(x=DOY,y=log10(CD))) + 
  geom_point(alpha=0.1) + 
  geom_smooth(color ='orange') 
# Axes - labels
DOY_CD <- DOY_CD + coord_cartesian()
DOY_CD <- DOY_CD + ylab(expression(log[10]*"("*C[D]*")"~"[dimensionless]"))
DOY_CD <- DOY_CD + xlab(expression("DOY"))

# Theme
DOY_CD <-  DOY_CD + theme_bw()
DOY_CD
dev.off()
rm(DOY_CD)
####


ggplot(df_daily, aes(x=DOY,y=FCO2)) +
  geom_point(alpha=0.2, color = '#E69F00') +
  geom_smooth(method="loess",color = '#E69F00', level = 0.95, alpha = 0.1) +
  #geom_point(alpha=0.2, color = '#56B4E9', data = df_2017) +
  #geom_smooth(method="loess",color = '#56B4E9', level = 0.95, data = df_2017, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'blue', data = df_2018) +
  #geom_smooth(method="loess",color = 'blue', level = 0.95, data = df_2018, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'green', data = df_2019) +
  #geom_smooth(method="loess",color = 'green', level = 0.95, data = df_2019, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'lightgreen', data = df_2020) +
  #geom_smooth(method="loess",color = 'lightgreen', level = 0.95, data = df_2020, alpha = 0.1) +
  coord_cartesian(ylim=c(-0.1,0.1)) +
  theme_bw()

ggplot(df_daily, aes(x=DOY,y=EMA)) +
  geom_point(alpha=0.2, color = 'blue') +
  geom_smooth(method="loess",color = 'blue', level = 0.95, alpha = 0.1) +
  #geom_point(alpha=0.2, color = '#56B4E9', data = df_2017) +
  #geom_smooth(method="loess",color = '#56B4E9', level = 0.95, data = df_2017, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'blue', data = df_2018) +
  #geom_smooth(method="loess",color = 'blue', level = 0.95, data = df_2018, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'green', data = df_2019) +
  #geom_smooth(method="loess",color = 'green', level = 0.95, data = df_2019, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'lightgreen', data = df_2020) +
  #geom_smooth(method="loess",color = 'lightgreen', level = 0.95, data = df_2020, alpha = 0.1) +
  #coord_cartesian(ylim=c(-0.25,0.25)) +
  theme_bw()

ggplot(df_daily, aes(x=DOY,y=WS)) +
  geom_point(alpha=0.2, color = 'green') +
  geom_smooth(method="loess",color = 'green', level = 0.95, alpha = 0.1) +
  #geom_point(alpha=0.2, color = '#56B4E9', data = df_2017) +
  #geom_smooth(method="loess",color = '#56B4E9', level = 0.95, data = df_2017, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'blue', data = df_2018) +
  #geom_smooth(method="loess",color = 'blue', level = 0.95, data = df_2018, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'green', data = df_2019) +
  #geom_smooth(method="loess",color = 'green', level = 0.95, data = df_2019, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'lightgreen', data = df_2020) +
  #geom_smooth(method="loess",color = 'lightgreen', level = 0.95, data = df_2020, alpha = 0.1) +
  #coord_cartesian(ylim=c(-0.25,0.25)) +
  theme_bw()

ggplot(df_daily, aes(x=DOY,y=ZL)) +
  geom_point(alpha=0.2, color = 'red') +
  geom_smooth(method="loess",color = 'red', level = 0.95, alpha = 0.1) +
  #geom_point(alpha=0.2, color = '#56B4E9', data = df_2017) +
  #geom_smooth(method="loess",color = '#56B4E9', level = 0.95, data = df_2017, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'blue', data = df_2018) +
  #geom_smooth(method="loess",color = 'blue', level = 0.95, data = df_2018, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'green', data = df_2019) +
  #geom_smooth(method="loess",color = 'green', level = 0.95, data = df_2019, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'lightgreen', data = df_2020) +
  #geom_smooth(method="loess",color = 'lightgreen', level = 0.95, data = df_2020, alpha = 0.1) +
  #coord_cartesian(ylim=c(-0.25,0.25)) +
  theme_bw()

ggplot(df_daily, aes(x=DOY,y=air_pressure)) +
  geom_point(alpha=0.2, color = 'red') +
  geom_smooth(method="loess",color = 'red', level = 0.95, alpha = 0.1) +
  #geom_point(alpha=0.2, color = '#56B4E9', data = df_2017) +
  #geom_smooth(method="loess",color = '#56B4E9', level = 0.95, data = df_2017, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'blue', data = df_2018) +
  #geom_smooth(method="loess",color = 'blue', level = 0.95, data = df_2018, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'green', data = df_2019) +
  #geom_smooth(method="loess",color = 'green', level = 0.95, data = df_2019, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'lightgreen', data = df_2020) +
  #geom_smooth(method="loess",color = 'lightgreen', level = 0.95, data = df_2020, alpha = 0.1) +
  #coord_cartesian(ylim=c(-0.25,0.25)) +
  theme_bw()

ggplot(df_sat, aes(x=DOY,y=air_pressure)) +
  geom_point(alpha=0.2, color = 'red') +
  geom_smooth(method="loess",color = 'red', level = 0.95, alpha = 0.1) +
  #geom_point(alpha=0.2, color = '#56B4E9', data = df_2017) +
  #geom_smooth(method="loess",color = '#56B4E9', level = 0.95, data = df_2017, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'blue', data = df_2018) +
  #geom_smooth(method="loess",color = 'blue', level = 0.95, data = df_2018, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'green', data = df_2019) +
  #geom_smooth(method="loess",color = 'green', level = 0.95, data = df_2019, alpha = 0.1) +
  #geom_point(alpha=0.2, color = 'lightgreen', data = df_2020) +
  #geom_smooth(method="loess",color = 'lightgreen', level = 0.95, data = df_2020, alpha = 0.1) +
  #coord_cartesian(ylim=c(-0.25,0.25)) +
  theme_bw()

ggplot(df, aes(WS, FCO2, color = EMA)) + 
  geom_point() +
  coord_cartesian(ylim = c(-0.5,0.5),xlim=c(0,3))

#### Plot: TS versus DOY ####
jpeg(file='figs/TS_DOY.jpeg',width=16,height=10,res=350, units = 'cm')
DOY_TS <- ggplot(df, aes(x=DOY,y=EMA)) + 
  geom_point(alpha=0.1) + 
  geom_smooth(color ='orange') 
# Axes - labels
DOY_TS <- DOY_TS + coord_cartesian()
DOY_TS <- DOY_TS + ylab(expression(paste(T[S]," ","[","\u{00B0}","C]")))
                        
DOY_TS <- DOY_TS + xlab(expression("DOY"))

# Theme
DOY_TS <-  DOY_TS + theme_bw()
DOY_TS
dev.off()
rm(DOY_TS)
####



#### Plot: FCO2 violin plot ####
# Create a df with the monsoon column 
Monsoon <- rep("NEM", nrow(NEM_30))
NEM_temp <- cbind(NEM_30,Monsoon)
Monsoon <- rep('STM', nrow(STM_30))
STM_temp <- cbind(STM_30,Monsoon)
Monsoon <- rep('SWM', nrow(SWM_30))
SWM_temp <- cbind(SWM_30,Monsoon)
Monsoon <- rep('FTM', nrow(FTM_30))
FTM_temp <- cbind(FTM_30,Monsoon)
df_season <- rbind(NEM_temp,SWM_temp, STM_temp, FTM_temp)
# Housekeeping
rm(NEM_temp,SWM_temp,FTM_temp,STM_temp,Monsoon)

# Violin plot for FCO2 
jpeg(file='figs/FCO2_violin.jpeg',width=8,height=8,res=350, units = 'cm')
FCO2_violin <- ggplot(df_season, aes(x = as.factor(Monsoon),
                                     y = FCO2, color = as.factor(Monsoon))) + 
  geom_violin() + 
  geom_boxplot(width=0.1, outlier.shape = NA) +
  scale_color_brewer(palette="Dark2") +
  ylab(expression("F"~CO[2]~"["*mu*mol~m^{-2}~s^{-1}*"]")) +
  xlab("") +
  coord_cartesian(ylim = c(-0.5,0.5)) + scale_x_discrete(limits=c('NEM','SWM')) +
  theme_bw() +
  theme(legend.position = "none")
FCO2_violin
dev.off()
rm(FCO2_violin)

####


#### Plot: z/L violin plot ####
# Violin plot for z/L 
jpeg(file='figs/ZL_violin.jpeg',width=8,height=8,res=350, units = 'cm')
ZL_violin <- ggplot(df_season, aes(x = as.factor(Monsoon),
                                     y = ZL, color = as.factor(Monsoon))) + 
  geom_violin() + 
  geom_boxplot(width=0.1, outlier.shape = NA) +
  scale_color_brewer(palette="Dark2") +
  ylab(expression("z/L [dimensionless]")) +
  xlab("") +
  coord_cartesian(ylim = c(-10,10)) + scale_x_discrete(limits=c('NEM','SWM')) +
  theme_bw() +
  theme(legend.position = "none")
ZL_violin
dev.off()
rm(ZL_violin)

####

#### Plot: u* violin plot ####
# Violin plot for u*
jpeg(file='figs/ustar_violin.jpeg',width=8,height=8,res=350, units = 'cm')
ustar_violin <- ggplot(df_season, aes(x = as.factor(Monsoon),
                                   y = USTAR, color = as.factor(Monsoon))) + 
  geom_violin() + 
  geom_boxplot(width=0.1, outlier.shape = NA) +
  scale_color_brewer(palette="Dark2") +
  ylab(expression(u["*"]~"["*m~s^{-1}*"]")) +
  xlab("") +
  coord_cartesian(ylim = c(0,0.4)) + scale_x_discrete(limits=c('NEM','SWM')) +
  theme_bw() +
  theme(legend.position = "none")
ustar_violin
dev.off()
rm(ustar_violin)

####

ggplot(df_hour,aes(as.integer(hour),delT)) + geom_point() + geom_smooth() +
  coord_cartesian(ylim=c(-3,3))

ggplot(df_hour_pos,aes(as.integer(hour),delT)) + geom_point() + geom_smooth() +
  coord_cartesian(ylim=c(-3,3))

ggplot(df_hour_neg,aes(as.integer(hour),delT)) + geom_point() + geom_smooth() +
  coord_cartesian(ylim=c(-3,3))

ggplot(df_hour,aes(as.integer(hour),WS)) + geom_point() + geom_smooth() + 
  coord_cartesian(ylim=c(0,3))

ggplot(df_hour_pos,aes(as.integer(hour),WS)) + geom_point() + geom_smooth() + 
  coord_cartesian(ylim=c(0,3)) 

ggplot(df_hour_neg,aes(as.integer(hour),WS)) + geom_point() + geom_smooth() + 
  coord_cartesian(ylim=c(0,3))

#### High and low CD ####
df_lowCD <- filter(df, CD <=0.005)
df_highCD <- filter(df, CD > 0.005)


ggplot(df_lowCD, aes(WS,abs(FCO2))) + geom_point(color='#F8766D') +
  geom_point(data=df_highCD,aes(WS,abs(FCO2)),color='#00BA38') + theme_bw()

# Low CD = red
# High CD = green
#### FCO2 against H ####


ggplot(df_lowCD, aes(H,abs(FCO2))) + geom_point(color='#F8766D') +
  geom_point(data=df_highCD,aes(H,abs(FCO2)),color='#00BA38',alpha=0.5) + theme_bw() + coord_cartesian(xlim=c(0,20))






df <- df[,-35]
cat_FCO2 <- cut(abs(df$FCO2),
               breaks=seq(0,4,by=0.1))

df <- cbind(df,cat_FCO2)
rm(cat_FCO2)


ggplot(filter(df,is.na(cat_FCO2) | cat_FCO2 != "(0,0.1]"), aes(CD, H, color=cat_FCO2)) + 
  geom_point(alpha=0.2) +
  coord_cartesian(xlim=c(0,0.025),ylim=c(0,5))





ggplot(df, aes(CD, H, abs(FCO2))) + geom_point() + geom_hex() +
  coord_cartesian(xlim=c(0,0.1),ylim=c(0,10)) +
  stat_summary_hex(fun = function(x) mean(x), bins = 20) +
  scale_fill_viridis_c(option = "magma") 
  
####
#### Plot: Daily FCO2 versus DOY ####
jpeg(file='figs/daily_fco2_DOY.jpeg',width=10,height=8,res=350, units = 'cm')
DOY_FCO2 <- ggplot(df_daily, aes(x=DOY,y=FCO2)) + 
  geom_line(alpha=0.2) + 
  geom_smooth(method="loess",level=0.95, data=df, color ='#F8766D', alpha=0.8) 
# Axes - labels
DOY_FCO2 <- DOY_FCO2 + coord_cartesian(ylim=c(-0.5,0.5))
DOY_FCO2 <- DOY_FCO2 + ylab(expression("F"~CO[2]~"["*mu*mol~m^{-2}~s^{-1}*"]"))
DOY_FCO2 <- DOY_FCO2 + xlab(expression("DOY"))

# Shaded area for SWM
DOY_FCO2 <- DOY_FCO2 + annotate("rect", xmin = 152, xmax = 273, ymin = -1, ymax = 1,
                                alpha = .1,fill = "#F8766D")

# Shaded area for NEM 1
DOY_FCO2 <- DOY_FCO2 + annotate("rect", xmin = 1, xmax = 90, ymin = -1, ymax = 1,
                                alpha = .1,fill = "#00BA38")
# Shaded area for NEM 2
DOY_FCO2 <- DOY_FCO2 + annotate("rect", xmin = 335, xmax = 365, ymin = -1, ymax = 1,
                                alpha = .1,fill = "#00BA38")


# Theme
DOY_FCO2 <-  DOY_FCO2 + theme_bw()
DOY_FCO2
dev.off()
rm(DOY_FCO2)
####

#### Plot: Daily z/L versus DOY ####
jpeg(file='figs/daily_ZL_DOY.jpeg',width=10,height=8,res=350, units = 'cm')
DOY_ZL <- ggplot(df_daily, aes(x=DOY,y=ZL)) + 
  geom_line(alpha=0.2) + 
  geom_smooth(color ='#F8766D') 
# Axes - labels
DOY_ZL <- DOY_ZL + coord_cartesian(ylim=c(-10,10))
DOY_ZL <- DOY_ZL + ylab(expression("z/L"~"[dimensionless]"))
DOY_ZL <- DOY_ZL + xlab(expression("DOY"))

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
####
#### Plot: Daily U versus DOY ####
jpeg(file='figs/daily_U_DOY.jpeg',width=10,height=8,res=350, units = 'cm')
DOY_U <- ggplot(df_daily, aes(x=DOY,y=WS)) + 
  geom_line(alpha=0.2) + 
  geom_smooth(color ='#F8766D') 
# Axes - labels
DOY_U <- DOY_U + coord_cartesian(ylim=c(0,4))
DOY_U <- DOY_U + ylab(expression("U"~"[m"~s^{-1}*"]"))
DOY_U <- DOY_U + xlab(expression("DOY"))

# Shaded area for SWM
DOY_U <- DOY_U + annotate("rect", xmin = 152, xmax = 273, ymin = -11, ymax = 11,
                            alpha = .1,fill = "#F8766D")

# Shaded area for NEM 1
DOY_U <- DOY_U + annotate("rect", xmin = 1, xmax = 90, ymin = -11, ymax = 11,
                            alpha = .1,fill = "#00BA38")
# Shaded area for NEM 2
DOY_U <- DOY_U + annotate("rect", xmin = 335, xmax = 365, ymin = -11, ymax = 11,
                            alpha = .1,fill = "#00BA38")

# Theme
DOY_U <-  DOY_U + theme_bw()
DOY_U
dev.off()
rm(DOY_U)
####

#### Plot: Daily CD versus DOY ####
jpeg(file='figs/Daily_CD_DOY.jpeg',width=10,height=8,res=350, units = 'cm')
DOY_CD <- ggplot(df_daily, aes(x=DOY,y=log10(CD))) + 
  geom_line(alpha=0.2) + 
  geom_smooth(color ='#F8766D') 
# Axes - labels
DOY_CD <- DOY_CD + coord_cartesian(ylim=c(-4,3))
DOY_CD <- DOY_CD + ylab(expression(log[10]*"("*C[D]*")"~"[dimensionless]"))
DOY_CD <- DOY_CD + xlab(expression("DOY"))

# Shaded area for SWM
DOY_CD <- DOY_CD + annotate("rect", xmin = 152, xmax = 273, ymin = -11, ymax = 11,
                          alpha = .1,fill = "#F8766D")

# Shaded area for NEM 1
DOY_CD <- DOY_CD + annotate("rect", xmin = 1, xmax = 90, ymin = -11, ymax = 11,
                          alpha = .1,fill = "#00BA38")
# Shaded area for NEM 2
DOY_CD <- DOY_CD + annotate("rect", xmin = 335, xmax = 365, ymin = -11, ymax = 11,
                          alpha = .1,fill = "#00BA38")

# Theme
DOY_CD <-  DOY_CD + theme_bw()
DOY_CD
dev.off()
rm(DOY_CD)
####




#### Plot: Daily TS versus DOY ####
jpeg(file='figs/Daily_TS_DOY.jpeg',width=10,height=8,res=350, units = 'cm')
DOY_TS <- ggplot(df_daily, aes(x=DOY,y=EMA)) + 
  geom_line(alpha=0.2) + 
  geom_smooth(color ='#F8766D') 
# Axes - labels
DOY_TS <- DOY_TS + coord_cartesian(ylim=c(26,34))
DOY_TS <- DOY_TS + ylab(expression(paste(T[S]," ","[","\u{00B0}","C]")))

DOY_TS <- DOY_TS + xlab(expression("DOY"))

# Shaded area for SWM
DOY_TS <- DOY_TS + annotate("rect", xmin = 152, xmax = 273, ymin = 20, ymax = 40,
                            alpha = .1,fill = "#F8766D")

# Shaded area for NEM 1
DOY_TS <- DOY_TS + annotate("rect", xmin = 1, xmax = 90, ymin = 20, ymax = 40,
                            alpha = .1,fill = "#00BA38")
# Shaded area for NEM 2
DOY_TS <- DOY_TS + annotate("rect", xmin = 335, xmax = 365, ymin = 20, ymax = 40,
                            alpha = .1,fill = "#00BA38")

# Theme
DOY_TS <-  DOY_TS + theme_bw()
DOY_TS
dev.off()
rm(DOY_TS)
####

#### Plot: Daily TA versus DOY ####
jpeg(file='figs/Daily_TA_DOY.jpeg',width=10,height=8,res=350, units = 'cm')
DOY_TA <- ggplot(df_daily, aes(x=DOY,y=TA_ema)) + 
  geom_line(alpha=0.2) + 
  geom_smooth(color ='#F8766D') 
# Axes - labels
DOY_TA <- DOY_TA + coord_cartesian(ylim=c(24,33))
DOY_TA <- DOY_TA + ylab(expression(paste(T[A]," ","[","\u{00B0}","C]")))

DOY_TA <- DOY_TA + xlab(expression("DOY"))

# Shaded area for SWM
DOY_TA <- DOY_TA + annotate("rect", xmin = 152, xmax = 273, ymin = 20, ymax = 40,
                            alpha = .1,fill = "#F8766D")

# Shaded area for NEM 1
DOY_TA <- DOY_TA + annotate("rect", xmin = 1, xmax = 90, ymin = 20, ymax = 40,
                            alpha = .1,fill = "#00BA38")
# Shaded area for NEM 2
DOY_TA <- DOY_TA + annotate("rect", xmin = 335, xmax = 365, ymin = 20, ymax = 40,
                            alpha = .1,fill = "#00BA38")

# Theme
DOY_TA <-  DOY_TA + theme_bw()
DOY_TA
dev.off()
rm(DOY_TA)
####



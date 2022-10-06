
library(dplyr)

#### Group the data according to hour of day ####

##### Overall ####
df_hour <- df %>% 
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WD = mean(WD,na.rm=TRUE),
            RN = mean(RN,na.rm=TRUE), RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
            EMA = mean(EMA,na.rm=TRUE), PCO2_sw = mean(PCO2_sw,na.rm=TRUE),
            residCO2 = mean(residCO2,na.rm = T))

##### NEM ####
NEM_hour <- NEM_30 %>% 
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WD = mean(WD,na.rm=TRUE),
            RN = mean(RN,na.rm=TRUE), RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
            EMA = mean(EMA,na.rm=TRUE), PCO2_sw = mean(PCO2_sw,na.rm=TRUE))
##### SWM ###
SWM_hour <- SWM_30 %>% 
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WD = mean(WD,na.rm=TRUE),
            RN = mean(RN,na.rm=TRUE), RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
            EMA = mean(EMA,na.rm=TRUE), PCO2_sw = mean(PCO2_sw,na.rm=TRUE))
##### STM ####
STM_hour <- STM_30 %>% 
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WD = mean(WD,na.rm=TRUE),
            RN = mean(RN,na.rm=TRUE), RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
            EMA = mean(EMA,na.rm=TRUE), PCO2_sw = mean(PCO2_sw,na.rm=TRUE))
##### FTM ####
FTM_hour <- FTM_30 %>% 
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WD = mean(WD,na.rm=TRUE),
            RN = mean(RN,na.rm=TRUE), RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
            EMA = mean(EMA,na.rm=TRUE), PCO2_sw = mean(PCO2_sw,na.rm=TRUE))


#### CO2 flux ####
##### NEM and FTM ####
jpeg('figs/asiaflux/FCO2_NEM_FTM.jpg',unit='cm',width=16,height=16,res=300)
plot(df_hour$hour, df_hour$FCO2, type = 'l', ylim=c(-0.10,0.06), lty=3, lwd = 2,
     xlab = 'Hour', ylab = 'FCO2')
lines(NEM_hour$hour, NEM_hour$FCO2, type = 'l', col = 'blue', lwd = 2)
lines(FTM_hour$hour, FTM_hour$FCO2, type = 'l', col='orange', lwd = 2)
abline(h = 0,lty=1, lwd=1.5)
abline(v=7,lty=2,lwd=2)
abline(v=19,lty=2,lwd=2)
abline(v = 12, lty=2,lwd=2)
dev.off()

##### SWM and STM ####
jpeg('figs/asiaflux/FCO2_SWM_STM.jpg',unit='cm',width=16,height=16,res=300)
plot(df_hour$hour, df_hour$FCO2, type = 'l', ylim=c(-0.10,0.06), lty=3, lwd = 2,
     xlab = 'Hour', ylab = 'FCO2')
lines(STM_hour$hour, STM_hour$FCO2, type = 'l', col='green', lwd = 2)
lines(SWM_hour$hour, SWM_hour$FCO2, type = 'l', col='red', lwd = 2)
abline(h = 0,lty=1, lwd=1.5)
abline(v=7,lty=2,lwd=2)
abline(v=19,lty=2,lwd=2)
abline(v = 12, lty=2,lwd=2)
dev.off()

#### PPFD ####
##### NEM and FTM ####
jpeg('figs/asiaflux/PPFD_NEM_FTM.jpg',unit='cm',width=16,height=16,res=300)
plot(df_hour$hour, df_hour$PPFD, type = 'l', ylim=c(0,1400), lty=3, lwd = 2,
     xlab = 'Hour', ylab = 'PPFD')
lines(NEM_hour$hour, NEM_hour$PPFD, type = 'l', col = 'blue', lwd = 2)
lines(FTM_hour$hour, FTM_hour$PPFD, type = 'l', col='orange', lwd = 2)
abline(v=7,lty=2,lwd=2)
abline(v=19,lty=2,lwd=2)
abline(v = 12, lty=2,lwd=2)
dev.off()

##### SWM and STM ####
jpeg('figs/asiaflux/PPFD_SWM_STM.jpg',unit='cm',width=16,height=16,res=300)
plot(df_hour$hour, df_hour$PPFD, type = 'l', ylim=c(0,1400), lty=3, lwd = 2,
     xlab = 'Hour', ylab = 'PPFD')
lines(STM_hour$hour, STM_hour$PPFD, type = 'l', col='green', lwd = 2)
lines(SWM_hour$hour, SWM_hour$PPFD, type = 'l', col='red', lwd = 2)
abline(v=7,lty=2,lwd=2)
abline(v=19,lty=2,lwd=2)
abline(v = 12, lty=2,lwd=2)
dev.off()


##### TS ####
jpeg('figs/asiaflux/TS_NEM_STM.jpg',unit='cm',width=16,height=16,res=300)
plot(df_hour$hour, df_hour$TS, type = 'l', ylim=c(28,32), lty=3, lwd = 2,
     xlab = 'Hour', ylab = 'TS')
lines(NEM_hour$hour, NEM_hour$TS, type = 'l', col = 'blue')
lines(FTM_hour$hour, FTM_hour$TS, type = 'l', col='orange')
abline(v=7,lty=2,lwd=2)
abline(v=19,lty=2,lwd=2)
abline(v = 12, lty=2,lwd=2)
dev.off()

jpeg('figs/asiaflux/TS_SWM_STM.jpg',unit='cm',width=16,height=16,res=300)
plot(df_hour$hour, df_hour$TS, type = 'l', ylim=c(28,32), lty=3, lwd = 2,
     xlab = 'Hour', ylab = 'TS')
lines(SWM_hour$hour, SWM_hour$TS, type = 'l', col='red')
lines(STM_hour$hour, STM_hour$TS, type = 'l', col='green')
abline(v=7,lty=2,lwd=2)
abline(v=19,lty=2,lwd=2)
abline(v = 12, lty=2,lwd=2)
dev.off()

##### CO2 mole fraction ####
jpeg('figs/asiaflux/co2_mole_NEM_STM.jpg',unit='cm',width=16,height=16,res=300)
plot(df_hour$hour, df_hour$co2_mole_fraction, type = 'l', ylim=c(380,420), lty=3, lwd = 2,
     xlab = 'Hour', ylab = 'CO2')
lines(NEM_hour$hour, NEM_hour$co2_mole_fraction, type = 'l', col = 'blue')
lines(FTM_hour$hour, FTM_hour$co2_mole_fraction, type = 'l', col='orange')
abline(v=7,lty=2,lwd=2)
abline(v=19,lty=2,lwd=2)
abline(v = 12, lty=2,lwd=2)
dev.off()

jpeg('figs/asiaflux/co2_mole_SWM_STM.jpg',unit='cm',width=16,height=16,res=300)
plot(df_hour$hour, df_hour$co2_mole_fraction, type = 'l', ylim=c(380,420), lty=3, lwd = 2,
     xlab = 'Hour', ylab = 'CO2')
lines(SWM_hour$hour, SWM_hour$co2_mole_fraction, type = 'l', col='red')
lines(STM_hour$hour, STM_hour$co2_mole_fraction, type = 'l', col='green')
abline(v=7,lty=2,lwd=2)
abline(v=19,lty=2,lwd=2)
abline(v = 12, lty=2,lwd=2)
dev.off()

##### Wind speed ####
jpeg('figs/asiaflux/WS_NEM_STM.jpg',unit='cm',width=16,height=16,res=300)
plot(df_hour$hour, df_hour$WS, type = 'l', ylim=c(0,1.5), lty=3, lwd = 2,
     xlab = 'Hour', ylab = 'Wind Speed')
lines(NEM_hour$hour, NEM_hour$WS, type = 'l', col = 'blue')
lines(FTM_hour$hour, FTM_hour$WS, type = 'l', col='orange')
abline(v=7,lty=2,lwd=2)
abline(v=19,lty=2,lwd=2)
abline(v = 12, lty=2,lwd=2)
dev.off()

jpeg('figs/asiaflux/WS_SWM_STM.jpg',unit='cm',width=16,height=16,res=300)
plot(df_hour$hour, df_hour$WS, type = 'l', ylim=c(0,1.5), lty=3, lwd = 2,
     xlab = 'Hour', ylab = 'Wind Speed')
lines(SWM_hour$hour, SWM_hour$WS, type = 'l', col='red')
lines(STM_hour$hour, STM_hour$WS, type = 'l', col='green')
abline(v=7,lty=2,lwd=2)
abline(v=19,lty=2,lwd=2)
abline(v = 12, lty=2,lwd=2)
dev.off()


##### RN ####
jpeg('figs/asiaflux/RN_NEM_STM.jpg',unit='cm',width=16,height=16,res=300)
plot(df_hour$hour, df_hour$RN, type = 'l', ylim=c(-50,400), lty=3, lwd = 2,
     xlab = 'Hour', ylab = 'RN')
lines(NEM_hour$hour, NEM_hour$RN, type = 'l', col = 'blue')
lines(FTM_hour$hour, FTM_hour$RN, type = 'l', col='orange')
abline(v=7,lty=2,lwd=2)
abline(v=19,lty=2,lwd=2)
abline(v = 12, lty=2,lwd=2)
dev.off()

jpeg('figs/asiaflux/RN_SWM_STM.jpg',unit='cm',width=16,height=16,res=300)
plot(df_hour$hour, df_hour$RN, type = 'l', ylim=c(-50,400), lty=3, lwd = 2,
     xlab = 'Hour', ylab = 'RN')
lines(SWM_hour$hour, SWM_hour$RN, type = 'l', col='red')
lines(STM_hour$hour, STM_hour$RN, type = 'l', col='green')
abline(v=7,lty=2,lwd=2)
abline(v=19,lty=2,lwd=2)
abline(v = 12, lty=2,lwd=2)
dev.off()

#### Correlation Plot ####
##### WS ####
jpeg('figs/asiaflux/CO2_WS.jpg',unit='cm',width=16,height=16,res=300)
plot(NEM_30$WS, NEM_30$FCO2, pch=19, xlim = c(0,5), cex = 0.5, col = 'blue',
     xlab = 'U', ylab = 'FCO2')
abline(lm(FCO2 ~ WS, data = NEM_30), col = 'blue', lwd = 2, lty = 2)
points(SWM_30$WS, SWM_30$FCO2, pch=19, xlim = c(0,5), cex = 0.5, col = 'red')
abline(lm(FCO2 ~ WS, data = SWM_30), col = 'red', lwd = 2, lty = 2)
points(FTM_30$WS, FTM_30$FCO2, pch=19, xlim = c(0,5),cex = 0.5, col = 'orange')
abline(lm(FCO2 ~ WS, data = FTM_30), col = 'orange', lwd = 2, lty = 2)
points(STM_30$WS, STM_30$FCO2, pch=19, xlim = c(0,5),cex = 0.5, col = 'green')
abline(lm(FCO2 ~ WS, data = STM_30), col = 'green', lwd = 2, lty = 2)
dev.off()
##### TS ####
jpeg('figs/asiaflux/CO2_TS.jpg',unit='cm',width=16,height=16,res=300)
plot(NEM_30$EMA, NEM_30$FCO2, pch=19, cex = 0.5, col = 'blue', xlim = c(25,35),
     xlab = 'T', ylab = 'FCO2')
abline(lm(FCO2 ~ EMA, data = NEM_30), col = 'blue', lwd = 2, lty = 2)
points(SWM_30$EMA, SWM_30$FCO2, pch=19, cex = 0.5, col = 'red')
abline(lm(FCO2 ~ EMA, data = SWM_30), col = 'red', lwd = 2, lty = 2)
points(FTM_30$EMA, FTM_30$FCO2, pch=19,cex = 0.5, col = 'orange')
abline(lm(FCO2 ~ EMA, data = FTM_30), col = 'orange', lwd = 2, lty = 2)
points(STM_30$EMA, STM_30$FCO2, pch=19, xlim = c(0,5),cex = 0.5, col = 'green')
abline(lm(FCO2 ~ EMA, data = STM_30), col = 'green', lwd = 2, lty = 2)
dev.off()

library(car) # load the package
NEM_30_1 <- na.omit(NEM_30)


NEM.pca <- prcomp(NEM_30_1[,-1], # Remove the first column
                  center = TRUE, # Center the data, similar to scaling
                  scale. = TRUE) # Need to be scaled because of magnitude diff.

SWM.pca <- prcomp(SWM_hour[,-1], # Remove the first column
                  center = TRUE, # Center the data, similar to scaling
                  scale. = TRUE) # Need to be scaled because of magnitude diff.

STM.pca <- prcomp(STM_hour[,-1], # Remove the first column
                  center = TRUE, # Center the data, similar to scaling
                  scale. = TRUE) # Need to be scaled because of magnitude diff.

FTM.pca <- prcomp(FTM_hour[,-1], # Remove the first column
                  center = TRUE, # Center the data, similar to scaling
                  scale. = TRUE) # Need to be scaled because of magnitude diff.

plot(NEM.pca$x[,1:2], # Plot only the first and second PCs
     pch = 20) 

plot(SWM.pca$x[,1:2], # Plot only the first and second PCs
     pch = 20) 

plot(STM.pca$x[,1:2], # Plot only the first and second PCs
     pch = 20) 

plot(FTM.pca$x[,1:2], # Plot only the first and second PCs
     pch = 20) 


jpeg('figs/biplot_csi.jpeg', res = 300, units = 'cm', height = 16, width = 16)
# The margins of the plot: bottom, left, top, right
par(mar = c(4,4,1,1))
plot(NEM.pca$x[,1:2], # Plot only the first and second PCs
     pch = 20)        # The marker 
     #col = df$Sample, # The color
     #xlim = c(-5,10), ylim = c(-2.5,6)) # The limits of the axes
dataEllipse(csi.pca$x[,1],      # PC1
            csi.pca$x[,2],      # PC2
            groups = df$Sample, # Groups
            lwd = 1,            # line width
            group.labels = c('car','plant','scene'), 
            plot.points = FALSE, # Do not want to redraw points on top of old points
            levels = 0.5,      # 50% confidence level, similar to what is set in ggbiplot
            add = TRUE,
            fill=TRUE, 
            fill.alpha = 0.02,
            col = c('black','red','green'))

legend('bottomright', c('Car','Plant','Scene'), pch = c(20,20,20),
       col = c('black', 'red', 'green'))
dev.off()



#### LE ####
plot(df_hour$hour, df_hour$LE, type = 'l', lty=2, ylim=c(0,20))

lines(NEM_hour$hour, NEM_hour$LE, type = 'l', col = 'blue')

lines(STM_hour$hour, STM_hour$LE, type = 'l', col='green')

lines(SWM_hour$hour, SWM_hour$LE, type = 'l', col='red')

lines(FTM_hour$hour, FTM_hour$LE, type = 'l', col='orange')


#### H ####
plot(df_hour$hour, df_hour$H, type = 'l', lty=2, ylim=c(0,4))

lines(NEM_hour$hour, NEM_hour$H, type = 'l', col = 'blue')

lines(STM_hour$hour, STM_hour$H, type = 'l', col='green')

lines(SWM_hour$hour, SWM_hour$H, type = 'l', col='red')

lines(FTM_hour$hour, FTM_hour$H, type = 'l', col='orange')



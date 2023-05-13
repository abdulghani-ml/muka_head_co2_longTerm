######packages ####
#install.packages('sjPlot')
library(openair)
library(ggplot2)
library(dplyr)
library(sjPlot)
library(stats)

##### partition of date #####


marchf <- selectByDate(df_merge_day,month = 3)[c(2,3,4,5,15,17,18,20,27,28,32,33,36)]
Febf <- selectByDate(df_merge_day,month = 2)[,c(2,3,4,5,15,17,18,20,27,28,32,33,36)]
Janf <- selectByDate(df_merge_day,month = 1)[,c(2,3,4,5,15,17,18,20,27,28,32,33,36)]
Novf <- selectByDate(df_merge_day,month = 11)[,c(2,3,4,5,15,17,18,20,27,28,32,33,36)]
Decf <- selectByDate(df_merge_day,month = 12)[,c(2,3,4,5,15,17,18,20,27,28,32,33,36)]
Julyf <- selectByDate(df_merge_day,month = 7)[,c(2,3,4,5,15,17,18,20,27,28,32,33,36)]
Aprf <- selectByDate(df_merge_day,month = 4)[,c(2,3,4,5,15,17,18,20,27,28,32,33,36)]
Mayf <- selectByDate(df_merge_day,month = 5)[,c(2,3,4,5,15,17,18,20,27,28,32,33,36)]
Junf <- selectByDate(df_merge_day,month = 6)[,c(2,3,4,5,15,17,18,20,27,28,32,33,36)]
Augf <- selectByDate(df_merge_day,month = 8)[,c(2,3,4,5,15,17,18,20,27,28,32,33,36)]
Sepf <- selectByDate(df_merge_day,month = 9)[,c(2,3,4,5,15,17,18,20,27,28,32,33,36)]
Octf <- selectByDate(df_merge_day,month = 10)[,c(2,3,4,5,15,17,18,20,27,28,32,33,36)]


df_NEM_day <- rbind(Decf,Janf, Febf,marchf)
df_SWM_day <- rbind(Junf,Julyf,Augf,Sepf)
df_FTM_day <- rbind(Octf,Novf)
df_STM_day <- rbind(Aprf,Mayf)

#### WD filter ####
#df$WD[df$WD > 45 & df$WD < 315] <- NA

#WDsin <- sin(df$WD * pi/180)

#df <- cbind(df,WDsin)

#df$WS[which(df$WS < 0 | df$WS > 5 )] <- NA


##### plot #####
library(ggplot2)

####not good ####


jpeg(filename='figs/Feb_marchCHL&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_feb_mar, aes(DOY,CHL)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim=c(40,80),ylim = c(0,5)) +
  theme_bw()
dev.off()

jpeg(filename='figs/Feb_marchFCO2mmol&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_feb_mar, aes(DOY,FCO2_mmol)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim=c(40,80),ylim = c(-15,15))+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_bw()
dev.off()


jpeg(filename='figs/Feb_marchWS&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_feb_mar, aes(DOY,WS)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(40,80),ylim = c(0,2.5))+
  theme_bw()
dev.off()

jpeg(filename='figs/Feb_marchTS&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_feb_mar, aes(DOY,TS)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(40,80),ylim = c(27.5,32.5))+
  theme_bw()
dev.off()

jpeg(filename='figs/F_m_H&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_feb_mar, aes(DOY,H)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(40,80),ylim = c(-0.5,3))+
  theme_bw()
dev.off()





jpeg(filename='figs/JUL_AUGUST_FCO2mmol&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_JUL_AUG, aes(DOY,FCO2_mmol)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(200,230),ylim = c(-15,15))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()
dev.off()

jpeg(filename='figs/JUL_AUGUST_CHL&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_JUL_AUG, aes(DOY,CHL)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(200,230),ylim = c(0,5))+
  theme_bw()
dev.off()

jpeg(filename='figs/JUL_AUGUST_ws&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_JUL_AUG, aes(DOY,WS)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(200,230),ylim = c(0,2.5))+
  theme_bw()
dev.off()

jpeg(filename='figs/JUL_AUGUST_TS&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_JUL_AUG, aes(DOY,TS)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(200,230),ylim = c(27.5,32.5))+
  theme_bw()
dev.off()

jpeg(filename='figs/JUL_AUGUST_H&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_JUL_AUG, aes(DOY,H)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(200,230),ylim = c(-0.5,3))+
  theme_bw()
dev.off()



###Decem_JANUARY ###
jpeg(filename='figs/janFCO2mmol&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(Janf, aes(DOY,FCO2_mmol)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(1,30),ylim = c(-15,15))+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_bw()
dev.off()

jpeg(filename='figs/decFCO2mmol&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(Decf, aes(DOY,FCO2_mmol)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(337,365),ylim = c(-15,15))+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_bw()
dev.off() 

jpeg(filename='figs/febFCO2mmol&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(Febf, aes(DOY,FCO2_mmol)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(30,62),ylim = c(-15,15))+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_bw()
dev.off()


##### PLOT all days of year ####
jpeg(filename='figs/all yearFCO2mmol&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_day, aes(DOY,FCO2_mmol)) + geom_point(alpha = 0.1) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  coord_cartesian(xlim = c(1,367),ylim = c(-15,15))+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_bw()
dev.off()


jpeg(filename='figs/all year CHL&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_merge_day, aes(DOY,CHL)) + geom_point(alpha = 0.1) +
geom_smooth(col = 'green', alpha = 0.2) +
coord_cartesian(xlim=c(1,367),ylim = c(0,5)) +
theme_bw()
dev.off()


jpeg(filename='figs/all year WS&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_merge_day, aes(DOY,WS)) + geom_point(alpha = 0.1) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  coord_cartesian(xlim=c(1,367),ylim = c(0,2.5)) +
  theme_bw()
dev.off()


jpeg(filename='figs/all year TS&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_merge_day, aes(DOY,TS)) + geom_point(alpha = 0.1) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  coord_cartesian(xlim=c(1,367),ylim = c(27.5,32.5)) +
  theme_bw()
dev.off()

jpeg(filename='figs/all year SST_SAT&DOY.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_merge_day, aes(DOY,SST)) + geom_point(alpha = 0.1) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  coord_cartesian(xlim=c(1,367),ylim = c(27.5,34)) +
  theme_bw()
dev.off()

#### day plot ####
jpeg(filename='figs/day &CHL.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_merge_day, aes(DOY,CHL)) + geom_point(alpha = 0.1) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  coord_cartesian(xlim=c(35,36),ylim = c(0,5)) +
  theme_bw()
dev.off()




#### CORRLATION PLOT ####
jpeg(filename='figs/cor_Chl&Fco2_feb_mar.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(df_feb_mar$CHL, df_feb_mar$FCO2,pch = 19,cex= 0.5, xlab= "", ylab="")
abline(lm(df_feb_mar$FCO2~df_feb_mar$CHL,data = df_feb_mar), col = "red")
mtext(side = 1, text = expression(paste('Chl', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')', sep = )),line = 2)
dev.off()


jpeg(filename='figs/cor_Chl&Fco2_JUL_AUGUT.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(df_JUL_AUG$CHL, df_JUL_AUG$FCO2,pch = 19,cex= 0.5, xlab= "", ylab="")
abline(lm(df_JUL_AUG$FCO2~df_JUL_AUG$CHL,data = df_JUL_AUG), col = "red")
mtext(side = 1, text = expression(paste('Chl', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')', sep = )),line = 2)
dev.off()

#####daily plot ####
df_30min<- selectByDate(df)
January<- selectByDate(df, month =1)
Jan2020<- selectByDate(January,year =2020)
Jan4<- selectByDate(df,month=1,day = 4)
Jan4_2020<- selectByDate(df,start = "2020-01-04", end = "2020-01-04")
Jan27_2020<- selectByDate(df,start = "2020-01-27", end = "2020-01-27")

jpeg(filename='figs/day jan_FCO2MMOL.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(Jan2020, aes(date,Jan2020$FCO2)) + geom_point(alpha = 0.1) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  coord_cartesian(ylim = c(-0.05,0.05))+
  theme_bw()
dev.off()

jpeg(filename='figs/day jan_FCO2MMOL.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(Jan4_2020, aes(date,Jan4_2020$FCO2_mmol)) + geom_point(alpha = 0.1) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  theme_bw()
dev.off()

jpeg(filename='figs/day jan_FCO2MMOL.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(Jan27_2020, aes(date,Jan27_2020$FCO2)) + geom_point(alpha = 0.1) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  theme_bw()
dev.off()

jpeg(filename='figs/day jan_WS.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(Jan4_2020, aes(date,Jan4_2020$WS)) + geom_point(alpha = 0.1) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  theme_bw()
dev.off()

jpeg(filename='figs/day jan_TS.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(Jan4_2020, aes(date,Jan4_2020$TS)) + geom_point(alpha = 0.1) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  theme_bw()
dev.off()
Jan
####PLOT POINT####

jpeg(filename='figs/marchFCO2&DOY.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(marchf$DOY, marchf$FCO2, xlab = '', ylab = '', col = "Red", 
     xaxt = "n", pch=19, ylim=c(-1,1))
abline(h=0)
#points(marchf$date,marchf$FCO2, pch = 19, col = "Red")
axis(side = 1, at = c(1:360))
dev.off()

jpeg(filename='figs/febFCO2&DOY.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

plot(Febf$DOY, Febf$FCO2, xlab = '', ylab = '', col = "Red", 
     xaxt = "n", pch=19, ylim=c(-1,1))
abline(h=0)
axis(side = 1, at = c(1:360))
dev.off()

jpeg(filename='figs/janFCO2&DOY.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

plot(Janf$DOY, Janf$FCO2, xlab = '', ylab = '', col = "Red", 
     xaxt = "n", pch=19, ylim=c(-1,1))
abline(h=0)
axis(side = 1, at = c(1:360))
dev.off()

jpeg(filename='figs/NovFCO2&DOY.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)


plot(Novf$DOY, Novf$FCO2, xlab = '', ylab = '', col = "Red", 
     xaxt = "n", pch=19, ylim=c(-1,1))
abline(h=0)

axis(side = 1, at = c(1:360))
dev.off()

jpeg(filename='figs/DecFCO2&DOY.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)


plot(Decf$DOY, Decf$FCO2, xlab = '', ylab = '', col = "Red", 
     xaxt = "n", pch=19, ylim=c(-1,1))
abline(h=0)

axis(side = 1, at = c(1:390))
dev.off()


jpeg(filename='figs/julyFCO2&DOY.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

plot(Julyf$DOY, Julyf$FCO2, xlab = '', ylab = '', col = "blue", 
     xaxt = "n", pch=19, ylim=c(-1,1))
abline(h=0)
axis(side = 1, at = c(1:360))
dev.off()

jpeg(filename='figs/AprFCO2&DOY.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

plot(Aprf$DOY, Aprf$FCO2, xlab = '', ylab = '', col = "blue", 
     xaxt = "n", pch=19, ylim=c(-1,1))
abline(h=0)
axis(side = 1, at = c(1:360))
dev.off()

jpeg(filename='figs/MayFCO2&DOY.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

plot(Mayf$DOY, Mayf$FCO2, xlab = '', ylab = '', col = "blue", 
     xaxt = "n", pch=19, ylim=c(-1,1))
abline(h=0)
axis(side = 1, at = c(1:360))
dev.off()

jpeg(filename='figs/JunFCO2&DOY.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

plot(Junf$DOY, Junf$FCO2, xlab = '', ylab = '', col = "blue", 
     xaxt = "n", pch=19, ylim=c(-1,1))
abline(h=0)
axis(side = 1, at = c(1:360))
dev.off()

jpeg(filename='figs/auguFCO2&DOY.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

plot(Augf$DOY, Augf$FCO2, xlab = '', ylab = '', col = "blue", 
     xaxt = "n", pch=19, ylim=c(-1,1))
abline(h=0)
axis(side = 1, at = c(1:360))
dev.off()

jpeg(filename='figs/SepFCO2&DOY.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

plot(Sepf$DOY, Sepf$FCO2, xlab = '', ylab = '', col = "blue", 
     xaxt = "n", pch=19, ylim=c(-1,1))
abline(h=0)
axis(side = 1, at = c(1:360))
dev.off()


jpeg(filename='figs/OctFCO2&DOY.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

plot(Octf$DOY, Octf$FCO2, xlab = '', ylab = '', col = "blue", 
     xaxt = "n", pch=19, ylim=c(-1,1))
abline(h=0)
axis(side = 1, at = c(1:360))
dev.off()



boxplot(df_feb_mar$FCO2,df_JUL_AUG$FCO2, ylim=c(-0.5,0.5))

library(ggplot2)
ggplot(marchf, aes(DOY,FCO2)) + geom_violin()
ggplot(Augf, aes(DOY,FCO2)) + geom_violin()

ggplot(marchf, aes(DOY,FCO2)) + geom_point() + geom_smooth()

ggplot(Augf, aes(DOY,FCO2)) + geom_point() + geom_smooth()




ggplot(marchf, aes(x= date, y = FCO2)) + geom_boxplot()





ggplot(df_feb_mar, aes(DOY,FCO2)) + geom_point() + geom_smooth() +
  coord_cartesian(xlim = c(40,85), ylim=c(-0.2,0.2))

####


#### Daily Averaged  for jan Feb and Mar ####
df_jan_feb_mar <- rbind(Janf, Febf,marchf)

ggplot(df_jan_feb_mar, aes(DOY,FCO2)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(0,90), ylim=c(-0.2,0.2)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_bw()

ggplot(df_jan_feb_mar, aes(DOY,WS)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(0,90),ylim = c(0.2,2.5))+
  theme_bw()

ggplot(df_sat, aes(date,CHL)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  theme_bw()
####


ggplot(df_feb_mar, aes(DOY,FCO2)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(31,90), ylim=c(-0.2,0.2)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_bw()






####


ggplot(Decf, aes(DOY,FCO2)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(345,365), ylim=c(-0.2,0.2)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_bw()

ggplot(Decf, aes(DOY,CHL)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  coord_cartesian(xlim = c(345,365), ylim=c(0,5)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_bw()

####


#### I want to average every hour of January####
# We need the dplyr package



#### Group the data according to hour of day ####

##### January ####
df_jan <- selectByDate(df,month = "Jan")
df_hour_jan <- df_jan %>%
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WDsin = mean(WDsin,na.rm=TRUE),
            RH = mean(RH,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE),
            PPFD = mean(PPFD,na.rm=TRUE),
            USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE),
            co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE))

# df_hour_jan <- df_jan %>%
# group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
# summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
# ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WD = mean(WD,na.rm=TRUE),
# RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
# RG = mean(RG,na.rm=TRUE),
# TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
# TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
# air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
# EMA = mean(EMA,na.rm=TRUE), PCO2_sw = mean(PCO2_sw,na.rm=TRUE))


df_hour_jan$hour <- as.numeric(df_hour_jan$hour)



jpeg(filename='figs/Jan_FCO2mmol.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_hour_jan, aes(hour,FCO2_mmol)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_bw()
dev.off()

jpeg(filename='figs/Jan_WS.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_hour_jan, aes(hour,WS)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  theme_bw()
dev.off()

jpeg(filename='figs/Jan_TS.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_hour_jan, aes(hour,TS)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  theme_bw()
dev.off()


jpeg(filename='figs/Jan_PPFD.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_hour_jan, aes(hour,PPFD)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  theme_bw()
dev.off()
##### June ####
df_jun <- selectByDate(df, month = "Jun")
df_hour_jun <- df_jun %>% 
  group_by(hour=format(as.POSIXct(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WDsin = mean(WDsin,na.rm=TRUE),
            RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE))


df_hour_jun$hour <- as.numeric(df_hour_jun$hour)

jpeg(filename='figs/Jun_FCO2mmol.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_hour_jun, aes(hour,FCO2_mmol)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_bw()
dev.off()

jpeg(filename='figs/jun_WS.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_hour_jun, aes(hour,WS)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'green', alpha = 0.1, method = "loess") +
  coord_cartesian(xlim = c(0,23),ylim = c(0.12,3))+
  theme_bw()
dev.off()

jpeg(filename='figs/jun_TS.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_hour_jun, aes(hour,TS)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  theme_bw()
dev.off()


jpeg(filename='figs/jun_PPFD.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_hour_jun, aes(hour,PPFD)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  theme_bw()
dev.off()


####plot two in one 2 in 1######

jpeg(filename='figs/jan_jun_TS.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

ggplot(df_hour_jan, aes(hour,TS)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'red', alpha = 0.2) +
  geom_point(data = df_hour_jun, aes(as.integer(hour), TS), alpha =0.2) +
  geom_smooth(data = df_hour_jun, aes(as.integer(hour), TS), col= "blue", alpha = 0.2) +
  theme_bw()

dev.off()

jpeg(filename='figs/jan_jun_wS2.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

ggplot(df_hour_jan, aes(hour,WS)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'red', alpha = 0.2) +
  geom_point(data = df_hour_jun, aes(as.integer(hour), WS), alpha =0.2) +
  geom_smooth(data = df_hour_jun, aes(as.integer(hour), WS), col= "blue", alpha = 0.2) +
  theme_bw()

dev.off()


jpeg(filename='figs/jan_jun_FCO2MMOL.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

ggplot(df_hour_jan, aes(hour,FCO2_mmol)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'red', alpha = 0.2) +
  geom_point(data = df_hour_jun, aes(as.integer(hour), FCO2_mmol), alpha =0.2) +
  geom_smooth(data = df_hour_jun, aes(as.integer(hour), FCO2_mmol), col= "blue", alpha = 0.2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()

dev.off()

#df_hour_jan1 <- df_hour_jan[1:7,]
#df_hour_jun1 <- df_hour_jun[1:7,]

jpeg(filename='figs/jan_jun_WDsin.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_hour_jan, aes(hour,WDsin)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'red', alpha = 0.2) +
  geom_point(data = df_hour_jun, aes(hour,WDsin), alpha =0.2) +
  geom_smooth(data = df_hour_jun, aes(hour,WDsin), col= "blue", alpha = 0.2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()
dev.off()


#### mean and sd #####
mean_df_day  <- sapply(df_merge_day[c(2,3,4,5,15,17,18,20,27,28,32,33,36)], mean, na.rm=T)
sd_df_day <- sapply(df_merge_day[c(2,3,4,5,15,17,18,20,27,28,32,33,36)], sd, na.rm=T)
df_statistic_day <- data.frame(rbind(mean_df_day,sd_df_day))
row.names(df_statistic_day) <- c("Mean","SD")

# mean_df_jan  <- sapply(na.omit(Janf[c(1,2,3,4,5,6,7,8,9,10,11,12)]), mean)
# sd_df_jan <- sapply(na.omit(Janf[c(1,2,3,4,5,6,7,8,9,10,11,12)]), sd)
# df_statistic_jan <- data.frame(rbind(mean_df_jan,sd_df_jan))
# row.names(df_statistic_jan) <- c("Mean","SD")
# 
# mean_df_jun  <- sapply(na.omit(Junf[c(1,2,3,4,5,6,7,8,9,10,11,12)]), mean)
# sd_df_jun <- sapply(na.omit(Junf[c(1,2,3,4,5,6,7,8,9,10,11,12)]), sd)
# df_statistic_jun <- data.frame(rbind(mean_df_jun,sd_df_jun))
# row.names(df_statistic_jun) <- c("Mean","SD")

mean_df_NEM_day  <- sapply(df_NEM_day, mean,na.rm=T)
sd_df_NEM_day <- sapply(df_NEM_day, sd,na.rm=T)
df_statistic_NEM<- data.frame(rbind(mean_df_NEM_day,sd_df_NEM_day))
row.names(df_statistic_NEM) <- c("Mean","SD")


mean_df_SWM_day  <- sapply(df_SWM_day, mean,na.rm=T)
sd_df_SWM_day <- sapply(df_SWM_day, sd,na.rm=T)
df_statistic_SWM<- data.frame(rbind(mean_df_SWM_day,sd_df_SWM_day))
row.names(df_statistic_SWM) <- c("Mean","SD")





######correlation daily time #####
corPlot(df_merge_day)
corPlot(df_NEM_day)
corPlot(df_SWM_day)
cor((df_merge_day)[c(27,3,32,20,15,33,4,36)], method = "pearson", use ="complete.obs")
cor((df_NEM_day)[c(9,2,10,8,5,11,3,12)], method = "pearson", use ="complete.obs")
cor((df_SWM_day)[c(9,2,10,8,5,11,3,12)], method = "pearson", use ="complete.obs")



library(sjPlot)

tab_corr(df_merge_day)

tab_corr((df_merge_day)[c(27,3,32,20,15,33,4,36)],na.deletion = c("listwise"),
         show.p = TRUE,corr.method = c("spearman"),p.numeric = F)

tab_corr((df_NEM_day)[c(9,2,11,8,5,12,3)],na.deletion = c("listwise"),
         show.p = TRUE,corr.method = c("spearman"),p.numeric = F)


tab_corr((df_SWM_day)[c(9,2,11,8,5,12,3)],na.deletion = c("listwise"),
         show.p = TRUE,corr.method = c("spearman"),p.numeric = F)


# tab_corr((df_merge_day)[c(27,3,32,20,15,33,4,36)],na.deletion = c("listwise"),
#          show.p = TRUE,corr.method = c("pearson"),p.numeric = F)

##### p-value ####

t.test(df_NEM_day$CHL,df_SWM_day$CHL)
t.test(df_NEM_day$WS,df_SWM_day$WS)
t.test(df_NEM_day$TS,df_SWM_day$TS)
t.test(df_NEM_day$SST,df_SWM_day$SST)
t.test(df_merge_day$SST,df_merge_day$TS)

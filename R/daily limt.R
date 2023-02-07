
##### partition of date #####
library(openair)

marchf <- selectByDate(df_merge_day,month = 3)[,c(1,2,3,4,5,8,9,15,17,18,20,27,31)]
Febf <- selectByDate(df_merge_day,month = 2)[,c(1,2,3,4,5,8,9,15,17,18,20,27,31)]
Janf <- selectByDate(df_merge_day,month = 1)[,c(1,2,3,4,5,8,9,15,17,18,20,27,31)]
Novf <- selectByDate(df_merge_day,month = 11)[,c(1,2,3,4,5,8,9,15,17,18,20,27,31)]
Decf <- selectByDate(df_merge_day,month = 12)[,c(1,2,3,4,5,8,9,15,17,18,20,27,31)]
Julyf <- selectByDate(df_merge_day,month = 7)[,c(1,2,3,4,5,8,9,15,17,18,20,27,31)]
Aprf <- selectByDate(df_merge_day,month = 4)[,c(1,2,3,4,5,8,9,15,17,18,20,27,31)]
Mayf <- selectByDate(df_merge_day,month = 5)[,c(1,2,3,4,5,8,9,15,17,18,20,27,31)]
Junf <- selectByDate(df_merge_day,month = 6)[,c(1,2,3,4,5,8,9,15,17,18,20,27,31)]
Augf <- selectByDate(df_merge_day,month = 8)[,c(1,2,3,4,5,8,9,15,17,18,20,27,31)]
Sepf <- selectByDate(df_merge_day,month = 9)[,c(1,2,3,4,5,8,9,15,17,18,20,27,31)]
Octf <- selectByDate(df_merge_day,month = 10)[,c(1,2,3,4,5,8,9,15,17,18,20,27,31)]


#### WD filter ####
df$WD[df$WD > 45 & df$WD < 315] <- NA

WDsin <- sin(df$WD * pi/180)

df <- cbind(df,WDsin)



##### add chl to the data #####
CHL_day <- selectByDate(df_sat)[,c(1,4)]

CHL_day$date <- strptime(CHL_day$date, 
                         format = "%Y-%m-%d", 
                         tz = "Asia/Kuala_Lumpur")
CHL_day$date <- as.POSIXct.POSIXlt(CHL_day$date)


SST_DAY_SAT <-selectByDate(df_sat)[,c(1,5)]
SST_DAY_SAT$date <- strptime(SST_DAY_SAT$date,format ="%Y-%m-%d", 
                             tz = "Asia/Kuala_Lumpur")
SST_DAY_SAT$date <- as.POSIXct.POSIXlt(SST_DAY_SAT$date)
df_merge_day <- merge(df_day,CHL_day, by= "date")
df_merge_day <- merge(df_merge_day,SST_DAY_SAT, by= "date")
df_merge_day$CHL[which(df_merge_day$CHL> 7)] <- NA


#### Daily Averaged FCO2 for Feb and Mar ...... ####
df_Dec_jan_feb <- rbind(Decf, Janf, Febf)
df_feb_mar <- rbind(Febf,marchf)
df_JUL_AUG <- rbind(Julyf,Augf)
df_jan_Dec<- rbind(Decf, Janf)
##### plot #####
library(ggplot2)

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
  coord_cartesian(,ylim = c(-0.05,0.05))+
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


#### Daily Averaged FCO2 for Aug ####


ggplot(Augf, aes(DOY,FCO2)) + geom_point() + geom_smooth() +
  coord_cartesian(ylim=c(-0.20,0.20))

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


# I want to average every hour of January
# We need the dplyr package

library(dplyr)

#### Group the data according to hour of day ####

##### January ####
df_jan <- selectByDate(df,month = "Jan")
df_hour_jan <- df_jan %>% 
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WDsin = mean(WDsin,na.rm=TRUE),
            RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
            EMA = mean(EMA,na.rm=TRUE), PCO2_sw = mean(PCO2_sw,na.rm=TRUE))


df_hour_jan$hour <- as.numeric(df_hour_jan$hour)


library(ggplot2)
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
df_jun <- selectByDate(df,month = "Jun")
df_hour_jun <- df_jun %>% 
  group_by(hour=format(as.POSIXlt(cut(date,breaks='hour')),'%H')) %>%
  summarise(FCO2=mean(FCO2,na.rm=TRUE), LE = mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE),
            ZL = mean(ZL,na.rm=TRUE), WS = mean(WS,na.rm=TRUE), WDsin = mean(WDsin,na.rm=TRUE),
            RH = mean(RH,na.rm=TRUE), P_RAIN = mean(P_RAIN,na.rm=TRUE),
            RG = mean(RG,na.rm=TRUE),
            TA = mean(TA,na.rm=TRUE), PPFD = mean(PPFD,na.rm=TRUE), USTAR = mean(USTAR,na.rm=TRUE),
            TS = mean(TS,na.rm=TRUE), co2_mole_fraction = mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure = mean(air_pressure,na.rm=TRUE), FCO2_mmol = mean(FCO2_mmol,na.rm=TRUE),
            EMA = mean(EMA,na.rm=TRUE), PCO2_sw = mean(PCO2_sw,na.rm=TRUE))


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
  geom_smooth(col = 'green', alpha = 0.2) +
  geom_point(data = df_hour_jun, aes(as.integer(hour), TS), alpha =0.2) +
  geom_smooth(data = df_hour_jun, aes(as.integer(hour), TS), col= "red", alpha = 0.2) +
  theme_bw()

dev.off()

jpeg(filename='figs/jan_jun_wS.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

ggplot(df_hour_jan, aes(hour,WS)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  geom_point(data = df_hour_jun, aes(as.integer(hour), WS), alpha =0.2) +
  geom_smooth(data = df_hour_jun, aes(as.integer(hour), WS), col= "red", alpha = 0.2) +
  theme_bw()

dev.off()


jpeg(filename='figs/jan_jun_FCO2MMOL.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)

ggplot(df_hour_jan, aes(hour,FCO2_mmol)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'green', alpha = 0.2) +
  geom_point(data = df_hour_jun, aes(as.integer(hour), FCO2_mmol), alpha =0.2) +
  geom_smooth(data = df_hour_jun, aes(as.integer(hour), FCO2_mmol), col= "red", alpha = 0.2) +
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()

dev.off()

df_hour_jan1 <- df_hour_jan[1:7,]
df_hour_jun1 <- df_hour_jun[1:7,]

ggplot(df_hour_jan, aes(hour,WDsin)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'green', alpha = 0.2) 

ggplot(df_hour_jun, aes(hour,WDsin)) + geom_point(alpha = 0.2) + 
  geom_smooth(col = 'green', alpha = 0.2) 
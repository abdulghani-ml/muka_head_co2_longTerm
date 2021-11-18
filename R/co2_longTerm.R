#### 1. Preliminaries #########################################
require(openair)
require(dplyr)
source('R/tools/tool_convert_magic.R')
source('R/tools/tool_charactersNumeric.R')
source('R/tools/tool_trapezium_intg_2.R')
source('R/tools/tool_trapezium_intg_3.R')


#### SATELLITE DATA ANALYSIS ####
#### import sat data ###############
chl <- read.csv('data/satellite/5year_chlor_Aqua.csv')
sst <- read.csv('data/satellite/5year_SST_Aqua.csv')
poc <- read.csv('data/satellite/5year_POC_Aqua.csv')
pic <- read.csv('data/satellite/5year_PIC_Aqua.csv')
par <- read.csv('data/satellite/5year_PAR_Aqua.csv')

#### merge variables sat data #####
#df1 <- merge(chl,sst,par,poc,pic,by= c('DATE'),all=TRUE)

x1<- merge(chl,sst)
x2<- merge(poc,pic)
x3<- merge(x1,x2)
df_sat<- merge(x3,par)
rm(chl,par,pic,poc,sst,x1,x2,x3)

# Remove letters in date

df_sat$DATE <- gsub("T"," ", df_sat$DATE)         # This is to remove the letter T
df_sat$DATE <- strptime(df_sat$DATE, 
                        format = "%Y-%m-%d %H:%M:%OS", 
                        tz = "Asia/Kuala_Lumpur")
#class (df_sat$DATE)                              #To check the format, make sure is POSIXlt POSIXt
df_sat$DATE <- as.POSIXct.POSIXlt(df_sat$DATE)    #Change to POSIXct format

df_sat<- df_sat[which(df_sat$DATE >= as.POSIXct(as.Date("2016-01-01")) 
                       & df_sat$DATE <= as.POSIXct(as.Date("2020-12-31"))),]

# Changing all the '-32767.0' or '-32767' (missing data) to NA
for (i in 4:8){                                   #This means column number 4 to coloumn number 8
  df_sat[i][df_sat[i] == '-32767' | df_sat[i] == '-32767.0'] <- NA
}
rm(i)

#### Averaging sat data #####

# Need to change name DATE to date
colnames(df_sat)[1] <- "date"

#Average data to monthly intervals
df_sat_month <- timeAverage(df_sat, avg.time = "1 month")

#Average data to yearly intervals
df_sat_year <- timeAverage(df_sat, avg.time = "1 year")


#### EC DATA ANALYSIS ####

### import EC data######
df_ec<-read.csv('data/station/MCO-MUKA21_full_output.csv')
df_biomet<- read.csv('data/station/biomet data 21.csv')

# Delete unnecessary columns and rows in EC data files
df_ec<- df_ec[-1,]  #remove the 1st row 


# Delete unnecessary columns and rows in biomet data files
df_biomet<- df_biomet[,c(-6,-7)] #remove (DOY,unamed)


#Rename the date
colnames(df_ec)[1] <- "DATE"
colnames(df_biomet)[7] <- "DATE"

#### merge df with df_bimet ####
df <- merge(df_ec,df_biomet,by= c('DATE'))


# Using convert_magic to convert all columns to 'character' first
df <- convert_magic(df[,c(seq(1,ncol(df)))],c(rep('character',times = ncol(df))))

# Changing all the '-9999.0' or '-9999' (missing data) to NA
for (i in 1:length(df)){
  df[i][df[i] == '-9999' | df[i] == '-9999.0' | df[i] == '--'] <- NA
}
rm(i)

# Change all non-factors (or characters) to numeric 
df[,-1] <- charactersNumeric(df[,-1])

# To check the class
#sapply(df,class)

# Change the date to POSIXCT format
df$DATE <- strptime(df$DATE, format = "%Y-%m-%d %H:%M", tz = "Asia/Kuala_Lumpur")
#class(df$DATE)
df$DATE <- as.POSIXct.POSIXlt(df$DATE)

#Change name DATE to date
colnames(df)[1] <- "date"

# extracting related variables from raw variables
df <- data.frame(df$date,df$DOY,df$WS,df$WD,
                 df$FCO2,df$RN_1_1_1,df$RH_1_1_1,df$H2O,
                 df$H,df$LE,df$ZL,df$SH,df$SLE,
                 df$P_RAIN_1_1_1,df$TA_1_1_1,df$ET,
                 df$PPFD_1_1_1,df$USTAR,df$RG_1_1_1,
                 df$TS_1_1_1,df$H_QC,df$LE_QC,df$FCO2_QC)


#Rename variables
colnames(df) <- c("date","DOY","WS","WD",
                  "FCO2","RN","RH","H2O",
                  "H","LE","ZL","SH","SLE",
                  "P_RAIN","TA","ET","PPFD",
                  "USTAR","RG","TS","H_QC","LE_QC","FCO2_QC")

# Convert TA, TS from K to Celcius
df$TA<- df$TA - 273.15
df$TS<- df$TS - 273.15


# Remove all improbable values of T
df$TA[which(df$TA < 0 | df$TA > 100 )] <- NA
df$TS[which(df$TS < 0 )] <- NA

# random filter
df$TA[df$TA < 20 | df$TA > 40] <- NA
df$TS[df$TS < 20 | df$TS > 40] <- NA
#df$SST[df$SST < 10 | df$SST > 50] <- NA

df$RH[df$RH < 25 | df$RH > 100] <- NA

df$RN[df$RN < -100] <- NA

df$RN[df$RN > 400] <- NA

df$LE[which(df$LE_QC == 2)] <- NA 

df$H[which(df$H_QC == 2)] <- NA 

df$FCO2[which(df$FCO2_QC == 2)] <- NA


#### atmospheric stability values#####

unstable <- df$ZL[which(df$ZL < -0.1)]          #unstable, previously y  
neutral <- df$ZL[(df$ZL > -0.1 & df$ZL < 0.1)]   #neutral, previously z
stable <- df$ZL[which(df$ZL > 0.1)]             #stable, previously x

# Average to 1 month
df_month <- timeAverage(df, avg.time = "1 month")

# Average to 1 year
df_year <- timeAverage(df, avg.time = "1 year")

# Merge satellite and eddy covariance data by month
df_merge_month <- merge(df_month, df_sat_month, by = "date")

# Merge satellite and eddy covariance data by year
df_merge_year <- merge(df_year, df_sat_year, by = "date")

# Random Filter after merge
df_merge_month$CHL[df_merge_month$CHL > 3] <- NA
df_merge_month$TS[df_merge_month$TS > 34] <- NA

rm(df_biomet, df_ec)

#### partitioning monsoon analysis ####

NEM <- selectByDate(df_merge_month, month = c(12,1,2,3))
SWM <- selectByDate(df_merge_month, month = c(6,7,8,9))
FTM <- selectByDate(df_merge_month, month = c(10,11))
STM <- selectByDate(df_merge_month, month = c(4,5))

# # Mean and SD for each monsoon
# mean_1 <- sapply(na.omit(NEM[c(-1,-2,-21,-22,-23,-24,-25)]), mean)
# sd_1 <- sapply(na.omit(NEM[c(-1,-2,-21,-22,-23,-24,-25)]), sd)
# df_NEM <- data.frame(matrix(c(mean_1,sd_1),ncol = 2))
# colnames(df_NEM) <- c("Mean_NEM","SD_NEM")
# 
# mean_1 <- sapply(na.omit(SWM[c(-1,-2,-21,-22,-23,-24,-25)]), mean)
# sd_1 <- sapply(na.omit(SWM[c(-1,-2,-21,-22,-23,-24,-25)]), sd)
# df_SWM <- data.frame(matrix(c(mean_1,sd_1),ncol = 2))
# colnames(df_SWM) <- c("Mean_SWM","SD_SWM")
# 
# mean_1 <- sapply(na.omit(FTM[c(-1,-2,-21,-22,-23,-24,-25)]), mean)
# sd_1 <- sapply(na.omit(FTM[c(-1,-2,-21,-22,-23,-24,-25)]), sd)
# df_FTM <- data.frame(matrix(c(mean_1,sd_1),ncol = 2))
# colnames(df_FTM) <- c("Mean_FTM","SD_FTM")
# 
# mean_1 <- sapply(na.omit(STM[c(-1,-2,-21,-22,-23,-24,-25)]), mean)
# sd_1 <- sapply(na.omit(STM[c(-1,-2,-21,-22,-23,-24,-25)]), sd)
# df_STM <- data.frame(matrix(c(mean_1,sd_1),ncol = 2))
# colnames(df_STM) <- c("Mean_STM","SD_STM")
# 
# df_monsoon <- cbind(df_NEM,df_SWM,df_FTM,df_STM)
# row.names(df_monsoon) <- colnames(SWM[c(-1,-2,-21,-22,-23,-24,-25)])

# Corelation monsoon 
corPlot(df_merge_month)
corPlot(NEM) #PRAIN, PPFD
corPlot(SWM) #RG,SH
corPlot(FTM) #PPFD, RN
corPlot(STM) #PRAIN,RG

plot(NEM$P_RAIN, NEM$FCO2, xlab= 'Preciptation', ylab='CO2 Flux')
abline(lm(NEM$FCO2~NEM$P_RAIN,data = NEM), col = "red")

plot(NEM$PPFD, NEM$FCO2, xlab= 'Photosynthesis', ylab='CO2 Flux')
abline(lm(NEM$FCO2~NEM$PPFD,data = NEM), col = "red")

plot(SWM$RG, SWM$FCO2, xlab= 'Radiation', ylab='CO2 Flux')
abline(lm(SWM$FCO2~SWM$RG,data = SWM), col = "red")

plot(SWM$SH, SWM$FCO2, xlab= 'H strgh', ylab='CO2 Flux')
abline(lm(SWM$FCO2~SWM$SH,data = SWM), col = "red")

plot(FTM$PPFD, FTM$FCO2, xlab= 'Photosynthesis', ylab='CO2 Flux')
abline(lm(FTM$FCO2~FTM$PPFD,data = FTM), col = "red")

plot(FTM$RN, FTM$FCO2, xlab= 'Net Radiation', ylab='CO2 Flux')
abline(lm(FTM$FCO2~FTM$RN,data = FTM), col = "red")

plot(STM$P_RAIN, STM$FCO2, xlab= 'Precipitation', ylab='CO2 Flux')
abline(lm(STM$FCO2~STM$P_RAIN,data = STM), col = "red")

plot(STM$RG, STM$FCO2, xlab= 'Global Radiation', ylab='CO2 Flux')
abline(lm(STM$FCO2~STM$RG,data = STM), col = "red")

plot(SWM$PPFD, SWM$FCO2, xlab= 'PPFD', ylab='CO2 Flux', main="SWM")
abline(lm(SWM$FCO2~SWM$PPFD,data = SWM), col = "red")

#write.table(df_merge_month,file = "monthly_data.csv", sep = ",",row.names = FALSE )

#write.table(df_merge_year,file = "yearly_data.csv", sep = ",",row.names = FALSE )


#partition stable and unstable dataset          
df_merge_month_unstable <- subset(df_merge_month,df_merge_month$ZL < 0) #convection
df_merge_month_unstable <- data.frame(df_merge_month_unstable,abs(df_merge_month_unstable$ZL))# convert all negative values to +ve to make logarithmic plot
colnames(df_merge_month_unstable)[29] <-'abs_zL'

df_merge_month_stable <- subset(df_merge_month, df_merge_month$ZL>=0)
#partitioning co2 positive and -ve values
CO2_positive <- subset(df_merge_month,df_merge_month$FCO2 > 0)
CO2_negative <- subset(df_merge_month,df_merge_month$FCO2 < 0)





##################################################################################
#plotting co2
plot(log(CO2_positive$WS), log(CO2_positive$FCO2), xlab= 'Wind Speed', ylab='CO2 Flux')
lmEB<- lm(CO2_positive$WS ~ CO2_positive$FCO2,data=CO2_positive)
summary(lmEB)
# Check the correlated or no? To check the correlation test (Pearson Coreelation) 
cor.test(CO2_positive$WS,CO2_positive$FCO2) #pvalue 0.06 no corelation
cor.test(CO2_positive$TA,CO2_positive$FCO2) #pvalue 0.01 got corelation
cor.test(CO2_positive$SST,CO2_positive$FCO2) #p-value 0.75 no corelation
cor.test(CO2_positive$ZL,CO2_positive$FCO2) # p-value 0.76 no corelation
cor.test(CO2_positive$PAR,CO2_positive$FCO2) #p-value 0.8727 no corelation
corPlot(CO2_positive)


cor.test(df_merge_month$ET,df_merge_month$FCO2) #p-value = 9.162e-08 got correlation
cor.test(df_merge_month$LE,df_merge_month$FCO2) #p-value = 1.97e-07 got correlation
corPlot((df_merge_month)) #this one can show Dr



#Daily corelation
df_day<- timeAverage(df, avg.time = "1 day")
cor.test(df_day$FCO2,df_day$WS)
#30 min corelation
cor.test(df$FCO2,df$WS)

#creating windRose
library(openair)
windRose(NEM, ws="WS",wd='WD')
windRose(SWM, ws="WS",wd='WD')





#DATA ANALYSIS
#Time series
#FCO2
# jpeg(filename='fig/Fco2_year.jpg', unit = 'cm', width = 12, height = 10, res = 360)
# par (mar = c(4,5,1,1))  #TIME SERIES
# plot(df_merge_month$date,df_merge_month$FCO2, type = 'l', xlab = 'Time', 
#      ylab=expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")))
# dev.off()
# 
# 
# jpeg(filename='fig/Rain_year.jpg', unit = 'cm', width = 12, height = 10, res = 360)
# par (mar = c(4,5,1,1))  #TIME SERIES
# plot(df_merge_month$date,df_merge_month$P_RAIN, type = 'l', xlab = 'Time', 
#      ylab=expression(paste('Cumulative', "", 'rian (','m','m',')' ,sep = "")))
# dev.off()
# 
# jpeg(filename='fig/PPFD_year.jpg', unit = 'cm', width = 12, height = 10, res = 360)
# par (mar = c(4,5,1,1))  #TIME SERIES
# plot(df_merge_month$date,df_merge_month$PPFD, type = 'l', xlab = 'Time', 
#      ylab=expression(paste('PPDF (','µ','molC','','m'^{'-2'}, 's'^{'-1'},')',sep = "")))
# dev.off()
# 
# 
# jpeg(filename='fig/TA_year.jpg', unit = 'cm', width = 12, height = 10, res = 360)
# par (mar = c(4,5,1,1))  #TIME SERIES
# plot(df_merge_month$date,df_merge_month$TA, type = 'l', xlab = 'Time', 
#      ylab=expression(paste('TA (','C'^{O},')',sep = "")))
#    
# dev.off()
# 
# 
# 
# jpeg(filename='fig/LE_year.jpg', unit = 'cm', width = 12, height = 10, res = 360)
# par (mar = c(4,5,1,1))  #TIME SERIES
# plot(df_merge_month$date,df_merge_month$LE, type = 'l', xlab = 'Time', 
#      ylab=expression(paste('LE(','W','m'^{'-2'},')',sep = "")))
# 
# dev.off()
# 
# 
# jpeg(filename='fig/H_year.jpg', unit = 'cm', width = 12, height = 10, res = 360)
# par (mar = c(4,5,1,1))  #TIME SERIES
# plot(df_merge_month$date,df_merge_month$H, type = 'l', xlab = 'Time', 
#      ylab=expression(paste('H(','W','m'^{'-2'},')',sep = "")))
# 
# dev.off()
# 
# 
# jpeg(filename='fig/RN&RG_year.jpg', unit = 'cm', width = 12, height = 10, res = 360)
# par (mar = c(4,5,1,1))  #TIME SERIES
# plot(df_merge_month$date,df_merge_month$RG, type = 'p',pch = 19, xlab = 'Time', col='orange',ylim = c(-100,500),
#      ylab=expression(paste('RN','&','RG (','W','m'^{'-2'},')',sep = "")))
# lines(df_merge_month$date,df_merge_month$RG,col = 'orange')
# lines(df_merge_month$date,df_merge_month$RN,col='red')
# points(df_merge_month$date,df_merge_month$RN,col='red', pch = 19)
# dev.off() 
# 
# jpeg(filename='fig/USTAR.jpg', unit = 'cm', width = 12, height = 10, res = 360)
# par (mar = c(4,5,1,1))  #TIME SERIES
# plot(df_merge_month$date,df_merge_month$USTAR, type = 'l', xlab = 'Time', 
#      ylab=expression(paste('U*(','m'^{'1'},'s'^{'-1'},')',sep = "")))
# 
# dev.off()


#plot(df_merge_month$date,df_merge_month$H, type = 'l', xlab = 'Time',  
#     ylab='Sensible Heat (Wm')

#plot(SWM$date,SWM$LE,  type = 'l', xlab = 'Time', 
#     ylab='Sensible Heat (Wm')



#df_merge_month$H[which(df_merge_month$H < -1 | df_merge_month$H > 2 )] <- NA
#library(openair)
#NEM_date <- selectByDate(df_merge_month, month = c(12))[,1]
#SWM_date <- selectByDate(df_merge_month, month = c(6))[,1]
#FTM_date <- selectByDate(df_merge_month, month = c(10))[,1]
#STM_date <- selectByDate(df_merge_month, month = c(4))[,1]

library(openair)
FCO2_2015 <- selectByDate(df_merge_month, year = 2015)[,c(1,5)]
FCO2_2016 <- selectByDate(df_merge_month,year = 2016)[,c(1,5)]
FCO2_2017 <- selectByDate(df_merge_month,year = 2017)[,c(1,5)]
FCO2_2018 <- selectByDate(df_merge_month,year = 2018)[,c(1,5)]
FCO2_2019 <- selectByDate(df_merge_month,year = 2019)[,c(1,5)]
FCO2_2020 <- selectByDate(df_merge_month,year = 2020)[,c(1,5)]
FCO2_2015$date <- format(as.POSIXct(FCO2_2015$date),"%m")
FCO2_2016$date <- format(as.POSIXct(FCO2_2016$date),"%m")
FCO2_2017$date <- format(as.POSIXct(FCO2_2017$date),"%m")
FCO2_2018$date <- format(as.POSIXct(FCO2_2018$date),"%m")
FCO2_2019$date <- format(as.POSIXct(FCO2_2019$date),"%m")
FCO2_2020$date <- format(as.POSIXct(FCO2_2020$date),"%m")

jpeg(filename='fig/FCO2_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(FCO2_2016$date, FCO2_2016$FCO2, type = 'l', xlab = '', ylab = '', col = "Red", ylim = c(-0.15,0.10), xaxt = "n")
points(FCO2_2016$date,FCO2_2016$FCO2, pch = 19, col = "Red")
points(FCO2_2017$date,FCO2_2017$FCO2, pch = 19, col = "Orange")
lines(FCO2_2017$date,FCO2_2017$FCO2, pch = 19, col = "Orange")
points(FCO2_2018$date,FCO2_2018$FCO2, pch = 19, col = "Blue")
lines(FCO2_2018$date,FCO2_2018$FCO2, pch = 19, col = "Blue")
points(FCO2_2019$date,FCO2_2019$FCO2, pch = 19, col = "Purple")
lines(FCO2_2019$date,FCO2_2019$FCO2, pch = 19, col = "Purple")
points(FCO2_2020$date,FCO2_2020$FCO2, pch = 19, col = "Black")
lines(FCO2_2020$date,FCO2_2020$FCO2, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c( "Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()

#windowsFonts("Times New Roman"=windowsFont("TT Times New Roman")) 
#FCO2
jpeg(filename='fig/FCO2_all_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par (mar = c(4,4,1,1)
     #,family = "Times New Roman" 
)
plot(df_merge_month$date,df_merge_month$FCO2, type = 'l', xlab = '', ylab = '')
points(df_merge_month$date,df_merge_month$FCO2, pch = 19)
#mtext('Year', side = 1, line = 2)
mtext(expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), side = 2, line = 2)
#abline(v= NEM_date, lty = 2,col = "red" )
#abline(v= SWM_date, lty = 2, col = "blue")
#abline(v= FTM_date, lty = 2, col = "blue")
#abline(v = STM_date, lty = 2, col = "red")
#legend("bottomright",legend = c("NEM","SWM"), col = c("red","blue"), lty = 2)
dev.off()

####CHL####
CHL_2016 <- selectByDate(df_merge_month,year = 2016)[,c(1,26)]
CHL_2017 <- selectByDate(df_merge_month,year = 2017)[,c(1,26)]
CHL_2018 <- selectByDate(df_merge_month,year = 2018)[,c(1,26)]
CHL_2019 <- selectByDate(df_merge_month,year = 2019)[,c(1,26)]
CHL_2020 <- selectByDate(df_merge_month,year = 2020)[,c(1,26)]
CHL_2016$date <- format(as.POSIXct(CHL_2016$date),"%m")
CHL_2017$date <- format(as.POSIXct(CHL_2017$date),"%m")
CHL_2018$date <- format(as.POSIXct(CHL_2018$date),"%m")
CHL_2019$date <- format(as.POSIXct(CHL_2019$date),"%m")
CHL_2020$date <- format(as.POSIXct(CHL_2020$date),"%m")

jpeg(filename='fig/CHL_years2.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(CHL_2016$date, CHL_2016$CHL, type = 'l', xlab = '', ylab = '', ylim = c(0.3,3.2),col = "Red", xaxt = "n")
points(CHL_2016$date,CHL_2016$CHL, pch = 19, col = "Red")
points(CHL_2017$date,CHL_2017$CHL, pch = 19, col = "Orange")
lines(CHL_2017$date,CHL_2017$CHL, pch = 19, col = "Orange")
points(CHL_2018$date,CHL_2018$CHL, pch = 19, col = "Blue")
lines(CHL_2018$date,CHL_2018$CHL, pch = 19, col = "Blue")
points(CHL_2019$date,CHL_2019$CHL, pch = 19, col = "Purple")
lines(CHL_2019$date,CHL_2019$CHL, pch = 19, col = "Purple")
points(CHL_2020$date,CHL_2020$CHL, pch = 19, col = "Black")
lines(CHL_2020$date,CHL_2020$CHL, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('Chlorophyll')), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c("Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()

jpeg(filename='fig/CHL_all_years2.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par (mar = c(4,4,1,1)
     #,family = "Times New Roman" 
)
plot(df_merge_month$date,df_merge_month$CHL, type = 'l', xlab = '', ylab = '')
points(df_merge_month$date,df_merge_month$CHL, pch = 19)
#mtext('Year', side = 1, line = 2)
mtext(expression(paste('Chlorophyll', sep = "")), side = 2, line = 2)
#abline(v= NEM_date, lty = 2,col = "red" )
#abline(v= SWM_date, lty = 2, col = "blue")
#abline(v= FTM_date, lty = 2, col = "blue")
#abline(v = STM_date, lty = 2, col = "red")
#legend("bottomright",legend = c("NEM","SWM"), col = c("red","blue"), lty = 2)
dev.off()

####WD####
WD_2016 <- selectByDate(df_merge_month,year = 2016)[,c(1,4)]
WD_2017 <- selectByDate(df_merge_month,year = 2017)[,c(1,4)]
WD_2018 <- selectByDate(df_merge_month,year = 2018)[,c(1,4)]
WD_2019 <- selectByDate(df_merge_month,year = 2019)[,c(1,4)]
WD_2020 <- selectByDate(df_merge_month,year = 2020)[,c(1,4)]
WD_2016$date <- format(as.POSIXct(WD_2016$date),"%m")
WD_2017$date <- format(as.POSIXct(WD_2017$date),"%m")
WD_2018$date <- format(as.POSIXct(WD_2018$date),"%m")
WD_2019$date <- format(as.POSIXct(WD_2019$date),"%m")
WD_2020$date <- format(as.POSIXct(WD_2020$date),"%m")

jpeg(filename='fig/WD_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(WD_2016$date, WD_2016$WD, type = 'l', xlab = '',ylim = c(140, 260), ylab = '', col = "Red", xaxt = "n")
points(WD_2016$date,WD_2016$WD, pch = 19, col = "Red")
points(WD_2017$date,WD_2017$WD, pch = 19, col = "Orange")
lines(WD_2017$date,WD_2017$WD, pch = 19, col = "Orange")
points(WD_2018$date,WD_2018$WD, pch = 19, col = "Blue")
lines(WD_2018$date,WD_2018$WD, pch = 19, col = "Blue")
points(WD_2019$date,WD_2019$WD, pch = 19, col = "Purple")
lines(WD_2019$date,WD_2019$WD, pch = 19, col = "Purple")
points(WD_2020$date,WD_2020$WD, pch = 19, col = "Black")
lines(WD_2020$date,WD_2020$WD, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('Wind Direction (','degree '^'o','North', ')', sep = "")), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c( "Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()

jpeg(filename='fig/WD_all_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par (mar = c(4,4,1,1)
     #,family = "Times New Roman" 
)
plot(df_merge_month$date,df_merge_month$WD, type = 'l', xlab = '', ylab = '', ylim = c(140,240))
points(df_merge_month$date,df_merge_month$WD, pch = 19)
#mtext('Year', side = 1, line = 2)
mtext(expression(paste('Wind Direction (','degree '^'o','North', ')', sep = "")), side = 2, line = 2)
#abline(v= NEM_date, lty = 2,col = "red" )
#abline(v= SWM_date, lty = 2, col = "blue")
#abline(v= FTM_date, lty = 2, col = "blue")
#abline(v = STM_date, lty = 2, col = "red")
#legend("bottomright",legend = c("NEM","SWM"), col = c("red","blue"), lty = 2)
dev.off()

####WS####
WS_2016 <- selectByDate(df_merge_month,year = 2016)[,c(1,3)]
WS_2017 <- selectByDate(df_merge_month,year = 2017)[,c(1,3)]
WS_2018 <- selectByDate(df_merge_month,year = 2018)[,c(1,3)]
WS_2019 <- selectByDate(df_merge_month,year = 2019)[,c(1,3)]
WS_2020 <- selectByDate(df_merge_month,year = 2020)[,c(1,3)]
WS_2016$date <- format(as.POSIXct(WS_2016$date),"%m")
WS_2017$date <- format(as.POSIXct(WS_2017$date),"%m")
WS_2018$date <- format(as.POSIXct(WS_2018$date),"%m")
WS_2019$date <- format(as.POSIXct(WS_2019$date),"%m")
WS_2020$date <- format(as.POSIXct(WS_2020$date),"%m")

jpeg(filename='fig/WS_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(WS_2016$date, WS_2016$WS, type = 'l', xlab = '', ylab = '', col = "Red", xaxt = "n", ylim = c(0.2,1.2))
points(WS_2016$date,WS_2016$WS, pch = 19, col = "Red")
points(WS_2017$date,WS_2017$WS, pch = 19, col = "Orange")
lines(WS_2017$date,WS_2017$WS, pch = 19, col = "Orange")
points(WS_2018$date,WS_2018$WS, pch = 19, col = "Blue")
lines(WS_2018$date,WS_2018$WS, pch = 19, col = "Blue")
points(WS_2019$date,WS_2019$WS, pch = 19, col = "Purple")
lines(WS_2019$date,WS_2019$WS, pch = 19, col = "Purple")
points(WS_2020$date,WS_2020$WS, pch = 19, col = "Black")
lines(WS_2020$date,WS_2020$WS, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('Wind Speed (', 'ms'^'-1', ')', sep = "")), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c("Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, cex = 0.85, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()

jpeg(filename='fig/WS_all_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par (mar = c(4,4,1,1)
     #,family = "Times New Roman" 
)
plot(df_merge_month$date,df_merge_month$WS, type = 'l',ylim = c(0.2,1.0), xlab = '', ylab = '')
points(df_merge_month$date,df_merge_month$WS, pch = 19)
#mtext('Year', side = 1, line = 2)
mtext(expression(paste('Wind Speed (', 'ms'^'-1', ')', sep = "")), side = 2, line = 2)
#abline(v= NEM_date, lty = 2,col = "red" )
#abline(v= SWM_date, lty = 2, col = "blue")
#abline(v= FTM_date, lty = 2, col = "blue")
#abline(v = STM_date, lty = 2, col = "red")
#legend("bottomright",legend = c("NEM","SWM"), col = c("red","blue"), lty = 2)
dev.off()

####TA####
TA_2016 <- selectByDate(df_merge_month,year = 2016)[,c(1,15)]
TA_2017 <- selectByDate(df_merge_month,year = 2017)[,c(1,15)]
TA_2018 <- selectByDate(df_merge_month,year = 2018)[,c(1,15)]
TA_2019 <- selectByDate(df_merge_month,year = 2019)[,c(1,15)]
TA_2020 <- selectByDate(df_merge_month,year = 2020)[,c(1,15)]
TA_2016$date <- format(as.POSIXct(TA_2016$date),"%m")
TA_2017$date <- format(as.POSIXct(TA_2017$date),"%m")
TA_2018$date <- format(as.POSIXct(TA_2018$date),"%m")
TA_2019$date <- format(as.POSIXct(TA_2019$date),"%m")
TA_2020$date <- format(as.POSIXct(TA_2020$date),"%m")

jpeg(filename='fig/TA_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(TA_2016$date, TA_2016$TA, type = 'l', xlab = '', ylab = '', col = "Red", xaxt = "n", ylim = c(26,32))
points(TA_2016$date,TA_2016$TA, pch = 19, col = "Red")
points(TA_2017$date,TA_2017$TA, pch = 19, col = "Orange")
lines(TA_2017$date,TA_2017$TA, pch = 19, col = "Orange")
points(TA_2018$date,TA_2018$TA, pch = 19, col = "Blue")
lines(TA_2018$date,TA_2018$TA, pch = 19, col = "Blue")
points(TA_2019$date,TA_2019$TA, pch = 19, col = "Purple")
lines(TA_2019$date,TA_2019$TA, pch = 19, col = "Purple")
points(TA_2020$date,TA_2020$TA, pch = 19, col = "Black")
lines(TA_2020$date,TA_2020$TA, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('T'['a'],' (','C'^{'o'},')', sep = "")), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c("Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, cex = 0.85, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()

jpeg(filename='fig/TA_all_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par (mar = c(4,4,1,1)
     #,family = "Times New Roman" 
)
plot(df_merge_month$date,df_merge_month$TA, type = 'l', xlab = '', ylab = '', ylim = c(26,31))
points(df_merge_month$date,df_merge_month$TA, pch = 19)
#mtext('Year', side = 1, line = 2)
mtext(expression(paste('T'['a'],' (','C'^{'o'},')', sep = "")), side = 2, line = 2)
#abline(v= NEM_date, lty = 2,col = "red" )
#abline(v= SWM_date, lty = 2, col = "blue")
#abline(v= FTM_date, lty = 2, col = "blue")
#abline(v = STM_date, lty = 2, col = "red")
#legend("bottomright",legend = c("NEM","SWM"), col = c("red","blue"), lty = 2)
dev.off()

####TS####
TS_2016 <- selectByDate(df_merge_month,year = 2016)[,c(1,20)]
TS_2017 <- selectByDate(df_merge_month,year = 2017)[,c(1,20)]
TS_2018 <- selectByDate(df_merge_month,year = 2018)[,c(1,20)]
TS_2019 <- selectByDate(df_merge_month,year = 2019)[,c(1,20)]
TS_2020 <- selectByDate(df_merge_month,year = 2020)[,c(1,20)]
TS_2016$date <- format(as.POSIXct(TS_2016$date),"%m")
TS_2017$date <- format(as.POSIXct(TS_2017$date),"%m")
TS_2018$date <- format(as.POSIXct(TS_2018$date),"%m")
TS_2019$date <- format(as.POSIXct(TS_2019$date),"%m")
TS_2020$date <- format(as.POSIXct(TS_2020$date),"%m")

jpeg(filename='fig/TS_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(TS_2016$date, TS_2016$TS, type = 'l', xlab = '', ylab = '', col = "Red", xaxt = "n", ylim = c(27,35))
points(TS_2016$date,TS_2016$TS, pch = 19, col = "Red")
points(TS_2017$date,TS_2017$TS, pch = 19, col = "Orange")
lines(TS_2017$date,TS_2017$TS, pch = 19, col = "Orange")
points(TS_2018$date,TS_2018$TS, pch = 19, col = "Blue")
lines(TS_2018$date,TS_2018$TS, pch = 19, col = "Blue")
points(TS_2019$date,TS_2019$TS, pch = 19, col = "Purple")
lines(TS_2019$date,TS_2019$TS, pch = 19, col = "Purple")
points(TS_2020$date,TS_2020$TS, pch = 19, col = "Black")
lines(TS_2020$date,TS_2020$TS, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('T'['s'],' (','C'^{'o'},')', sep = "")), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c( "Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, cex = 0.85, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()

jpeg(filename='fig/TS_all_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par (mar = c(4,4,1,1)
     #,family = "Times New Roman" 
)
plot(df_merge_month$date,df_merge_month$TS, type = 'l', xlab = '', ylab = '', ylim = c(27,33))
points(df_merge_month$date,df_merge_month$TS, pch = 19)
#mtext('Year', side = 1, line = 2)
mtext(expression(paste('T'['s'],' (','C'^{'o'},')', sep = "")), side = 2, line = 2)
#abline(v= NEM_date, lty = 2,col = "red" )
#abline(v= SWM_date, lty = 2, col = "blue")
#abline(v= FTM_date, lty = 2, col = "blue")
#abline(v = STM_date, lty = 2, col = "red")
#legend("bottomright",legend = c("NEM","SWM"), col = c("red","blue"), lty = 2)
dev.off()

####SST####
SST_2016 <- selectByDate(df_merge_month,year = 2016)[,c(1,27)]
SST_2017 <- selectByDate(df_merge_month,year = 2017)[,c(1,27)]
SST_2018 <- selectByDate(df_merge_month,year = 2018)[,c(1,27)]
SST_2019 <- selectByDate(df_merge_month,year = 2019)[,c(1,27)]
SST_2020 <- selectByDate(df_merge_month,year = 2020)[,c(1,27)]
SST_2016$date <- format(as.POSIXct(SST_2016$date),"%m")
SST_2017$date <- format(as.POSIXct(SST_2017$date),"%m")
SST_2018$date <- format(as.POSIXct(SST_2018$date),"%m")
SST_2019$date <- format(as.POSIXct(SST_2019$date),"%m")
SST_2020$date <- format(as.POSIXct(SST_2020$date),"%m")

jpeg(filename='fig/SST_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(SST_2016$date, SST_2016$SST, type = 'l', xlab = '', ylab = '', col = "Red", xaxt = "n", ylim = c(28,34))
points(SST_2016$date,SST_2016$SST, pch = 19, col = "Red")
points(SST_2017$date,SST_2017$SST, pch = 19, col = "Orange")
lines(SST_2017$date,SST_2017$SST, pch = 19, col = "Orange")
points(SST_2018$date,SST_2018$SST, pch = 19, col = "Blue")
lines(SST_2018$date,SST_2018$SST, pch = 19, col = "Blue")
points(SST_2019$date,SST_2019$SST, pch = 19, col = "Purple")
lines(SST_2019$date,SST_2019$SST, pch = 19, col = "Purple")
points(SST_2020$date,SST_2020$SST, pch = 19, col = "Black")
lines(SST_2020$date,SST_2020$SST, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('SST (','C'^'o', ')', sep = "")), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c( "Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, cex = 0.85, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()

jpeg(filename='fig/SST_all_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par (mar = c(4,4,1,1)
     #,family = "Times New Roman" 
)
plot(df_merge_month$date,df_merge_month$SST, type = 'l', xlab = '', ylab = '', ylim = c(28,33))
points(df_merge_month$date,df_merge_month$SST, pch = 19)
#mtext('Year', side = 1, line = 2)
mtext(expression(paste('SST (','C'^'o', ')', sep = "")), side = 2, line = 2)
#abline(v= NEM_date, lty = 2,col = "red" )
#abline(v= SWM_date, lty = 2, col = "blue")
#abline(v= FTM_date, lty = 2, col = "blue")
#abline(v = STM_date, lty = 2, col = "red")
#legend("bottomright",legend = c("NEM","SWM"), col = c("red","blue"), lty = 2)
dev.off()

####PAR####
PAR_2016 <- selectByDate(df_merge_month,year = 2016)[,c(1,30)]
PAR_2017 <- selectByDate(df_merge_month,year = 2017)[,c(1,30)]
PAR_2018 <- selectByDate(df_merge_month,year = 2018)[,c(1,30)]
PAR_2019 <- selectByDate(df_merge_month,year = 2019)[,c(1,30)]
PAR_2020 <- selectByDate(df_merge_month,year = 2020)[,c(1,30)]
PAR_2016$date <- format(as.POSIXct(PAR_2016$date),"%m")
PAR_2017$date <- format(as.POSIXct(PAR_2017$date),"%m")
PAR_2018$date <- format(as.POSIXct(PAR_2018$date),"%m")
PAR_2019$date <- format(as.POSIXct(PAR_2019$date),"%m")
PAR_2020$date <- format(as.POSIXct(PAR_2020$date),"%m")

jpeg(filename='fig/PAR_years2.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(PAR_2016$date, PAR_2016$PAR, type = 'l', xlab = '', ylab = '', ylim = c(35,65),col = "Red", xaxt = "n")
points(PAR_2016$date,PAR_2016$PAR, pch = 19, col = "Red")
points(PAR_2017$date,PAR_2017$PAR, pch = 19, col = "Orange")
lines(PAR_2017$date,PAR_2017$PAR, pch = 19, col = "Orange")
points(PAR_2018$date,PAR_2018$PAR, pch = 19, col = "Blue")
lines(PAR_2018$date,PAR_2018$PAR, pch = 19, col = "Blue")
points(PAR_2019$date,PAR_2019$PAR, pch = 19, col = "Purple")
lines(PAR_2019$date,PAR_2019$PAR, pch = 19, col = "Purple")
points(PAR_2020$date,PAR_2020$PAR, pch = 19, col = "Black")
lines(PAR_2020$date,PAR_2020$PAR, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('PAR Radiation')), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c( "Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()

jpeg(filename='fig/PAR_all_years2.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par (mar = c(4,4,1,1)
     #,family = "Times New Roman" 
)
plot(df_merge_month$date,df_merge_month$PAR, type = 'l', xlab = '', ylab = '')
points(df_merge_month$date,df_merge_month$PAR, pch = 19)
#mtext('Year', side = 1, line = 2)
mtext(expression(paste('PAR Radiation', sep = "")), side = 2, line = 2)
#abline(v= NEM_date, lty = 2,col = "red" )
#abline(v= SWM_date, lty = 2, col = "blue")
#abline(v= FTM_date, lty = 2, col = "blue")
#abline(v = STM_date, lty = 2, col = "red")
#legend("bottomright",legend = c("NEM","SWM"), col = c("red","blue"), lty = 2)
dev.off()

####PPFD####
PPFD_2016 <- selectByDate(df_merge_month,year = 2016)[,c(1,17)]
PPFD_2017 <- selectByDate(df_merge_month,year = 2017)[,c(1,17)]
PPFD_2018 <- selectByDate(df_merge_month,year = 2018)[,c(1,17)]
PPFD_2019 <- selectByDate(df_merge_month,year = 2019)[,c(1,17)]
PPFD_2020 <- selectByDate(df_merge_month,year = 2020)[,c(1,17)]
PPFD_2016$date <- format(as.POSIXct(PPFD_2016$date),"%m")
PPFD_2017$date <- format(as.POSIXct(PPFD_2017$date),"%m")
PPFD_2018$date <- format(as.POSIXct(PPFD_2018$date),"%m")
PPFD_2019$date <- format(as.POSIXct(PPFD_2019$date),"%m")
PPFD_2020$date <- format(as.POSIXct(PPFD_2020$date),"%m")

jpeg(filename='fig/PPFD_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(PPFD_2016$date, PPFD_2019$PPFD, type = 'l', xlab = '',ylim = c(250,520), ylab = '',col = "Red", xaxt = "n")
points(PPFD_2019$date,PPFD_2019$PPFD, pch = 19, col = "Purple")
points(PPFD_2020$date,PPFD_2020$PPFD, pch = 19, col = "Black")
lines(PPFD_2020$date,PPFD_2020$PPFD, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('PPFD')), side = 2, line = 2)
legend_order <- c(2019:2020)
legend_colour<-c( "Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()

jpeg(filename='fig/PPFD_all_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par (mar = c(4,4,1,1)
     #,family = "Times New Roman" 
)
plot(df_merge_month$date,df_merge_month$PPFD, type = 'l', xlab = '', ylab = '', xlim = c(as.POSIXct("2019-01-01 00:00:00"),as.POSIXct("2020-12-31 00:00:00")))
points(df_merge_month$date,df_merge_month$PPFD, pch = 19)
#mtext('Year', side = 1, line = 2)
mtext(expression(paste('PPFD', sep = "")), side = 2, line = 2)
#abline(v= NEM_date, lty = 2,col = "red" )
#abline(v= SWM_date, lty = 2, col = "blue")
#abline(v= FTM_date, lty = 2, col = "blue")
#abline(v = STM_date, lty = 2, col = "red")
#legend("bottomright",legend = c("NEM","SWM"), col = c("red","blue"), lty = 2)
dev.off()


####USTAR####
USTAR_2016 <- selectByDate(df_merge_month,year = 2016)[,c(1,18)]
USTAR_2017 <- selectByDate(df_merge_month,year = 2017)[,c(1,18)]
USTAR_2018 <- selectByDate(df_merge_month,year = 2018)[,c(1,18)]
USTAR_2019 <- selectByDate(df_merge_month,year = 2019)[,c(1,18)]
USTAR_2020 <- selectByDate(df_merge_month,year = 2020)[,c(1,18)]
USTAR_2016$date <- format(as.POSIXct(USTAR_2016$date),"%m")
USTAR_2017$date <- format(as.POSIXct(USTAR_2017$date),"%m")
USTAR_2018$date <- format(as.POSIXct(USTAR_2018$date),"%m")
USTAR_2019$date <- format(as.POSIXct(USTAR_2019$date),"%m")
USTAR_2020$date <- format(as.POSIXct(USTAR_2020$date),"%m")

jpeg(filename='fig/USTAR_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(USTAR_2016$date, USTAR_2016$USTAR, type = 'l', xlab = '', ylab = '', ylim =c(0.033,0.055) ,col = "Red", xaxt = "n")
points(USTAR_2016$date,USTAR_2016$USTAR, pch = 19, col = "Red")
points(USTAR_2017$date,USTAR_2017$USTAR, pch = 19, col = "Orange")
lines(USTAR_2017$date,USTAR_2017$USTAR, pch = 19, col = "Orange")
points(USTAR_2018$date,USTAR_2018$USTAR, pch = 19, col = "Blue")
lines(USTAR_2018$date,USTAR_2018$USTAR, pch = 19, col = "Blue")
points(USTAR_2019$date,USTAR_2019$USTAR, pch = 19, col = "Purple")
lines(USTAR_2019$date,USTAR_2019$USTAR, pch = 19, col = "Purple")
points(USTAR_2020$date,USTAR_2020$USTAR, pch = 19, col = "Black")
lines(USTAR_2020$date,USTAR_2020$USTAR, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('USTAR')), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c( "Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()

jpeg(filename='fig/USTAR_all_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par (mar = c(4,4,1,1)
     #,family = "Times New Roman" 
)
plot(df_merge_month$date,df_merge_month$USTAR, type = 'l', xlab = '', ylab = '')
points(df_merge_month$date,df_merge_month$USTAR, pch = 19)
#mtext('Year', side = 1, line = 2)
mtext(expression(paste('USTAR', sep = "")), side = 2, line = 2)
#abline(v= NEM_date, lty = 2,col = "red" )
#abline(v= SWM_date, lty = 2, col = "blue")
#abline(v= FTM_date, lty = 2, col = "blue")
#abline(v = STM_date, lty = 2, col = "red")
#legend("bottomright",legend = c("NEM","SWM"), col = c("red","blue"), lty = 2)
dev.off()

####RH####
RH_2016 <- selectByDate(df_merge_month,year = 2016)[,c(1,7)]
RH_2017 <- selectByDate(df_merge_month,year = 2017)[,c(1,7)]
RH_2018 <- selectByDate(df_merge_month,year = 2018)[,c(1,7)]
RH_2019 <- selectByDate(df_merge_month,year = 2019)[,c(1,7)]
RH_2020 <- selectByDate(df_merge_month,year = 2020)[,c(1,7)]
RH_2016$date <- format(as.POSIXct(RH_2016$date),"%m")
RH_2017$date <- format(as.POSIXct(RH_2017$date),"%m")
RH_2018$date <- format(as.POSIXct(RH_2018$date),"%m")
RH_2019$date <- format(as.POSIXct(RH_2019$date),"%m")
RH_2020$date <- format(as.POSIXct(RH_2020$date),"%m")

jpeg(filename='fig/RH_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(RH_2016$date, RH_2016$RH, type = 'l', xlab = '', ylab = '', ylim =c(65,105) ,col = "Red", xaxt = "n")
points(RH_2016$date,RH_2016$RH, pch = 19, col = "Red")
points(RH_2017$date,RH_2017$RH, pch = 19, col = "Orange")
lines(RH_2017$date,RH_2017$RH, pch = 19, col = "Orange")
points(RH_2018$date,RH_2018$RH, pch = 19, col = "Blue")
lines(RH_2018$date,RH_2018$RH, pch = 19, col = "Blue")
points(RH_2019$date,RH_2019$RH, pch = 19, col = "Purple")
lines(RH_2019$date,RH_2019$RH, pch = 19, col = "Purple")
points(RH_2020$date,RH_2020$RH, pch = 19, col = "Black")
lines(RH_2020$date,RH_2020$RH, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('RH')), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c( "Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()

jpeg(filename='fig/RH_all_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par (mar = c(4,4,1,1)
     #,family = "Times New Roman" 
)
plot(df_merge_month$date,df_merge_month$RH, type = 'l', xlab = '', ylab = '')
points(df_merge_month$date,df_merge_month$RH, pch = 19)
#mtext('Year', side = 1, line = 2)
mtext(expression(paste('RH', sep = "")), side = 2, line = 2)
#abline(v= NEM_date, lty = 2,col = "red" )
#abline(v= SWM_date, lty = 2, col = "blue")
#abline(v= FTM_date, lty = 2, col = "blue")
#abline(v = STM_date, lty = 2, col = "red")
#legend("bottomright",legend = c("NEM","SWM"), col = c("red","blue"), lty = 2)
dev.off()

####ZL####
ZL_2016 <- selectByDate(df_merge_month,year = 2016)[,c(1,11)]
ZL_2017 <- selectByDate(df_merge_month,year = 2017)[,c(1,11)]
ZL_2018 <- selectByDate(df_merge_month,year = 2018)[,c(1,11)]
ZL_2019 <- selectByDate(df_merge_month,year = 2019)[,c(1,11)]
ZL_2020 <- selectByDate(df_merge_month,year = 2020)[,c(1,11)]
ZL_2016$date <- format(as.POSIXct(ZL_2016$date),"%m")
ZL_2017$date <- format(as.POSIXct(ZL_2017$date),"%m")
ZL_2018$date <- format(as.POSIXct(ZL_2018$date),"%m")
ZL_2019$date <- format(as.POSIXct(ZL_2019$date),"%m")
ZL_2020$date <- format(as.POSIXct(ZL_2020$date),"%m")

jpeg(filename='fig/ZL_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
plot(ZL_2016$date, ZL_2016$ZL, type = 'l', xlab = '', ylab = '', ylim =c(-2.5,0.0) ,col = "Red", xaxt = "n")
points(ZL_2016$date,ZL_2016$ZL, pch = 19, col = "Red")
points(ZL_2017$date,ZL_2017$ZL, pch = 19, col = "Orange")
lines(ZL_2017$date,ZL_2017$ZL, pch = 19, col = "Orange")
points(ZL_2018$date,ZL_2018$ZL, pch = 19, col = "Blue")
lines(ZL_2018$date,ZL_2018$ZL, pch = 19, col = "Blue")
points(ZL_2019$date,ZL_2019$ZL, pch = 19, col = "Purple")
lines(ZL_2019$date,ZL_2019$ZL, pch = 19, col = "Purple")
points(ZL_2020$date,ZL_2020$ZL, pch = 19, col = "Black")
lines(ZL_2020$date,ZL_2020$ZL, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('ZL')), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c( "Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()

jpeg(filename='fig/ZL_all_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par (mar = c(4,4,1,1)
     #,family = "Times New Roman" 
)
plot(df_merge_month$date,df_merge_month$ZL, type = 'l', xlab = '', ylab = '')
points(df_merge_month$date,df_merge_month$ZL, pch = 19)
#mtext('Year', side = 1, line = 2)
mtext(expression(paste('ZL', sep = "")), side = 2, line = 2)
#abline(v= NEM_date, lty = 2,col = "red" )
#abline(v= SWM_date, lty = 2, col = "blue")
#abline(v= FTM_date, lty = 2, col = "blue")
#abline(v = STM_date, lty = 2, col = "red")
#legend("bottomright",legend = c("NEM","SWM"), col = c("red","blue"), lty = 2)
dev.off()





##Resore to Default par()
dev.off()

library(dplyr)
library(ggpubr)
library(rstatix)

#Alternative plot
library(openair)
jpeg(filename='fig/FCO2_smooth.jpg', unit = 'cm', width = 20, height = 20, res = 360)
smoothTrend(df_merge_month, pollutant = "FCO2", deseason = T, simulate = T)
dev.off()
jpeg(filename='fig/FCO2_theil.jpg', unit = 'cm', width = 20, height = 20, res = 360)
TheilSen(df_merge_month,pollutant = "FCO2", xlab = "year", date.breaks = 5, date.format = "%Y")
dev.off()
jpeg(filename='fig/FCO2_WS_alt.jpg', unit = 'cm', width = 10, height = 10, res = 360)
ggplot(df_merge_month, aes(FCO2, WS)) +
  geom_point() +
  geom_smooth(formula = y~x, method = "lm") + 
  stat_regline_equation(data = df_merge_month, label.x = 0.0, label.y = 1.1)
dev.off()


#Descriptive Statistic Mean and SD table overall
mean_df <- sapply(na.omit(df_merge_month[c(5,26,4,3,15,20,27,30,17,18,7,11)]), mean)
sd_df <- sapply(na.omit(df_merge_month[c(5,26,4,3,15,20,27,30,17,18,7,11)]), sd)
df_statistic <- data.frame(rbind(mean_df,sd_df))
row.names(df_statistic) <- c("Mean","SD")
write.table(x = df_statistic,file = "descriptive_statistic.csv", sep = ",", row.names = TRUE)

####mean and SD in years #####
library(lubridate)
library(openair)
#df_merge_year$date <- year(df_merge_year$date)
year_df <- df_merge_year[c(1,5,26,4,3,15,20,27,30,17,18,7,11)]
date_2016 <- selectByDate(df, year = 2016)
date_2017 <- selectByDate(df, year = 2017)
date_2018 <- selectByDate(df, year = 2018)
date_2019 <- selectByDate(df, year = 2019)
date_2020 <- selectByDate(df, year = 2020)
sd_df_1 <- rbind(sapply(sapply(date_2016[c(3,4,5,7,11,15,17,18,20)],na.omit), sd),
                 sapply(sapply(date_2017[c(3,4,5,7,11,15,17,18,20)],na.omit), sd),
                 sapply(sapply(date_2018[c(3,4,5,7,11,15,17,18,20)],na.omit), sd),
                 sapply(sapply(date_2019[c(3,4,5,7,11,15,17,18,20)],na.omit), sd),
                 sapply(sapply(date_2020[c(3,4,5,7,11,15,17,18,20)],na.omit), sd))
date_2016 <- selectByDate(df_sat, year = 2016)
date_2017 <- selectByDate(df_sat, year = 2017)
date_2018 <- selectByDate(df_sat, year = 2018)
date_2019 <- selectByDate(df_sat, year = 2019)
date_2020 <- selectByDate(df_sat, year = 2020)
sd_df_2 <- rbind(sapply(sapply(date_2016[c(4,5,8)],na.omit), sd),
                 sapply(sapply(date_2017[c(4,5,8)],na.omit), sd),
                 sapply(sapply(date_2018[c(4,5,8)],na.omit), sd),
                 sapply(sapply(date_2019[c(4,5,8)],na.omit), sd),
                 sapply(sapply(date_2020[c(4,5,8)],na.omit), sd))
sd_df <- cbind(year_df$date,sd_df_1,sd_df_2)
colnames(sd_df)[1] <- "date"
library(plyr)
df_statistic_year <- rbind.fill(year_df,as.data.frame(sd_df))
row.names(df_statistic_year) <- c("Mean 2016", "Mean 2017", "Mean 2018", "Mean 2019", "Mean 2020",
                                  "SD 2016", "SD 2017", "SD 2018", "SD 2019", "SD 2020")

df_statistic_year <- df_statistic_year[order(df_statistic_year$date),]
df_statistic_year <-round(df_statistic_year[,-1], 2)
write.table(x = df_statistic_year,file = "statistic_year_1.csv", sep = ",", row.names = TRUE)


#Pearson correlation coefficient
cor_Pearson <- cor(df_merge_month[c(5,26,4,3,15,20,27,30,17,18,7,11)], method = "pearson", use = "complete.obs")
cor_Pearson[upper.tri(cor_Pearson, diag = FALSE)]<- 0
write.table(x = cor_Pearson,file = "cor_Pearson.csv", sep = ",", row.names = TRUE)

#Correlation Trest -Pvalue statical Analysis
pvalue_test<- data.frame(c(
cor.test(df_merge_month$FCO2,df_merge_month$CHL)[3] ,
cor.test(df_merge_month$FCO2, df_merge_month$SST)[3] ,
cor.test(df_merge_month$FCO2, df_merge_month$TA)[3] ,
cor.test(df_merge_month$FCO2, df_merge_month$TS)[3] ,
cor.test(df_merge_month$FCO2, df_merge_month$PAR)[3] ,
cor.test(df_merge_month$FCO2, df_merge_month$USTAR)[3] ,
cor.test(df_merge_month$FCO2, df_merge_month$RH)[3] ,
cor.test(df_merge_month$FCO2, df_merge_month$PPFD)[3] ,
cor.test(df_merge_month$FCO2, df_merge_month$ZL)[3]
))
colnames(pvalue_test) <- c("CHL","SST","TA","TS","PAR","USTAR","RH","PPFD","ZL")
row.names(pvalue_test) <- "FCO2"
write.table(x = pvalue_test,file = "pvalue.csv", sep = ",", row.names = FALSE)

#install.packages("Hmisc")
library("Hmisc")
pvalue_test <- rcorr(as.matrix(df_merge_month[c(5,26,4,3,15,20,27,30,17,18,7,11)]), type = "pearson")$P
pvalue_test[upper.tri(pvalue_test)]<- 0
write.table(x = pvalue_test,file = "pvalue_Pearson.csv", sep = ",", row.names = TRUE)

#Linear Correlation
jpeg(filename='fig/FCO2_CHL_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$CHL, df_merge_month$FCO2,pch = 19,cex= 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$CHL,data = df_merge_month), col = "red")
mtext(side = 1, text = expression(paste('Chlorophyll', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')', sep = )),line = 2)
dev.off()

jpeg(filename='fig/FCO2_WD_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$WD, df_merge_month$FCO2, pch = 19, cex = 0.5,xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$WD,data = df_merge_month), col = "red")
mtext(side = 1,text = expression(paste('Wind Direction (','degree '^'o','North', ')', sep = "")), line = 2)
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()

jpeg(filename='fig/FCO2_WS_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$WS, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$WS,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('Wind Speed (', 'ms'^'-1', ')', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()

jpeg(filename='fig/FCO2_TA_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$TA, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$TS,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('T'['a'],' (','C'^{'o'},')', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()

jpeg(filename='fig/FCO2_TS_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$TS, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$TS,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('T'['s'],' (','C'^{'o'},')', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()


jpeg(filename='fig/FCO2_SST_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$SST, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$SST,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('SST (','C'^'o', ')', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()

jpeg(filename='fig/FCO2_PAR_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$PAR, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$PAR,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('PAR', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()

jpeg(filename='fig/FCO2_PPFD_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$PPFD, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$PPFD,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('PPFD', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()

jpeg(filename='fig/FCO2_USTAR_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$USTAR, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$RH,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('USTAR', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()


jpeg(filename='fig/FCO2_RH_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$RH, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$RH,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('RH', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()

jpeg(filename='fig/FCO2_ZL_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$ZL, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$ZL,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('ZL', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()



#Restore par() to default
dev.off()
library(dplyr)
library('openair')

#Create Month Variable
df_merge_month$month <- months(df_merge_month$date)
#Create Year variable
df_merge_month$year <- format(df_merge_month$date,"%Y")
#inter-annual
jpeg(filename='fig/CO2_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
boxplot(FCO2 ~ year, data = df_merge_month, xlab='',ylab='')
mtext(expression(paste('CO'['2'],' flux (',mu,'mol ',' m'^{'-2'}, ' s'^{'-1'},')')), 
      side=2, line = 2)
mtext(text = "Year",side = 1, line = 2)
dev.off()

jpeg(filename='fig/CHL_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
boxplot(CHL ~ year, data = df_merge_month, xlab='Month',ylab='')
mtext(expression(paste('Chlorophyll')), 
      side=2, line = 2)
mtext(text = "Year",side = 1, line = 2)
#title("Chlorophyll", line=1)
dev.off()

jpeg(filename='fig/WD_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
boxplot(WD ~ year, data = df_merge_month, xlab='Month',ylab='')
mtext(expression(paste('Wind Direction (','degree '^'o','North', ')', sep = "")), 
      side=2, line = 2)
mtext(text = "Year",side = 1, line = 2)
#title("Wind Direction", line=1)
dev.off()

jpeg(filename='fig/WS_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
boxplot(WS ~ year, data = df_merge_month, xlab='Month',ylab='')
mtext(expression(paste('Wind Speed (', 'ms'^'-1', ')', sep = "")), 
      side=2, line = 2)
mtext(text = "Year",side = 1, line = 2)
#title("Wind Speed", line=1)
dev.off()

jpeg(filename='fig/TA_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
#jpeg(filename='fig/Ta_year_FYP.jpg', unit = 'cm', width = 12, height = 10, res = 360)
boxplot(TA ~ year, data = df_merge_month, xlab='Month',ylab='')
mtext(expression(paste('T'['a'],' ('^{'o'}, 'C',')')), 
      side=2, line = 2)
mtext(text = "Year",side = 1, line = 2)
#title("Ta", line=1)
dev.off()

jpeg(filename='fig/TS_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
boxplot(TS ~ year, data = df_merge_month, xlab='Month',ylab='')
mtext(expression(paste('T'['s'],' ('^{'o'}, 'C',')')), 
      side=2, line = 2)
mtext(text = "Year",side = 1, line = 2)
#title("Ts", line=1)
dev.off()

jpeg(filename='fig/SST_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
boxplot(SST ~ year, data = df_merge_month, xlab='Month',ylab='')
mtext(expression(paste('SST ('^{'o'}, 'C',')')), 
      side=2, line = 2.5)
mtext(text = "Year",side = 1, line = 2)
#title("Sea Surface Temperature", line=1)
dev.off()

jpeg(filename='fig/PAR_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
boxplot(PAR ~ year, data = df_merge_month, xlab='Month',ylab='')
mtext(expression(paste('PAR')), 
      side=2, line = 2.5)
mtext(text = "Year",side = 1, line = 2)
#title("PAR RADIATION", line=1)
dev.off()

jpeg(filename='fig/PPFD_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
boxplot(PPFD ~ year, data = df_merge_month, xlab='Month',ylab='')
mtext(expression(paste('PPFD')), 
      side=2, line = 2.5)
mtext(text = "Year",side = 1, line = 2)
#title("PPFD", line=1)
dev.off()

jpeg(filename='fig/USTAR_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
boxplot(PAR ~ year, data = df_merge_month, xlab='Month',ylab='')
mtext(expression(paste('USTAR')), 
      side=2, line = 2.5)
mtext(text = "Year",side = 1, line = 2)
#title("USTAR", line=1)
dev.off()

jpeg(filename='fig/RH_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
boxplot(RH ~ year, data = df_merge_month, xlab='Month',ylab='')
mtext(expression(paste('RH')), 
      side=2, line = 2.5)
mtext(text = "Year",side = 1, line = 2)
#title("Relative Humidity", line=1)
dev.off()

jpeg(filename='fig/ZL_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
boxplot(ZL ~ year, data = df_merge_month, xlab='Month',ylab='')
mtext(expression(paste('ZL')), 
      side=2, line = 2.5)
mtext(text = "Year",side = 1, line = 2)
#title("ZL", line=1)
dev.off()

####SMOOTHWITH GGPLOT####
jpeg(filename='fig/CO2_year_smooth.jpg', unit = 'cm', width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
library(scales)
library(ggplot2)
ggplot(df_merge_month, aes(date, FCO2)) +
  geom_line() + 
  xlab('Year') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('CO'['2'],' flux (',mu,'mol ',' m'^{'-2'}, ' s'^{'-1'},')')))
#geom_text(data = df_merge_month, aes(date, CO2),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/CHL_year_smooth.jpg', unit = 'cm', width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
library(scales)
library(ggplot2)
ggplot(df_merge_month, aes(date, CHL)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab("Chlorophyll")
#geom_text(data = df_merge_month, aes(date, CHL),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/WD_year_smooth.jpg', unit = 'cm', width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
library(scales)
library(ggplot2)
ggplot(df_merge_month, aes(date, WD)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('Wind Direction (','degree '^'o','North', ')', sep = "")))
#geom_text(data = df_merge_month, aes(date, WD),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/WS_year_smooth.jpg', unit = 'cm', width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
library(scales)
library(ggplot2)
ggplot(df_merge_month, aes(date, WS)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('Wind Speed (', 'ms'^'-1', ')', sep = "")))
#geom_text(data = df_merge_month, aes(date, WS),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off ()

jpeg(filename='fig/TA_year_smooth.jpg', unit = 'cm', width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
library(scales)
library(ggplot2)
ggplot(df_merge_month, aes(date, TA)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('T'['a'],' (','C'^{'o'},')', sep = "")))
#geom_text(data = df_merge_month, aes(date, TA),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/TS_year_smooth.jpg', unit = 'cm', width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
library(scales)
library(ggplot2)
ggplot(df_merge_month, aes(date, TS)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('T'['s'],' (','C'^{'o'},')', sep = "")))
#geom_text(data = df_merge_month, aes(date, TS),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/SST_year_smooth.jpg', unit = 'cm', width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
library(scales)
library(ggplot2)
ggplot(df_merge_month, aes(date, SST)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('SST',' (','C'^{'o'},')', sep = "")))
#geom_text(data = df_merge_month, aes(date, SST),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/PAR_year_smooth.jpg', unit = 'cm', width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
library(scales)
library(ggplot2)
ggplot(df_merge_month, aes(date, PAR)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('PAR', sep = "")))
#geom_text(data = df_merge_month, aes(date, PAR),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/PPFD_year_smooth.jpg', unit = 'cm', width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
library(scales)
library(ggplot2)
ggplot(df_merge_month, aes(date, PPFD)) +
  xlim(c(as.POSIXct("2019-01-01 00:00:00"),as.POSIXct("2020-12-31 00:00:00"))) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('PPFD', sep = ""))) 
#geom_text(data = df_merge_month, aes(date, PPFD),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()


jpeg(filename='fig/USTAR_year_smooth.jpg', unit = 'cm', width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
library(scales)
library(ggplot2)
ggplot(df_merge_month, aes(date,USTAR)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('USTAR', sep = "")))
#geom_text(data = df_merge_month, aes(date, USTAR),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/RH_year_smooth.jpg', unit = 'cm', width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
library(scales)
library(ggplot2)
ggplot(df_merge_month, aes(date, RH)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('RH', sep = "")))
#geom_text(data = df_merge_month, aes(date, RH),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/ZL_year_smooth.jpg', unit = 'cm', width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
library(scales)
library(ggplot2)
ggplot(df_merge_month, aes(date,ZL)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('ZL', sep = "")))
#geom_text(data = df_merge_month, aes(date, ZL),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()


#Break into years
jpeg(filename='fig/Timeseries_FCO2_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)    #Break into Years Time series
df_merge_month_1 <- df_merge_month
df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
ggplot((df_merge_month_1), aes(x = date, y = FCO2)) + 
  geom_line(col='pink') +  
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+
  xlab(label = "Time") +
  ylab(label = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = ""))) + 
  facet_wrap( ~ format(date, "%Y"), scales = "free") + 
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))

dev.off()

jpeg(filename='fig/Timeseries_CHL_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)    #Break into Years Time series
df_merge_month_1 <- df_merge_month
df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
ggplot((df_merge_month_1), aes(x = date, y = CHL)) + 
  geom_line(col='pink') +  
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
  xlab(label = "Time") +
  ylab(label = expression(paste("Chlorophyll"))) + 
  facet_wrap( ~ format(date, "%Y"), scales = "free") + 
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))

dev.off()

jpeg(filename='fig/Timeseries_WD_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)    #Break into Years Time series
df_merge_month_1 <- df_merge_month
df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
ggplot((df_merge_month_1), aes(x = date, y = WD)) + 
  geom_line(col='pink') +  
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
  xlab(label = "Time") +
  ylab(label = expression(paste('Wind Direction (','degree '^'o','North', ')', sep = ""))) + 
  facet_wrap( ~ format(date, "%Y"), scales = "free") + 
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))

dev.off()


jpeg(filename='fig/Timeseries_WS_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)    #Break into Years Time series
df_merge_month_1 <- df_merge_month
df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
ggplot((df_merge_month_1), aes(x = date, y = WS)) + 
  geom_line(col='pink') +  
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
  xlab(label = "Time") +
  ylab(label = expression(paste('Wind Speed (', 'ms'^'-1', ')', sep = ""))) + 
  facet_wrap( ~ format(date, "%Y"), scales = "free") + 
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))

dev.off()

jpeg(filename='fig/Timeseries_TA_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)
df_merge_month_1 <- df_merge_month
df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
ggplot((df_merge_month_1), aes(x = date, y = TA)) + 
  geom_line(col='pink') +  
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
  xlab(label = "Time") +
  ylab(label = expression(paste('Air Temperature',' ('^{'o'}, 'C',')'))) + 
  facet_wrap( ~ format(date, "%Y"), scales = "free") + 
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))

dev.off()

jpeg(filename='fig/Timeseries_TS_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)
df_merge_month_1 <- df_merge_month
df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
ggplot((df_merge_month_1), aes(x = date, y = TS)) + 
  geom_line(col='pink') +  
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
  xlab(label = "Time") +
  ylab(label = expression(paste('T'['s'],' ('^{'o'}, 'C',')'))) + 
  facet_wrap( ~ format(date, "%Y"), scales = "free") + 
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))

dev.off()

jpeg(filename='fig/Timeseries_SST_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)
df_merge_month_1 <- df_merge_month
df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
ggplot((df_merge_month_1), aes(x = date, y = SST)) + 
  geom_line(col='pink') +  
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
  xlab(label = "Time") +
  ylab(label = expression(paste('Sea Surface Temperature',' ('^{'o'}, 'C',')'))) + 
  facet_wrap( ~ format(date, "%Y"), scales = "free") + 
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))

dev.off()

jpeg(filename='fig/Timeseries_PAR_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)
df_merge_month_1 <- df_merge_month
df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
ggplot((df_merge_month_1), aes(x = date, y = PAR)) + 
  geom_line(col='pink') +  
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
  xlab(label = "Time") +
  ylab(label = expression(paste('PAR'))) + 
  facet_wrap( ~ format(date, "%Y"), scales = "free") + 
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))

dev.off()

jpeg(filename='fig/Timeseries_PPFD_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)
df_merge_month_1 <- df_merge_month
df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
ggplot((df_merge_month_1), aes(x = date, y = PPFD)) + 
  geom_line(col='pink') +  
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
  xlab(label = "Time") +
  ylab(label = expression(paste('PPFD'))) + 
  facet_wrap( ~ format(date, "%Y"), scales = "free") + 
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))

dev.off()

jpeg(filename='fig/Timeseries_USTAR_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)
df_merge_month_1 <- df_merge_month
df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
ggplot((df_merge_month_1), aes(x = date, y = USTAR)) + 
  geom_line(col='pink') +  
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
  xlab(label = "Time") +
  ylab(label = expression(paste('USTAR'))) + 
  facet_wrap( ~ format(date, "%Y"), scales = "free") + 
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))

dev.off()

jpeg(filename='fig/Timeseries_RH_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)
df_merge_month_1 <- df_merge_month
df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
ggplot((df_merge_month_1), aes(x = date, y = RH)) + 
  geom_line(col='pink') +  
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
  xlab(label = "Time") +
  ylab(label = expression(paste('Relative Humidity'))) + 
  facet_wrap( ~ format(date, "%Y"), scales = "free") + 
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))

dev.off()

jpeg(filename='fig/Timeseries_ZL_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)
df_merge_month_1 <- df_merge_month
df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
ggplot((df_merge_month_1), aes(x = date, y = ZL)) + 
  geom_line(col='pink') +  
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
  xlab(label = "Time") +
  ylab(label = expression(paste('ZL'))) + 
  facet_wrap( ~ format(date, "%Y"), scales = "free") + 
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))

dev.off()


x <- df_merge_month$RN -df_merge_month$SH - df_merge_month$SLE
y <- df_merge_month$LE + df_merge_month$H

y[which (y < -300)] <- NA
x[which (x < - 50)] <- NA

#jpeg(filename='fig/EBC_unstable.jpg', width = 10, height = 10, res = 500, units='cm') #This one not sure need check
par(mar=c(4,4,0.8,0.8))
plot(x,y, ylab = "LE + H", xlab = "RN - SLE - SH") 
#legend("topleft",lty=c(1),col='white',bg="white",lwd=2,bty='n')
#dev.off()

EB_Ratio <- sum(y, na.rm=TRUE)/sum(x,na.rm=TRUE)

#####################################
plot(NEM$date,NEM$FCO2, type='l',col='blue', ylim = c(-0.12,0.1))    #LINE PLOT COMPARE MONSOON
lines(SWM$date,SWM$FCO2,type='l')
lines(STM$date,STM$FCO2, type='l', col='red')
lines(FTM$date,FTM$FCO2, type='l', col='purple')

plot(NEM$date[-1],NEM$RN[-1], type='l',col='blue', ylim = c(0,80)) #first data got problem need to check for RN
lines(SWM$date,SWM$RN,type='l')
lines(STM$date,STM$RN, type='l', col='red')
lines(FTM$date,FTM$RN, type='l', col='purple')

##################barplot###################
jpeg(filename='fig/histogram_CO2_monthly.jpg', width = 12, height = 8, res = 500, units='cm')       #BARPLOT
par(mar=c(7,3.8,0.8,0.8))
barplot(df_merge_month$FCO2,ylab="",xlab=, col='pink')
mtext(side=2,expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")),line=2)
mtext(side = 1,"Month",line = 5)
axis(side=1, at=seq.int(from = 0.7, to = 72.7,by = 1.2),
     labels=format(seq.Date(from = as.Date("2015-11-01"), to = as.Date("2020-11-01"),by = "1 month"), "%B %Y"), las =2, cex.axis =0.7)
box()
dev.off()


jpeg(filename='fig/histogram_RN_monthly.jpg', width = 12, height = 8, res = 500, units='cm')
par(mar=c(7,3.8,0.8,0.8))
barplot(df_merge_month$RN, ylim = c(0,150),ylab="",xlab="", col='pink',
        yaxt='n',yaxt='n')
mtext(side=2,expression(paste('Net Radiation (','W m'^'-2', ')')),line=2)
mtext(side = 1,"Month", line = 5)
axis(side=1, at=seq.int(from = 0.7, to = 72.7,by = 1.2),
     labels=format(seq.Date(from = as.Date("2015-11-01"), to = as.Date("2020-11-01"),by = "1 month"), "%B %Y"), las =2, cex.axis =0.7)
axis(side=2,at=c(0,50,100,150),labels = c(0,50,100,150))
box()
dev.off()

jpeg(filename='fig/histogram_LE_monthly_1.jpg', width = 16, height = 8, res = 500, units='cm') 
par(mar=c(7.0,3.8,2.0,2.0))
barplot(df_merge_month$LE,ylab="",xlab="", col='green',ylim=c(0,12))
mtext(side=2,expression(paste('Latent Heat (','W m'^'-2', ')')),line=2)
mtext(side = 1,"Month", line = 5)
axis(side=1, at=seq.int(from = 0.7, to = 72.7,by = 1.2),
     labels=format(seq.Date(from = as.Date("2015-11-01"), to = as.Date("2020-11-01"),by = "1 month"), "%B %Y"), las =2, cex.axis =0.7)
#axis(side=2,at=c(0,1,2,3,4,5,6,7,8,9,10,11,12),labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12))  #i think need to ask Dr this one 
box()
dev.off()



#################PLOT SST &TS COMPAR#######################


library(lattice)
library(latticeExtra)
jpeg(filename='fig/TS&SSTWITHFCO2.jpg', width = 10, height = 10, res = 500, units='cm')
plot_1 <- xyplot(df_merge_month$TS+df_merge_month$SST~df_merge_month$date, 
                 type = "l", lwd =2, col = c("blue","red") , ylim = c(27,40), ylab = "Sea Surface Temperature", xlab = "Time")
plot_2 <- xyplot(df_merge_month$FCO2~df_merge_month$date, 
                 type = "l", lwd =2, col = "purple", ylim = c(-0.2,0.2), 
                 ylab = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), xlab = "Time")
doubleYScale(plot_1, plot_2, add.ylab2 = TRUE, use.style = FALSE)

dev.off()








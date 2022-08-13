#### 1. Preliminaries #########################################
require(openair)
require(dplyr)
source('R/tools/tool_convert_magic.R')
source('R/tools/tool_charactersNumeric.R')
source('R/tools/trapezium_intg_2.R')
source('R/tools/trapezium_intg_3.R')


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

### import EC data ######
df_ec<-read.csv('data/MCO-MUKA21_full_output.csv')
df_biomet<- read.csv('data/biomet data 21.csv')

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

####Convert Fco2 from micro mole per second to m mole per d####
df$FCO2<- df$FCO2 * 86.4

# Remove all improbable values of T
df$TA[which(df$TA < 0 | df$TA > 100 )] <- NA
df$TS[which(df$TS < 0 )] <- NA

#### random filter####
df$TA[df$TA < 20 | df$TA > 40] <- NA
df$TS[df$TS < 20 | df$TS > 40] <- NA
#df$SST[df$SST < 10 | df$SST > 50] <- NA

df$RH[df$RH < 25 | df$RH > 100] <- NA

df$RN[df$RN < -100] <- NA

df$RN[df$RN > 400] <- NA

df$LE[which(df$LE_QC == 2)] <- NA 

df$H[which(df$H_QC == 2)] <- NA 

df$FCO2[which(df$FCO2_QC == 2)] <- NA

##### remove data FCO2 from land ######
df$FCO2[df$WD > 45 & df$WD < 315] <- NA


##### Atmospheric stability classification #####
# 
# unstable <- df$ZL[which(df$ZL < -0.1)]          #unstable, previously y  
# neutral <- df$ZL[which(df$ZL > -0.1 & df$ZL < 0.1)]   #neutral, previously z
# stable <- df$ZL[which(df$ZL > 0.1)]             #stable, previously x

# Average to 1 month
df_month <- timeAverage(df, avg.time = "1 month")

# Average to 1 year
df_year <- timeAverage(df, avg.time = "1 year")

##### Merge satellite and eddy covariance data by month######
df_merge_month <- merge(df_month, df_sat_month, by = "date")

###### Merge satellite and eddy covariance data by year#####
df_merge_year <- merge(df_year, df_sat_year, by = "date")

##### Random Filter after merge#####
df_merge_month$CHL[df_merge_month$CHL > 3] <- NA
df_merge_month$TS[df_merge_month$TS > 34] <- NA

rm(df_biomet, df_ec)

# Create Month Variable
df_merge_month$month <- months(df_merge_month$date)
# Create Year Variable
df_merge_month$year <- format(df_merge_month$date,"%Y")






#### MONTHLY TIME SCALE - Partitioning monsoon & analysis ####

NEM <- selectByDate(df_merge_month, month = c(12,1,2,3))
SWM <- selectByDate(df_merge_month, month = c(6,7,8,9))
FTM <- selectByDate(df_merge_month, month = c(10,11))
STM <- selectByDate(df_merge_month, month = c(4,5))

# creating wind rose
windRose(NEM, ws="WS",wd='WD', paddle = F)
windRose(SWM, ws="WS",wd='WD', paddle = F)

# Corelation monsoon 
corPlot(df_merge_month)
corPlot(NEM) #PRAIN, PPFD
corPlot(SWM) #RG,SH
corPlot(FTM) #PPFD, RN
corPlot(STM) #PRAIN,RG

plot(NEM$P_RAIN, NEM$FCO2, xlab= 'Rain', ylab='CO2 Flux', 
     main ='NEM')
abline(lm(NEM$FCO2~NEM$P_RAIN,data = NEM), col = "red")

plot(NEM$PPFD, NEM$FCO2, xlab= 'PPFD', ylab='CO2 Flux', 
     main ='NEM')
abline(lm(NEM$FCO2~NEM$PPFD,data = NEM), col = "red")

plot(STM$P_RAIN, STM$FCO2, xlab= 'Rain', ylab='CO2 Flux', 
     main ='STM')
abline(lm(STM$FCO2~STM$P_RAIN,data = STM), col = "red")

plot(STM$RG, STM$FCO2, xlab= 'Global Radiation', ylab='CO2 Flux', 
     main ='STM')
abline(lm(STM$FCO2~STM$RG,data = STM), col = "red")

plot(SWM$RG, SWM$FCO2, xlab= 'Global Radiation', ylab='CO2 Flux', 
     main ='SWM')
abline(lm(SWM$FCO2~SWM$RG,data = SWM), col = "red")

plot(SWM$PPFD, SWM$FCO2, xlab= 'PPFD', ylab='CO2 Flux', main="SWM")
abline(lm(SWM$FCO2~SWM$PPFD,data = SWM), col = "red")

plot(FTM$PPFD, FTM$FCO2, xlab= 'PPFD', ylab='CO2 Flux', 
     main ='FTM')
abline(lm(FTM$FCO2~FTM$PPFD,data = FTM), col = "red")

plot(FTM$RN, FTM$FCO2, xlab= 'Net Radiation', ylab='CO2 Flux',
     main ='FTM')
abline(lm(FTM$FCO2~FTM$RN,data = FTM), col = "red")



# partition stable and unstable dataset          
df_merge_month_unstable <- subset(df_merge_month,df_merge_month$ZL < 0) # convection
df_merge_month_unstable <- data.frame(df_merge_month_unstable,
                                      abs(df_merge_month_unstable$ZL)) # convert all negative values to +ve to make logarithmic plot
colnames(df_merge_month_unstable)[31] <-'abs_zL'
df_merge_month_stable <- subset(df_merge_month, df_merge_month$ZL>=0)

# partitioning co2 positive and -ve values
CO2_positive <- subset(df_merge_month,df_merge_month$FCO2 > 0)
CO2_negative <- subset(df_merge_month,df_merge_month$FCO2 < 0)



# Plotting CO2 
plot(log(df_merge_month$WS), df_merge_month$FCO2, 
     xlab= 'Wind Speed', ylab='CO2 Flux')
lmEB <- lm(df_merge_month$FCO2 ~ log(df_merge_month$WS))
abline(lmEB, col='red')
summary(lmEB)
rm(lmEB)
# Check the correlated or no? To check the correlation test (Pearson Coreelation) 
cor.test(log(df_merge_month$WS), df_merge_month$FCO2)

# plot(CO2_positive$WS,CO2_positive$FCO2)
# plot(CO2_negative$WS,CO2_negative$FCO2)
# cor.test(CO2_positive$WS,CO2_positive$FCO2) #p-value 0.06 no corelation
# cor.test(CO2_positive$TA,CO2_positive$FCO2) #p-value 0.01 got corelation
# cor.test(CO2_positive$SST,CO2_positive$FCO2) #p-value 0.75 no corelation
# cor.test(CO2_positive$ZL,CO2_positive$FCO2) #p-value 0.76 no corelation
# cor.test(CO2_positive$PAR,CO2_positive$FCO2) #p-value 0.8727 no corelation
# corPlot(CO2_positive)




#### PLOT - FCO2 monthly trends ####
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
#points(FCO2_2016$date,FCO2_2016$FCO2, pch = 19, col = "Red")
#points(FCO2_2017$date,FCO2_2017$FCO2, pch = 19, col = "Orange")
lines(FCO2_2017$date,FCO2_2017$FCO2, pch = 19, col = "Orange")
#points(FCO2_2018$date,FCO2_2018$FCO2, pch = 19, col = "Blue")
lines(FCO2_2018$date,FCO2_2018$FCO2, pch = 19, col = "Blue")
#points(FCO2_2019$date,FCO2_2019$FCO2, pch = 19, col = "Purple")
lines(FCO2_2019$date,FCO2_2019$FCO2, pch = 19, col = "Purple")
#points(FCO2_2020$date,FCO2_2020$FCO2, pch = 19, col = "Black")
lines(FCO2_2020$date,FCO2_2020$FCO2, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", 
                                        "Apr", "May", "Jun", 
                                        "Jul", "Aug", "Sep", 
                                        "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('CO'['2'],' flux (','mol',' ','m'^{'-2'}, 
                       ' ', 'yr'^{'-1'},')',sep = "")), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c( "Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()



#### BIO CONTROL PLOT - CHL monthly trends ####
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
#points(CHL_2016$date,CHL_2016$CHL, pch = 19, col = "Red")
#points(CHL_2017$date,CHL_2017$CHL, pch = 19, col = "Orange")
lines(CHL_2017$date,CHL_2017$CHL, pch = 19, col = "Orange")
#points(CHL_2018$date,CHL_2018$CHL, pch = 19, col = "Blue")
lines(CHL_2018$date,CHL_2018$CHL, pch = 19, col = "Blue")
#points(CHL_2019$date,CHL_2019$CHL, pch = 19, col = "Purple")
lines(CHL_2019$date,CHL_2019$CHL, pch = 19, col = "Purple")
#points(CHL_2020$date,CHL_2020$CHL, pch = 19, col = "Black")
lines(CHL_2020$date,CHL_2020$CHL, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
#mtext(expression(paste('Chlorophyll')), side = 2, line = 2)
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

#### BIO CONTROL PLOT - PPFD monthly trends ####
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
#points(PPFD_2019$date,PPFD_2019$PPFD, pch = 19, col = "Purple")
#points(PPFD_2020$date,PPFD_2020$PPFD, pch = 19, col = "Black")
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

#### BIO CONTROL PLOT - PAR monthly trends ####
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
#points(PAR_2016$date,PAR_2016$PAR, pch = 19, col = "Red")
#points(PAR_2017$date,PAR_2017$PAR, pch = 19, col = "Orange")
lines(PAR_2017$date,PAR_2017$PAR, pch = 19, col = "Orange")
#points(PAR_2018$date,PAR_2018$PAR, pch = 19, col = "Blue")
lines(PAR_2018$date,PAR_2018$PAR, pch = 19, col = "Blue")
#points(PAR_2019$date,PAR_2019$PAR, pch = 19, col = "Purple")
lines(PAR_2019$date,PAR_2019$PAR, pch = 19, col = "Purple")
#points(PAR_2020$date,PAR_2020$PAR, pch = 19, col = "Black")
lines(PAR_2020$date,PAR_2020$PAR, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('PAR')), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c( "Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()

plot(df_merge_month$PAR,df_merge_month$PPFD)
cor.test(df_merge_month$PAR,df_merge_month$PPFD)

#### PYHSICAL CONTROL PLOT - WS monthly trends ####
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
#points(WS_2016$date,WS_2016$WS, pch = 19, col = "Red")
#points(WS_2017$date,WS_2017$WS, pch = 19, col = "Orange")
lines(WS_2017$date,WS_2017$WS, pch = 19, col = "Orange")
#points(WS_2018$date,WS_2018$WS, pch = 19, col = "Blue")
lines(WS_2018$date,WS_2018$WS, pch = 19, col = "Blue")
#points(WS_2019$date,WS_2019$WS, pch = 19, col = "Purple")
lines(WS_2019$date,WS_2019$WS, pch = 19, col = "Purple")
#points(WS_2020$date,WS_2020$WS, pch = 19, col = "Black")
lines(WS_2020$date,WS_2020$WS, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('U (', 'ms'^'-1', ')', sep = "")), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c("Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, cex = 0.85, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()


#### PYHSICAL CONTROL PLOT - TA monthly trends ####
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
#points(TA_2016$date,TA_2016$TA, pch = 19, col = "Red")
#points(TA_2017$date,TA_2017$TA, pch = 19, col = "Orange")
lines(TA_2017$date,TA_2017$TA, pch = 19, col = "Orange")
#points(TA_2018$date,TA_2018$TA, pch = 19, col = "Blue")
lines(TA_2018$date,TA_2018$TA, pch = 19, col = "Blue")
#points(TA_2019$date,TA_2019$TA, pch = 19, col = "Purple")
lines(TA_2019$date,TA_2019$TA, pch = 19, col = "Purple")
#points(TA_2020$date,TA_2020$TA, pch = 19, col = "Black")
lines(TA_2020$date,TA_2020$TA, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('TA',' (','C'^{'o'},')', sep = "")), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c("Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, cex = 0.85, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()



#### PHYSICAL CONTROL PLOT - TS monthly trends ####
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
#points(TS_2016$date,TS_2016$TS, pch = 19, col = "Red")
#points(TS_2017$date,TS_2017$TS, pch = 19, col = "Orange")
lines(TS_2017$date,TS_2017$TS, pch = 19, col = "Orange")
#points(TS_2018$date,TS_2018$TS, pch = 19, col = "Blue")
lines(TS_2018$date,TS_2018$TS, pch = 19, col = "Blue")
#points(TS_2019$date,TS_2019$TS, pch = 19, col = "Purple")
lines(TS_2019$date,TS_2019$TS, pch = 19, col = "Purple")
#points(TS_2020$date,TS_2020$TS, pch = 19, col = "Black")
lines(TS_2020$date,TS_2020$TS, pch = 19, col = "Black")
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('TS',' (','C'^{'o'},')', sep = "")), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_colour<-c( "Red","Orange","Blue","Purple","Black")
legend("topleft", legend = legend_order, bty = "n",
       col = legend_colour, pch=19, cex = 0.85, ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()


#### PHYSICAL CONTROL PLOT - SST monthly trends ####
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
#points(SST_2016$date,SST_2016$SST, pch = 19, col = "Red")
#points(SST_2017$date,SST_2017$SST, pch = 19, col = "Orange")
lines(SST_2017$date,SST_2017$SST, pch = 19, col = "Orange")
#points(SST_2018$date,SST_2018$SST, pch = 19, col = "Blue")
lines(SST_2018$date,SST_2018$SST, pch = 19, col = "Blue")
#points(SST_2019$date,SST_2019$SST, pch = 19, col = "Purple")
lines(SST_2019$date,SST_2019$SST, pch = 19, col = "Purple")
#points(SST_2020$date,SST_2020$SST, pch = 19, col = "Black")
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



# ####  UNUSED PHYSICAL CONTROL PLOT - USTAR monthly trends ####
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
 #points(USTAR_2016$date,USTAR_2016$USTAR, pch = 19, col = "Red")
 #points(USTAR_2017$date,USTAR_2017$USTAR, pch = 19, col = "Orange")
 lines(USTAR_2017$date,USTAR_2017$USTAR, pch = 19, col = "Orange")
 #points(USTAR_2018$date,USTAR_2018$USTAR, pch = 19, col = "Blue")
 lines(USTAR_2018$date,USTAR_2018$USTAR, pch = 19, col = "Blue")
 #points(USTAR_2019$date,USTAR_2019$USTAR, pch = 19, col = "Purple")
 lines(USTAR_2019$date,USTAR_2019$USTAR, pch = 19, col = "Purple")
 #points(USTAR_2020$date,USTAR_2020$USTAR, pch = 19, col = "Black")
 lines(USTAR_2020$date,USTAR_2020$USTAR, pch = 19, col = "Black")
 axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
 mtext('Months', side = 1, line = 2.5)
 mtext(expression(paste('u*')), side = 2, line = 2)
 legend_order <- c(2016:2020)
 legend_colour<-c( "Red","Orange","Blue","Purple","Black")
 legend("topleft", legend = legend_order, bty = "n",
        col = legend_colour, pch=19, ncol = 2)
# #grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
 dev.off()

# #### UNUSED PHYSICAL CONTROL PLOT - Z/L monthly trends ####
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
# points(ZL_2016$date,ZL_2016$ZL, pch = 19, col = "Red")
 #points(ZL_2017$date,ZL_2017$ZL, pch = 19, col = "Orange")
 lines(ZL_2017$date,ZL_2017$ZL, pch = 19, col = "Orange")
# points(ZL_2018$date,ZL_2018$ZL, pch = 19, col = "Blue")
 lines(ZL_2018$date,ZL_2018$ZL, pch = 19, col = "Blue")
 #points(ZL_2019$date,ZL_2019$ZL, pch = 19, col = "Purple")
 lines(ZL_2019$date,ZL_2019$ZL, pch = 19, col = "Purple")
 #points(ZL_2020$date,ZL_2020$ZL, pch = 19, col = "Black")
 lines(ZL_2020$date,ZL_2020$ZL, pch = 19, col = "Black")
 axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
 #mtext('Months', side = 1, line = 2.5)
 mtext(expression(paste('Z/L')), side = 2, line = 2)
 legend_order <- c(2016:2020)
 legend_colour<-c( "Red","Orange","Blue","Purple","Black")
 legend("topleft", legend = legend_order, bty = "n",
        col = legend_colour, pch=19, ncol = 2)
# #grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
 dev.off()

#### UNUSED PHYSICAL CONTROL PLOT - RH monthly trends ####
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
# points(RH_2016$date,RH_2016$RH, pch = 19, col = "Red")
# points(RH_2017$date,RH_2017$RH, pch = 19, col = "Orange")
 lines(RH_2017$date,RH_2017$RH, pch = 19, col = "Orange")
# points(RH_2018$date,RH_2018$RH, pch = 19, col = "Blue")
 lines(RH_2018$date,RH_2018$RH, pch = 19, col = "Blue")
# points(RH_2019$date,RH_2019$RH, pch = 19, col = "Purple")
 lines(RH_2019$date,RH_2019$RH, pch = 19, col = "Purple")
# points(RH_2020$date,RH_2020$RH, pch = 19, col = "Black")
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


#### TIME SERIES ANALYSIS - monthly trends ####

library(dplyr)
library(ggpubr)
library(rstatix)


# FCO2 Theil-Sen analysis
jpeg(filename='fig/FCO2_theil.jpg', unit = 'cm', width = 20, height = 20, res = 360)
TheilSen(df_merge_month,pollutant = "FCO2", xlab = "year", date.breaks = 5, date.format = "%Y")
dev.off()

# # FCO2 trend analysis
# jpeg(filename='fig/FCO2_smooth.jpg', unit = 'cm', width = 20, height = 20, res = 360)
# smoothTrend(df_merge_month, pollutant = "FCO2", deseason = T, simulate = T)
# dev.off()

# jpeg(filename='fig/FCO2_WS_alt.jpg', unit = 'cm', width = 10, height = 10, res = 360)
# ggplot(df_merge_month, aes(FCO2, WS)) +
#   geom_point() +
#   geom_smooth(formula = y~x, method = "lm") + 
#   stat_regline_equation(data = df_merge_month, label.x = 0.0, label.y = 1.1)
# dev.off()


#### DESCRIPTIVE STATISTICS ####

library(lubridate)
library(plyr)

## Mean and SD for all years
mean_df <- sapply(na.omit(df_merge_month[c(5,26,4,3,15,20,27,30,17,18,7,11)]), mean)
sd_df <- sapply(na.omit(df_merge_month[c(5,26,4,3,15,20,27,30,17,18,7,11)]), sd)
df_statistic <- data.frame(rbind(mean_df,sd_df))
row.names(df_statistic) <- c("Mean","SD")

## Mean and SD for every year 
year_df <- df_merge_year[c(1,5,26,4,3,15,20,27,30,17,18,7,11)]

# EC data
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

## Sat data
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
sd_df <- cbind(sd_df_1,sd_df_2)
sd_df <- as.data.frame(sd_df)

sd_df <- cbind(year_df$date, sd_df)

colnames(sd_df)[1] <- "date"

df_statistic_year <- rbind.fill(year_df,sd_df)

row.names(df_statistic_year) <- c("Mean 2016", "Mean 2017", "Mean 2018", "Mean 2019", "Mean 2020",
                                  "SD 2016", "SD 2017", "SD 2018", "SD 2019", "SD 2020")

df_statistic_year <- df_statistic_year[order(df_statistic_year$date),]
df_statistic_year <-round(df_statistic_year[,-1], 2)


# Pearson correlation coefficient
cor_Pearson <- cor(df_merge_month[c(5,26,4,3,15,20,27,30,17,18,7,11)], 
                   method = "pearson", use = "complete.obs")
cor_Pearson[upper.tri(cor_Pearson, diag = FALSE)]<- 0

rm(sd_df_1,sd_df_2)

#### CORRELATION Analysis ####

library("Hmisc")

pvalue_test <- data.frame(c(
  cor.test(df_merge_month$FCO2, df_merge_month$CHL)[3] ,
  cor.test(df_merge_month$FCO2, df_merge_month$SST)[3] ,
  cor.test(df_merge_month$FCO2, df_merge_month$TA)[3] ,
  cor.test(df_merge_month$FCO2, df_merge_month$TS)[3] ,
  cor.test(df_merge_month$FCO2, df_merge_month$PAR)[3] ,
  cor.test(df_merge_month$FCO2, df_merge_month$USTAR)[3] ,
  cor.test(df_merge_month$FCO2, df_merge_month$RH)[3] ,
  #cor.test(df_merge_month$FCO2, df_merge_month$PPFD)[3] ,
  cor.test(df_merge_month$FCO2, df_merge_month$ZL)[3]
))

colnames(pvalue_test) <- c("CHL","SST","TA","TS","PAR","USTAR","RH","ZL")
row.names(pvalue_test) <- "FCO2"

pvalue_test <- rcorr(as.matrix(df_merge_month[c(5,26,4,3,15,20,27,30,17,18,7,11)]), 
                     type = "pearson")$P
pvalue_test[upper.tri(pvalue_test)] <- 0



#### BIO CONTROL - Correlation Plots ####

## BIO CHL
jpeg(filename='fig/FCO2_CHL_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$CHL, df_merge_month$FCO2,pch = 19,cex= 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$CHL,data = df_merge_month), col = "red")
mtext(side = 1, text = expression(paste('Chl', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')', sep = )),line = 2)
dev.off()


## BIO PAR
jpeg(filename='fig/FCO2_PAR_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$PAR, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$PAR,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('PAR', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()

## BIO PPFD
jpeg(filename='fig/FCO2_PPFD_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$PPFD, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$PPFD,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('PPFD', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()


#### PHY CONTROL - Correlation Plots ####

## PHY WS
jpeg(filename='fig/FCO2_WS_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$WS, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$WS,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('Wind Speed (', 'ms'^'-1', ')', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()

## PHY TA
jpeg(filename='fig/FCO2_TA_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$TA, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$TS,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('T'['a'],' (','C'^{'o'},')', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()

## PHY TS
jpeg(filename='fig/FCO2_TS_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$TS, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$TS,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('T'['s'],' (','C'^{'o'},')', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()

## PHY SST
jpeg(filename='fig/FCO2_SST_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$SST, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$SST,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('SST (','C'^'o', ')', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()

# ## PHY RH
# jpeg(filename='fig/FCO2_RH_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
# par(mar = c(4,4,1,1))
# plot(df_merge_month$RH, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
# abline(lm(df_merge_month$FCO2~df_merge_month$RH,data = df_merge_month), col = "red")
# mtext(side = 1, text =expression(paste('RH', sep = "")), line = 2 )
# mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
# dev.off()

# ## PHY USTAR
# jpeg(filename='fig/FCO2_USTAR_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
# par(mar = c(4,4,1,1))
# plot(df_merge_month$USTAR, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
# abline(lm(df_merge_month$FCO2~df_merge_month$RH,data = df_merge_month), col = "red")
# mtext(side = 1, text =expression(paste('USTAR', sep = "")), line = 2 )
# mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
# dev.off()

# ## PHY Z/L
# jpeg(filename='fig/FCO2_ZL_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
# par(mar = c(4,4,1,1))
# plot(df_merge_month$ZL, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
# abline(lm(df_merge_month$FCO2~df_merge_month$ZL,data = df_merge_month), col = "red")
# mtext(side = 1, text =expression(paste('ZL', sep = "")), line = 2 )
# mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
# dev.off()

##PHY CHL 

jpeg(filename='fig/FCO2_CHL_lin.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df_merge_month$CHL, df_merge_month$FCO2,pch = 19, cex = 0.5, xlab= "", ylab="")
abline(lm(df_merge_month$FCO2~df_merge_month$CHL,data = df_merge_month), col = "red")
mtext(side = 1, text =expression(paste('CHL', sep = "")), line = 2 )
mtext(side = 2,text = expression(paste('CO'['2'],' flux (','molC','','m'^{'-2'}, 'yr'^{'-1'},')',sep = "")), line = 2)
dev.off()

#### BIO CONTROL - Boxplot analysis ####


# inter-annual
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

#### PHYSICAL CONTROL - Boxplot analysis ####

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


# jpeg(filename='fig/USTAR_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
# boxplot(PAR ~ year, data = df_merge_month, xlab='Month',ylab='')
# mtext(expression(paste('USTAR')), 
#       side=2, line = 2.5)
# mtext(text = "Year",side = 1, line = 2)
# #title("USTAR", line=1)
# dev.off()
# 
# jpeg(filename='fig/RH_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
# boxplot(RH ~ year, data = df_merge_month, xlab='Month',ylab='')
# mtext(expression(paste('RH')), 
#       side=2, line = 2.5)
# mtext(text = "Year",side = 1, line = 2)
# #title("Relative Humidity", line=1)
# dev.off()
# 
# jpeg(filename='fig/ZL_year_box.jpg', unit = 'cm', width = 12, height = 14, res = 360)
# boxplot(ZL ~ year, data = df_merge_month, xlab='Month',ylab='')
# mtext(expression(paste('ZL')), 
#       side=2, line = 2.5)
# mtext(text = "Year",side = 1, line = 2)
# #title("ZL", line=1)
# dev.off()


library(scales)
library(ggplot2)

#### BIO CONTROL - smooth trends ggplot ####

jpeg(filename='fig/CO2_year_smooth.jpg', unit = 'cm', 
     width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
ggplot(df_merge_month, aes(date, FCO2)) +
  geom_line() + 
  xlab('Year') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('CO'['2'],' flux (',mu,'mol ',' m'^{'-2'}, ' s'^{'-1'},')')))
#geom_text(data = df_merge_month, aes(date, CO2),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/CHL_year_smooth.jpg', unit = 'cm', 
     width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
ggplot(df_merge_month, aes(date, CHL)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab("Chlorophyll")
#geom_text(data = df_merge_month, aes(date, CHL),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/PAR_year_smooth.jpg', unit = 'cm', 
     width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
ggplot(df_merge_month, aes(date, PAR)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('PAR', sep = "")))
#geom_text(data = df_merge_month, aes(date, PAR),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/PPFD_year_smooth.jpg', unit = 'cm', 
     width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES

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

#### PHY CONTROL - smooth trends ggplot ####

jpeg(filename='fig/WS_year_smooth.jpg', unit = 'cm', 
     width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES

ggplot(df_merge_month, aes(date, WS)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('Wind Speed (', 'ms'^'-1', ')', sep = "")))
#geom_text(data = df_merge_month, aes(date, WS),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off ()

jpeg(filename='fig/TA_year_smooth.jpg', unit = 'cm', 
     width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES

ggplot(df_merge_month, aes(date, TA)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('T'['a'],' (','C'^{'o'},')', sep = "")))
#geom_text(data = df_merge_month, aes(date, TA),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/TS_year_smooth.jpg', unit = 'cm', 
     width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES

ggplot(df_merge_month, aes(date, TS)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('T'['s'],' (','C'^{'o'},')', sep = "")))
#geom_text(data = df_merge_month, aes(date, TS),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()

jpeg(filename='fig/SST_year_smooth.jpg', unit = 'cm', 
     width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES

ggplot(df_merge_month, aes(date, SST)) +
  geom_line() + 
  xlab('Month') +
  stat_smooth(method='auto', color='black') +
  ylab(expression(paste('SST',' (','C'^{'o'},')', sep = "")))
#geom_text(data = df_merge_month, aes(date, SST),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()



#### BIO CONTROL - yearly analysis ####

## FCO2
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

## CHL
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

## PAR
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

## PPFD
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

#### PHY CONTROL - yearly analysis ####

## WS
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

## TA
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

## TS
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

## SST
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

# ## RH
# jpeg(filename='fig/Timeseries_RH_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)
# df_merge_month_1 <- df_merge_month
# df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
# ggplot((df_merge_month_1), aes(x = date, y = RH)) + 
#   geom_line(col='pink') +  
#   scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
#   xlab(label = "Time") +
#   ylab(label = expression(paste('Relative Humidity'))) + 
#   facet_wrap( ~ format(date, "%Y"), scales = "free") + 
#   theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))
# dev.off()
# 
# ## USTAR
# jpeg(filename='fig/Timeseries_USTAR_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)
# df_merge_month_1 <- df_merge_month
# df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
# ggplot((df_merge_month_1), aes(x = date, y = USTAR)) + 
#   geom_line(col='pink') +  
#   scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
#   xlab(label = "Time") +
#   ylab(label = expression(paste('USTAR'))) + 
#   facet_wrap( ~ format(date, "%Y"), scales = "free") + 
#   theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))
# dev.off()
# 
# 
# ## Z/L
# jpeg(filename='fig/Timeseries_ZL_all.jpg', unit = 'cm', width = 15, height = 12, res = 360)
# df_merge_month_1 <- df_merge_month
# df_merge_month_1$date <- as.Date(df_merge_month_1$date)+1
# ggplot((df_merge_month_1), aes(x = date, y = ZL)) + 
#   geom_line(col='pink') +  
#   scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%Y-%m"))+ 
#   xlab(label = "Time") +
#   ylab(label = expression(paste('ZL'))) + 
#   facet_wrap( ~ format(date, "%Y"), scales = "free") + 
#   theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))
# dev.off()

rm(df_merge_month_1)

#### MISC PLOTS ####
plot(NEM$date,NEM$FCO2, type='l',col='blue', ylim = c(-0.12,0.1))    #LINE PLOT COMPARE MONSOON
lines(SWM$date,SWM$FCO2,type='l')
lines(STM$date,STM$FCO2, type='l', col='red')
lines(FTM$date,FTM$FCO2, type='l', col='purple')

plot(NEM$date[-1],NEM$RN[-1], type='l',col='blue', ylim = c(0,80)) #first data got problem need to check for RN
lines(SWM$date,SWM$RN,type='l')
lines(STM$date,STM$RN, type='l', col='red')
lines(FTM$date,FTM$RN, type='l', col='purple')

#### BARPLOTS #####
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



#### LATTICE PLOT SST & TS COMPARISON ####


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
rm(plot_1,plot_2)

# #### DAILY TIME SCALE ####
# df_day <- timeAverage(df, avg.time = "1 day")
# cor.test(df_day$FCO2,df_day$WS)

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

#####Precipitation####
##Cumulative Rainfall,2015, 2016,2017 and 2018 Jan-March no data for rainfall
library(lubridate)
library(openair)
rain_c <- selectByDate(df,year = 2018)[,c(1,14)]
rain_2018 <- aggregate((rain_c$P_RAIN)~month(rain_c$date),
                       data=rain_c,FUN=sum)
rain_c <- selectByDate(df,year = 2019)[,c(1,14)]
rain_2019 <- aggregate((rain_c$P_RAIN)~month(rain_c$date),
                       data=rain_c,FUN=sum)
rain_c <- selectByDate(df,year = 2020)[,c(1,14)]
rain_2020 <- aggregate((rain_c$P_RAIN)~month(rain_c$date),
                       data=rain_c,FUN=sum)


#####spactral analysis######
spectral_1 <- read.csv('C:/Users/User-PC/Documents/co2/data/PLOT R/20180228-0730_binned_cospectra_2022-01-01T173055_adv.csv')
# spectral_1 <- spectral_1[,c(1,2)]
spectral_2 <- read.csv('C:/Users/User-PC/Documents/co2/data/PLOT R/20180630-0630_binned_cospectra_2022-01-01T173055_adv.csv')
#spectral_3 <- read.csv('C:/Users/User-PC/Documents/co2/data/PLOT R/20160222-0530_binned_cospectra_2022-01-01T173055_adv.csv')
#spectral_4 <- read.csv('C:/Users/User-PC/Documents/co2/data/PLOT R/20190411-0600_binned_cospectra_2022-01-01T173055_adv.csv')
#spectral <-rbind(spectral_1,spectral_2,spectral_3,spectral_4)
spectral <-rbind(spectral_1,spectral_2)
#Filter -99999
# spectral <- spectral[which(spectral[,1]!=-9999),]
spectral_1 <- spectral_1[which(spectral_1[,1]!=-9999),]
spectral_2 <- spectral_2[which(spectral_2[,1]!=-9999),]
#spectral_3 <- spectral_3[which(spectral_3[,1]!=-9999),]
#spectral_4 <- spectral_4[which(spectral_4[,1]!=-9999),]
#Filter Negative 2nd colun because 1st column no negative
# spectral <- spectral[which(spectral[,2]>0),]
spectral_1 <- spectral_1[which(spectral_1[,2]>0),]
spectral_2 <- spectral_2[which(spectral_2[,2]>0),]
#spectral_3 <- spectral_3[which(spectral_3[,2]>0),]
#spectral_4 <- spectral_4[which(spectral_4[,2]>0),]



plot(spectral_1$normalized_frequency,spectral_1$f_nat.cospec.w_u..cov.w_u., pch =19)
# spectral_1_log <- spectral_1
# spectral_1_log$normalized_frequency <- log10(spectral_1_log$normalized_frequency)
# plot(spectral_1_log$normalized_frequency,spectral_1_log$f_nat.cospec.w_u..cov.w_u., pch = 19)
# colnames(spectral_1_log)[1] <- "log_frequency"
# colnames(spectral_1_log)[2] <- "power"
spectral_1_log <- log10(spectral_1)
colnames(spectral_1_log)[1] <- "log_frequency_1"
colnames(spectral_1_log)[2] <- "log_power_1"


plot(spectral_2$normalized_frequency,spectral_2$f_nat.cospec.w_u..cov.w_u., pch =19)
# spectral_2_log <- spectral_2
# spectral_2_log$normalized_frequency <- log10(spectral_2_log$normalized_frequency)
# plot(spectral_2_log$normalized_frequency,spectral_2_log$f_nat.cospec.w_u..cov.w_u., pch = 19)
# colnames(spectral_2_log)[1] <- "log_frequency"
# colnames(spectral_2_log)[2] <- "power"
spectral_2_log <- log10(spectral_2)
colnames(spectral_2_log)[1] <- "log_frequency_2"
colnames(spectral_2_log)[2] <- "log_power_2"

#plot(spectral_3$normalized_frequency,spectral_3$f_nat.cospec.w_u..cov.w_u., pch =19)
# spectral_3_log <- spectral_3
# spectral_3_log$normalized_frequency <- log10(spectral_3_log$normalized_frequency)
# plot(spectral_3_log$normalized_frequency,spectral_3_log$f_nat.cospec.w_u..cov.w_u., pch = 19)
# colnames(spectral_3_log)[1] <- "log_frequency"
# colnames(spectral_3_log)[2] <- "power"
#spectral_3_log <- log10(spectral_3)
#colnames(spectral_3_log)[1] <- "log_frequency_3"
#colnames(spectral_3_log)[2] <- "log_power_3"

#plot(spectral_4$normalized_frequency,spectral_4$f_nat.cospec.w_u..cov.w_u., pch =19)
# spectral_4_log <- spectral_4
# spectral_4_log$normalized_frequency <- log10(spectral_4_log$normalized_frequency)
# plot(spectral_4_log$normalized_frequency,spectral_4_log$f_nat.cospec.w_u..cov.w_u., pch = 19)
# colnames(spectral_4_log)[1] <- "log_frequency"
# colnames(spectral_4_log)[2] <- "power"
#spectral_4_log <- log10(spectral_4)
#colnames(spectral_4_log)[1] <- "log_frequency_4"
#colnames(spectral_4_log)[2] <- "log_power_4"

#Combine data frames (withoutlog)
library(rowr)
spectral <- cbind.fill(spectral_1,spectral_2)
colnames(spectral) <- c("frequency_1","power_1","frequency_2","power_2")

library(MASS)
library(scales)
library(ggplot2)
jpeg(filename='fig/spectral_AGOOD.jpg', unit = 'cm', width = 12, height = 10, res = 360)
ggplot(spectral, aes(x = frequency_1,y = power_1))+
  stat_smooth(method = lm, formula = y ~ poly(x,2,raw = TRUE), colour = "green") +
  stat_smooth(aes(frequency_2, y = power_2), 
              method = lm, formula = y ~ poly(x,2,raw = TRUE), colour = "purple") +
   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))  +
  xlab( expression(paste(pi['2'],' =',' fz / U',sep = ""))) +
  ylab(expression(paste( sep=""))) +
  theme_bw() + 
  theme(axis.title.y  = element_text(angle = 0, vjust = 0.5, hjust=1))
dev.off()

##Combine log   data frames
library(rowr)
spectral_log <- cbind.fill(spectral_1_log,spectral_2_log)
#spectral_log <- cbind.fill(spectral_log,spectral_3_log)
#spectral_log <- cbind.fill(spectral_log,spectral_4_log)


# jpeg(filename='fig/spectral_logA1.jpg', unit = 'cm', width = 12, height = 10, res = 360)
# ggplot(spectral_log, aes(x = log_frequency_1,y = log_power_1))+
#   stat_smooth(method = lm, formula = y ~ poly(x,2,raw = TRUE), colour = "green") +
#   stat_smooth(aes(log_frequency_2, y = log_power_2), 
#               method = lm, formula = y ~ poly(x,2,raw = TRUE), colour = "purple") +
#   xlab("log Normalized Frequency") +
#   ylab("log Power") +
#   theme_bw()
# dev.off()




#stat_smooth(aes(frequency_3, y = power_3),
      #      method = lm, formula = y ~ poly(x,2,raw = TRUE), colour = "blue") +
  #stat_smooth(aes(frequency_4, y = power_4),
         #     method = lm, formula = y ~ poly(x,2,raw = TRUE), colour = "orange") +


library("ggplot2")
library("ggpubr")
library("rstatix")
library("ggpmisc")

ggplot(spectral_1_log, aes(log_frequency, power)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

ggplot(spectral_2_log, aes(log_frequency, power)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

ggplot(spectral_3_log, aes(log_frequency, power)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))



#Logatrithm
#spectral_log10 <- log10(spectral)

spectral_log10_2 <- log10(spectral_2)

#Plot Logartihm
colnames(spectral_log10_2)[1] <- "log_frequency"
colnames(spectral_log10_2)[2] <- "log_power"

jpeg(filename='fig/spectral_2.jpg', unit = 'cm', width = 12, height = 10, res = 360)
plot(spectral_log10_2$log_frequency,spectral_log10_2$log_power)
dev.off()





library("ggplot2")
library("ggpubr")
library("rstatix")
library("ggpmisc")
jpeg(filename='fig/spectral_1.jpg', unit = 'cm', width = 12, height = 10, res = 360)
ggplot(spectral_log10_2, aes(log_frequency, log_power)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))
dev.off()

spectral_log10_2 <- spectral_log10_2[which(spectral_log10_2$log_frequency>= 0),]
jpeg(filename='fig/spectral_2.jpg', unit = 'cm', width = 12, height = 10, res = 360)
ggplot(spectral_log10_2, aes(log_frequency, log_power)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE)) 
dev.off()


#1-CO2 flux with the ratio u*/U > ???;
#Need to calculate the average ratio. Determine the highest and lowest ratio values
mean(na.omit(df$USTAR/df$WS)) #Average
max(na.omit(df$USTAR/df$WS)) #Highest
min(na.omit(df$USTAR/df$WS)) #Lowest


#2-The mean wind speed is AVG??? ± SD??? m s–1 . 
mean(na.omit(df$WS)) #aVERAGE WINDSPEED
sd(na.omit(df$WS))
#3-The wind speed range is MIN??? – MAX??? m s–1. 
min(na.omit(df$WS))
max(na.omit(df$WS))

#4-The boundary layer was mostly weakly unstable: PERCENT z/L < –1  , PERCENT z/L < –0.1, PERCENT –0.1 < z/L < 0.1, PERCENT z/L > 0.1, PERCENT z/L > 1.
length(df$ZL[which(df$ZL< -1)])/length(df$ZL) *100 
length(df$ZL[which(df$ZL< -0.1)])/length(df$ZL) *100
length(df$ZL[which(df$ZL> -0.1 & df$ZL< 0.1)])/length(df$ZL) *100
length(df$ZL[which(df$ZL> 0.1)])/length(df$ZL) *100
length(df$ZL[which(df$ZL> 1)])/length(df$ZL) *100


#5-Percentage of flags 0=%,1=% ,2 =% of qfco2
length(df$FCO2_QC[which(df$FCO2_QC == 0)])/length(df$FCO2_QC) *100
length(df$FCO2_QC[which(df$FCO2_QC == 1)])/length(df$FCO2_QC) *100
length(df$FCO2_QC[which(df$FCO2_QC == 2)])/length(df$FCO2_QC) *100



###ustar u###
#plot(df$USTAR, df$WS, xlab= 'WS', ylab='u*', 
    # main ='df')
#abline(lm(df$WS~df$USTAR,data = df), col = "red")

par(mar = c(4,4,1,1))
plot(df$WS, df$USTAR,pch = 19,cex= 0.5, xlab= "", ylab="")
abline(lm(df$USTAR~df$WS,data =df), col = "red")
mtext(side = 1, text = expression(paste('U', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('u*', sep = )),line = 2)

lmustar <- lm(df$USTAR~df$WS,data =df)

summary(lmustar)

#plot(log(df$WS), df$USTAR, 
    # xlab= 'Wind Speed', ylab='u*')
#lmEB <- lm(df$WS ~ log(df$USTAR))
#abline(lmEB, col='red')
#summary(lmEB)
#rm(lmEB)


###slope####
lm(formula = USTAR ~ WS, data = df)




#####u*/ws#####

df_divide <- df$USTAR /df$WS



length (df$FCO2[which(df$divide < 0.416)])
length(df$FCO2[which(df$divide < 0.416)])/length(df$FCO2)*100


####RMSE####


install.packages("Metrics")
library(Metrics)

df_rmse <- cbind(df_merge_month$SST, df_merge_month$TS)
df_rmse <- df_rmse[!rowSums(is.na(df_rmse)),]
rmse(df_rmse[,2], df_rmse[,1])


cor(df_merge_month$SST, df_merge_month$TS,use='complete')
lmSST_TS <- lm(SST~TS,data=SST_TS_month)
summary(lmSST_TS)

#This is the best one, out of below, unless you wann try change the time and see which is the closest
df_rmse <- cbind(df_merge_month$PAR[which(df_merge_month$date >= as.POSIXct("2019-01-01") & df_merge_month$date < as.POSIXct("2020-01-01"))]
                 , df_merge_month$PPFD[which(df_merge_month$date >= as.POSIXct("2019-01-01") & df_merge_month$date < as.POSIXct("2020-01-01"))])
##Based on Plant Growth Chamber Handbook, (Chapter 1) conversion, assume Solar lights fall within 400 to 700nm
#400~700nm , conversion 4.57
#If is total radiation of Pyranometer sensor, conversion 2.1
df_rmse <- df_rmse[!rowSums(is.na(df_rmse)),]
df_rmse[,1] <- df_rmse[,1]*4.57
rmse(df_rmse[,2], df_rmse[,1])
df_rmse

#PPFD 12pm
df_12 <- df[as.numeric(strftime(df$date, "%H%M"))%in% 1200:1200,]
df_12_month<- timeAverage(df_12, avg.time = "1 month")
df_merge_month_12 <- merge(df_12_month,df_sat_month, by = "date") 
df_rmse <- cbind(df_merge_month_12$PAR[which(df_merge_month_12$date >= as.POSIXct("2019-01-01") & df_merge_month_12$date < as.POSIXct("2020-01-01"))]
                 , df_merge_month_12$PPFD[which(df_merge_month_12$date >= as.POSIXct("2019-01-01") & df_merge_month_12$date < as.POSIXct("2020-01-01"))])
df_rmse <- df_rmse[!rowSums(is.na(df_rmse)),]
df_rmse[,1] <- df_rmse[,1]*4.57
rmse(df_rmse[,2], df_rmse[,1])
df_rmse

#PPFD 11am
df_12 <- df[as.numeric(strftime(df$date, "%H%M"))%in% 0300:0300,]
df_12_month<- timeAverage(df_12, avg.time = "1 month")
df_merge_month_12 <- merge(df_12_month,df_sat_month, by = "date") 
df_rmse <- cbind(df_merge_month_12$PAR[which(df_merge_month_12$date >= as.POSIXct("2019-01-01") & df_merge_month_12$date < as.POSIXct("2020-01-01"))]
                 , df_merge_month_12$PPFD[which(df_merge_month_12$date >= as.POSIXct("2019-01-01") & df_merge_month_12$date < as.POSIXct("2020-01-01"))])
df_rmse <- df_rmse[!rowSums(is.na(df_rmse)),]
df_rmse[,1] <- df_rmse[,1]*4.57
rmse(df_rmse[,2], df_rmse[,1])
df_rmse

#PPFD Daily, 12pm
df_12 <- df[as.numeric(strftime(df$date, "%H%M"))%in% 1200:1200,]
df_12_month<- timeAverage(df_12, avg.time = "1 day")
df_sat_day <- timeAverage(df_sat, avg.time = "1 day")
df_merge_month_12 <- merge(df_12_month,df_sat_day, by = "date") 
df_rmse <- cbind(df_merge_month_12$PAR[which(df_merge_month_12$date >= as.POSIXct("2019-01-01") & df_merge_month_12$date < as.POSIXct("2020-01-01"))]
                 , df_merge_month_12$PPFD[which(df_merge_month_12$date >= as.POSIXct("2019-01-01") & df_merge_month_12$date < as.POSIXct("2020-01-01"))])
df_rmse <- df_rmse[!rowSums(is.na(df_rmse)),]
df_rmse[,1] <- df_rmse[,1]*4.57
rmse(df_rmse[,2], df_rmse[,1]) 
df_rmse




library(zoo)

# Note: You need to have the "df" and "df_sat" datasets before you use this script.

# PPFD
PPFD <- data.frame(date=df$date,PPFD=df$PPFD)
PPFD$PPFD <- PPFD$PPFD * 60 * 30 / 10^6 # Convert from umol m-2 s-1 to mol m-2 for 30 min avg time
#PPFD$PPFD[which(PPFD$PPFD < 0)] <- NA # remove all negative values of PPFD

# Convert from umol m-2 to umol m-2 d-1
PPFD24 <-rollapply(PPFD$PPFD, 48, sum, by=48)
date_PPFD24 <- seq(as.Date("2015-11-12"), length.out = 1528, by = "day")
PPFD24 <- data.frame(date_PPFD24,PPFD24=PPFD24)
colnames(PPFD24)[1] <- 'date'
PPFD24$date <- as.POSIXct(PPFD24$date,format = "%Y-%m-%d", tz = "Asia/Kuala_Lumpur")
rm(PPFD,date_PPFD24)

# PAR
PAR <- timeAverage(df_sat, avg.time='day')
PAR <- data.frame(PAR$date,PAR$PAR)
colnames(PAR) <- c('date','PAR')
temp_date <- as.character(PAR$date)
temp_date2 <- paste(temp_date,"08:00:00", sep = " ")
PAR$date <- as.POSIXct(temp_date2,format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Kuala_Lumpur")
rm(temp_date,temp_date2)
PPFD_PAR <- merge(PAR,PPFD24,by='date')
rm(PPFD24,PAR)

# Monthly average

PPFD_PAR_month <- timeAverage(PPFD_PAR, avg.time = "1 month")

# Plot time series
plot(PPFD_PAR_month$date,PPFD_PAR_month$PPFD24,pch=19)
points(PPFD_PAR_month$date,PPFD_PAR_month$PAR,pch=19,col='green')


# Regression and correlation
cor(PPFD_PAR_month$PPFD24,PPFD_PAR_month$PAR,use='complete')
lmPPFD_PAR <- lm(PAR~PPFD24,data=PPFD_PAR_month)
summary(lmPPFD_PAR)

plot(PPFD_PAR_month$PPFD24, PPFD_PAR_month$PAR,pch=19)
abline(lmPPFD_PAR)

mean(PPFD_PAR_month$PAR,na.rm = T)
mean(PPFD_PAR_month$PPFD24, na.rm = T)
# RMSE
sqrt(mean((PPFD_PAR_month$PPFD24 - PPFD_PAR_month$PAR)^2, na.rm = T)) 

rm(lmPPFD_PAR)


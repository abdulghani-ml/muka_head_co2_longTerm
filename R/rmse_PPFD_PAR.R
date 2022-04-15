
library(zoo)

# Note: You need to have the "df" and "df_sat" datasets before you use this script.

# PPFD
PPFD <- data.frame(date=df$date,PPFD=df$PPFD)
PPFD$PPFD <- PPFD$PPFD * 60 * 30 / 10^6 # Convert from umol m-2 s-1 to mol m-2 for 30 min avg time
#PPFD$PPFD[which(PPFD$PPFD < 0)] <- NA # remove all negative values of PPFD

# Convert from umol m-2 to umol m-2 d-1
PPFD24 <-rollapply(PPFD$PPFD, 48, sum, by=48)
date_PPFD24 <- seq(as.Date("2015-11-12"), length.out = 1825, by = "day")
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



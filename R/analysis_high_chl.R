
q_3 <- summary(df_sat$CHL)[5]

df_sat$date[which(df_sat$CHL > q_3)]

# Pick one day. I picked 2020-03-20.
high_chl_day1 <- selectByDate(df, start = "20/3/2020", end = "20/3/2020")
plot(high_chl_day1$date,high_chl_day1$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-03-20',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-03-20 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-03-20 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day1$date,high_chl_day1$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-03-20 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-03-20 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

# Pick one day. I picked 2020-02-28.
high_chl_day2 <- selectByDate(df, start = "28/2/2020", end = "28/2/2020")
plot(high_chl_day2$date,high_chl_day2$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-02-28',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-02-28 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-02-28 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day2$date,high_chl_day2$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-02-28 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-02-28 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)




# Pick one day. I picked 2020-02-26.
high_chl_day3 <- selectByDate(df, start = "26/2/2020", end = "26/2/2020")

plot(high_chl_day3$date,high_chl_day3$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-02-26',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-02-26 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-02-26 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day3$date,high_chl_day3$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-02-26 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-02-26 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2020-02-25.
high_chl_day4 <- selectByDate(df, start = "25/2/2020", end = "25/2/2020")

plot(high_chl_day4$date,high_chl_day4$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-02-25',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-02-25 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-02-25 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day4$date,high_chl_day4$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-02-25 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-02-25 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

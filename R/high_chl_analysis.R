
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


# Pick one day. I picked 2020-02-16.
high_chl_day5 <- selectByDate(df, start = "16/2/2020", end = "16/2/2020")

plot(high_chl_day5$date,high_chl_day5$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-02-16',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-02-16 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-02-16 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day5$date,high_chl_day5$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-02-16 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-02-16 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2020-02-05.
high_chl_day6 <- selectByDate(df, start = "05/2/2020", end = "05/2/2020")

plot(high_chl_day6$date,high_chl_day6$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-02-05',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-02-05 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-02-05 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day6$date,high_chl_day6$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-02-05 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-02-05 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2020-01-27.
high_chl_day7 <- selectByDate(df, start = "27/1/2020", end = "27/1/2020")

plot(high_chl_day7$date,high_chl_day7$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-01-27',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-01-27 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-01-27 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day7$date,high_chl_day7$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-01-27 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-01-27 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

# Pick one day. I picked 2020-01-25.
high_chl_day8 <- selectByDate(df, start = "25/1/2020", end = "25/1/2020")

plot(high_chl_day8$date,high_chl_day8$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-01-25',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-01-25 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-01-25 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day8$date,high_chl_day8$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-01-25 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-01-25 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2020-01-24.
high_chl_day9 <- selectByDate(df, start = "24/1/2020", end = "24/1/2020")

plot(high_chl_day9$date,high_chl_day9$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-01-24',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-01-24 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-01-24 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day9$date,high_chl_day9$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-01-24 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-01-24 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

# Pick one day. I picked 2020-01-23.
high_chl_day10 <- selectByDate(df, start = "23/1/2020", end = "23/1/2020")

plot(high_chl_day10$date,high_chl_day10$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-01-23',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-01-23 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-01-23 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day10$date,high_chl_day10$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-01-23 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-01-23 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

# Pick one day. I picked 2020-01-18.
high_chl_day11 <- selectByDate(df, start = "18/1/2020", end = "18/1/2020")

plot(high_chl_day11$date,high_chl_day11$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-01-18',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-01-18 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-01-18 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day11$date,high_chl_day11$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-01-18 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-01-18 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2020-01-15.
high_chl_day12 <- selectByDate(df, start = "15/1/2020", end = "15/1/2020")

plot(high_chl_day12$date,high_chl_day12$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-01-15',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-01-15 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-01-15 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day12$date,high_chl_day12$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-01-15 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-01-15 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2020-01-07.
high_chl_day13 <- selectByDate(df, start = "7/1/2020", end = "7/1/2020")

plot(high_chl_day13$date,high_chl_day13$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-01-07',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-01-07 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-01-07 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day13$date,high_chl_day13$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-01-07 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-01-07 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

# Pick one day. I picked 2020-01-06.
high_chl_day14 <- selectByDate(df, start = "6/1/2020", end = "6/1/2020")

plot(high_chl_day14$date,high_chl_day14$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2020-01-06',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2020-01-06 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2020-01-06 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day14$date,high_chl_day14$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2020-01-06 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2020-01-06 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

# Pick one day. I picked 2019-12-31.
high_chl_day15 <- selectByDate(df, start = "31/12/2019", end = "31/12/2019")

plot(high_chl_day15$date,high_chl_day15$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-12-31',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-12-31 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-12-31 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day15$date,high_chl_day15$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-12-31 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-12-31 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-12-30.
high_chl_day16 <- selectByDate(df, start = "30/12/2019", end = "30/12/2019")

plot(high_chl_day16$date,high_chl_day16$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-12-30',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-12-30 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-12-30 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day16$date,high_chl_day16$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-12-30 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-12-30 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

# Pick one day. I picked 2019-12-28.
high_chl_day17 <- selectByDate(df, start = "28/12/2019", end = "28/12/2019")

plot(high_chl_day17$date,high_chl_day17$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-12-28',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-12-28 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-12-28 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day17$date,high_chl_day17$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-12-28 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-12-28 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-12-26.
high_chl_day18 <- selectByDate(df, start = "26/12/2019", end = "26/12/2019")

plot(high_chl_day18$date,high_chl_day18$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-12-26',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-12-26 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-12-26 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day18$date,high_chl_day18$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-12-26 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-12-26 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-12-24.
high_chl_day19 <- selectByDate(df, start = "24/12/2019", end = "24/12/2019")
plot(high_chl_day19$date,high_chl_day19$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-12-24',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-12-24 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-12-24 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day19$date,high_chl_day19$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-12-24 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-12-24 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-12-21.
high_chl_day20 <- selectByDate(df, start = "21/12/2019", end = "21/12/2019")
plot(high_chl_day20$date,high_chl_day20$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-12-21',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-12-21 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-12-21 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day20$date,high_chl_day20$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-12-21 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-12-21 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-12-13.
high_chl_day21 <- selectByDate(df, start = "13/12/2019", end = "13/12/2019")
plot(high_chl_day21$date,high_chl_day21$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-12-13',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-12-13 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-12-13 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day21$date,high_chl_day21$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-12-13 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-12-13 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-12-08.
high_chl_day22 <- selectByDate(df, start = "8/12/2019", end = "8/12/2019")
plot(high_chl_day22$date,high_chl_day22$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-12-08',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-12-08 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-12-08 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day22$date,high_chl_day22$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-12-08 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-12-08 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-12-07.
high_chl_day23 <- selectByDate(df, start = "7/12/2019", end = "7/12/2019")
plot(high_chl_day23$date,high_chl_day23$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-12-07',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-12-07 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-12-07 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day23$date,high_chl_day23$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-12-07 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-12-07 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-12-06.
high_chl_day24 <- selectByDate(df, start = "6/12/2019", end = "6/12/2019")
plot(high_chl_day24$date,high_chl_day24$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-12-06',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-12-06 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-12-06 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day24$date,high_chl_day24$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-12-06 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-12-06 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-11-24.
high_chl_day25 <- selectByDate(df, start = "24/11/2019", end = "24/11/2019")
plot(high_chl_day25$date,high_chl_day25$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-11-24',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-11-24 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-11-24 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day25$date,high_chl_day25$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-11-24 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-11-24 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-03-06.
high_chl_day26 <- selectByDate(df, start = "6/3/2019", end = "6/3/2019")
plot(high_chl_day26$date,high_chl_day26$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-03-06',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-03-06 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-03-06 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day26$date,high_chl_day26$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-03-06 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-03-06 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-03-02.
high_chl_day27 <- selectByDate(df, start = "2/3/2019", end = "2/3/2019")
plot(high_chl_day27$date,high_chl_day27$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-03-02',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-03-02 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-03-02 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day27$date,high_chl_day27$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-03-02 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-03-02 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-02-15.
high_chl_day28 <- selectByDate(df, start = "15/2/2019", end = "15/2/2019")
plot(high_chl_day28$date,high_chl_day28$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-02-15',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-02-15 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-02-15 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day28$date,high_chl_day28$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-02-15 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-02-15 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

# Pick one day. I picked 2019-02-14.
high_chl_day29 <- selectByDate(df, start = "14/2/2019", end = "14/2/2019")
plot(high_chl_day29$date,high_chl_day29$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-02-14',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-02-14 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-02-14 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day29$date,high_chl_day29$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-02-14 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-02-14 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-02-12.
high_chl_day30 <- selectByDate(df, start = "12/2/2019", end = "12/2/2019")
plot(high_chl_day30$date,high_chl_day30$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-02-12',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-02-12 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-02-12 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day30$date,high_chl_day30$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-02-12 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-02-12 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-02-09.
high_chl_day31 <- selectByDate(df, start = "9/2/2019", end = "9/2/2019")
plot(high_chl_day31$date,high_chl_day31$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-02-09',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-02-09 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-02-09 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day31$date,high_chl_day31$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-02-09 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-02-09 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-02-07.
high_chl_day32 <- selectByDate(df, start = "7/2/2019", end = "7/2/2019")
plot(high_chl_day32$date,high_chl_day32$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-02-07',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-02-07 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-02-07 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day32$date,high_chl_day32$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-02-07 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-02-07 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-02-05.
high_chl_day33 <- selectByDate(df, start = "5/2/2019", end = "5/2/2019")
plot(high_chl_day33$date,high_chl_day33$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-02-05',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-02-05 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-02-05 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day33$date,high_chl_day33$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-02-05 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-02-05 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-01-24.
high_chl_day34 <- selectByDate(df, start = "24/1/2019", end = "24/1/2019")
plot(high_chl_day34$date,high_chl_day34$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-01-24',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-01-24 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-01-24 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day34$date,high_chl_day34$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-01-24 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-01-24 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2019-01-20.
high_chl_day35 <- selectByDate(df, start = "20/1/2019", end = "20/1/2019")
plot(high_chl_day35$date,high_chl_day35$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-01-20',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-01-20 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-01-20 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day35$date,high_chl_day35$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-01-20 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-01-20 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

# Pick one day. I picked 2019-01-15.
high_chl_day36 <- selectByDate(df, start = "15/1/2019", end = "15/1/2019")
plot(high_chl_day36$date,high_chl_day36$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2019-01-15',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2019-01-15 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2019-01-15 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day36$date,high_chl_day36$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2019-01-15 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2019-01-15 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2018-12-21.
high_chl_day37 <- selectByDate(df, start = "21/12/2018", end = "21/12/2018")
plot(high_chl_day37$date,high_chl_day37$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2018-12-21',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2018-12-21 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2018-12-21 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day37$date,high_chl_day37$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2018-12-21 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2018-12-21 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2018-12-09.
high_chl_day38 <- selectByDate(df, start = "9/12/2018", end = "9/12/2018")
plot(high_chl_day38$date,high_chl_day38$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2018-12-09',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2018-12-09 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2018-12-09 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day38$date,high_chl_day38$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2018-12-09 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2018-12-09 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2018-11-26.
high_chl_day39 <- selectByDate(df, start = "26/11/2018", end = "26/11/2018")
plot(high_chl_day39$date,high_chl_day39$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2018-11-26',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2018-11-26 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2018-11-26 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day39$date,high_chl_day39$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2018-11-26 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2018-11-26 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)



# Pick one day. I picked 2018-03-31.
high_chl_day40 <- selectByDate(df, start = "31/3/2018", end = "31/3/2018")
plot(high_chl_day40$date,high_chl_day40$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2018-03-31',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2018-03-31 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2018-03-31 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day40$date,high_chl_day40$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2018-03-31 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2018-03-31 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2018-03-15.
high_chl_day41 <- selectByDate(df, start = "15/3/2018", end = "15/3/2018")
plot(high_chl_day41$date,high_chl_day41$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2018-03-15',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2018-03-15 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2018-03-15 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day41$date,high_chl_day41$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2018-03-15 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2018-03-15 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2018-02-20.
high_chl_day42 <- selectByDate(df, start = "20/2/2018", end = "20/2/2018")
plot(high_chl_day42$date,high_chl_day42$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2018-02-20',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2018-02-20 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2018-02-20 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day42$date,high_chl_day42$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2018-02-20 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2018-02-20 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

# Pick one day. I picked 2018-02-180.
high_chl_day43 <- selectByDate(df, start = "18/2/2018", end = "18/2/2018")
plot(high_chl_day43$date,high_chl_day43$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2018-02-18',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2018-02-18 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2018-02-18 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day43$date,high_chl_day43$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2018-02-18 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2018-02-18 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2018-02-13.
high_chl_day44 <- selectByDate(df, start = "13/2/2018", end = "13/2/2018")
plot(high_chl_day44$date,high_chl_day44$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2018-02-13',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2018-02-13 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2018-02-13 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day44$date,high_chl_day44$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2018-02-13 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2018-02-13 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

# Pick one day. I picked 2018-02-11.
high_chl_day45 <- selectByDate(df, start = "11/2/2018", end = "11/2/2018")
plot(high_chl_day45$date,high_chl_day45$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2018-02-11',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2018-02-11 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2018-02-11 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day45$date,high_chl_day45$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2018-02-11 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2018-02-11 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2018-02-10.
high_chl_day46 <- selectByDate(df, start = "10/2/2018", end = "10/2/2018")
plot(high_chl_day46$date,high_chl_day46$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2018-02-10',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2018-02-10 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2018-02-10 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day46$date,high_chl_day46$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2018-02-10 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2018-02-10 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2018-02-09.
high_chl_day47 <- selectByDate(df, start = "9/2/2018", end = "9/2/2018")
plot(high_chl_day47$date,high_chl_day47$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2018-02-09',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2018-02-09 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2018-02-09 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day47$date,high_chl_day47$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2018-02-09 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2018-02-09 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

# Pick one day. I picked 2018-02-06.
high_chl_day48 <- selectByDate(df, start = "6/2/2018", end = "6/2/2018")
plot(high_chl_day48$date,high_chl_day48$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2018-02-06',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2018-02-06 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2018-02-06 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day48$date,high_chl_day48$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2018-02-06 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2018-02-06 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2018-01-19.
high_chl_day49 <- selectByDate(df, start = "19/1/2018", end = "19/1/2018")
plot(high_chl_day49$date,high_chl_day49$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2018-01-19',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2018-01-19 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2018-01-19 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day49$date,high_chl_day49$PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2018-01-19 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2018-01-19 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2017-12-27.
high_chl_day50 <- selectByDate(df, start = "27/12/2017", end = "27/12/2017")
plot(high_chl_day50 $date,high_chl_day50 $FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2017-12-27',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2017-12-27 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2017-12-27 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day50 $date,high_chl_day50 $PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2017-12-27 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2017-12-27 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)

# Pick one day. I picked 2017-12-25.
high_chl_day51 <- selectByDate(df, start = "25/12/2017", end = "25/12/2017")
plot(high_chl_day51 $date,high_chl_day51 $FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2017-12-25',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2017-12-25 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2017-12-25 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day51 $date,high_chl_day51 $PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2017-12-25 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2017-12-25 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)



# Pick one day. I picked 2017-12-18.
high_chl_day52 <- selectByDate(df, start = "18/12/2017", end = "18/12/2017")
plot(high_chl_day52 $date,high_chl_day52 $FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2017-12-18',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2017-12-18 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2017-12-18 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day52 $date,high_chl_day52 $PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2017-12-18 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2017-12-18 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2017-12-11.
high_chl_day53 <- selectByDate(df, start = "11/12/2017", end = "11/12/2017")
plot(high_chl_day53 $date,high_chl_day53 $FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2017-12-11',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2017-12-11 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2017-12-11 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day53 $date,high_chl_day53 $PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2017-12-11 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2017-12-11 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2017-12-08.
high_chl_day54 <- selectByDate(df, start = "8/12/2017", end = "8/12/2017")
plot(high_chl_day54 $date,high_chl_day54 $FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2017-12-08',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2017-12-08 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2017-12-08 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day54 $date,high_chl_day54 $PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2017-12-08 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2017-12-08 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2017-03-10.
high_chl_day55<- selectByDate(df, start = "10/3/2017", end = "10/3/2017")
plot(high_chl_day55 $date,high_chl_day55$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2017-03-10',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2017-03-10 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2017-03-10 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day55 $date,high_chl_day55 $PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2017-03-10 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2017-03-10 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2017-02-17.
high_chl_day56<- selectByDate(df, start = "17/2/2017", end = "17/2/2017")
plot(high_chl_day56 $date,high_chl_day56$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2017-02-17',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2017-02-17 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2017-02-17 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day56 $date,high_chl_day56 $PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2017-02-17 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2017-02-17 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2017-02-16.
high_chl_day57<- selectByDate(df, start = "16/2/2017", end = "16/2/2017")
plot(high_chl_day57 $date,high_chl_day57$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2017-02-16',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2017-02-16 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2017-02-16 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day57 $date,high_chl_day57 $PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2017-02-16 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2017-02-16 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2016-02-01.
high_chl_day58<- selectByDate(df, start = "1/2/2016", end = "1/2/2016")
plot(high_chl_day58 $date,high_chl_day58$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2016-02-01',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2016-02-01 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2016-02-01 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day58 $date,high_chl_day58 $PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2016-02-01 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2016-02-01 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)



# Pick one day. I picked 2016-01-05.
high_chl_day59<- selectByDate(df, start = "5/1/2016", end = "5/1/2016")
plot(high_chl_day59 $date,high_chl_day59$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2016-01-05',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2016-01-05 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2016-01-05 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day59 $date,high_chl_day59 $PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2016-01-05 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2016-01-05 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)


# Pick one day. I picked 2016-01-04.
high_chl_day60<- selectByDate(df, start = "4/1/2016", end = "4/1/2016")
plot(high_chl_day60 $date,high_chl_day60$FCO2, pch=19, type = 'o',
     xlab = 'Time',
     ylab = 'FCO2',
     main = '2016-01-04',
     ylim = c(-0.5,0.5), 
     xlim =c(as.POSIXct("2016-01-04 00:01:00", format = "%Y-%m-%d %H:%M:%S"),
             as.POSIXct("2016-01-04 23:59:00", format = "%Y-%m-%d %H:%M:%S")))
lines(high_chl_day60 $date,high_chl_day60 $PPFD/10000,col='green', lwd=2)
abline(v = as.POSIXct("2016-01-04 07:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(v = as.POSIXct("2016-01-04 19:00:00", format = "%Y-%m-%d %H:%M:%S"), lty = 2)
abline(h=0)




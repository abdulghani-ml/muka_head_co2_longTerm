
plot(c(mean(selectByDate(df_merge_month, month =1)$FCO2),
       mean(selectByDate(df_merge_month, month =2)$FCO2),
       mean(selectByDate(df_merge_month, month =3)$FCO2),
       mean(selectByDate(df_merge_month, month =4)$FCO2),
       mean(selectByDate(df_merge_month, month =5)$FCO2),
       mean(selectByDate(df_merge_month, month =6)$FCO2),
       mean(selectByDate(df_merge_month, month =7)$FCO2),
       mean(selectByDate(df_merge_month, month =8)$FCO2),
       mean(selectByDate(df_merge_month, month =9)$FCO2),
       mean(selectByDate(df_merge_month, month =10)$FCO2),
       mean(selectByDate(df_merge_month, month =11)$FCO2),
       mean(selectByDate(df_merge_month, month =12)$FCO2)), ylab = "")

c(mean(selectByDate(df_merge_month, month =1)$FCO2),
  mean(selectByDate(df_merge_month, month =2)$FCO2),
  mean(selectByDate(df_merge_month, month =3)$FCO2),
  mean(selectByDate(df_merge_month, month =4)$FCO2),
  mean(selectByDate(df_merge_month, month =5)$FCO2),
  mean(selectByDate(df_merge_month, month =6)$FCO2),
  mean(selectByDate(df_merge_month, month =7)$FCO2),
  mean(selectByDate(df_merge_month, month =8)$FCO2),
  mean(selectByDate(df_merge_month, month =9)$FCO2),
  mean(selectByDate(df_merge_month, month =10)$FCO2),
  mean(selectByDate(df_merge_month, month =11)$FCO2),
  mean(selectByDate(df_merge_month, month =12)$FCO2))

c(mean(selectByDate(df_merge_month, year =2016)$FCO2),
  mean(selectByDate(df_merge_month, year =2017)$FCO2),
  mean(selectByDate(df_merge_month, year =2018)$FCO2),
  mean(selectByDate(df_merge_month, year =2019)$FCO2),
  mean(selectByDate(df_merge_month, year = 2020)$FCO2))

c(sd(selectByDate(df_merge_month, year =2016)$FCO2),
  sd(selectByDate(df_merge_month, year =2017)$FCO2),
  sd(selectByDate(df_merge_month, year =2018)$FCO2),
  sd(selectByDate(df_merge_month, year =2019)$FCO2),
  sd(selectByDate(df_merge_month, year = 2020)$FCO2))


plot(c(mean(selectByDate(df_merge_month, year =2016)$WS),
  mean(selectByDate(df_merge_month, year =2017)$WS),
  mean(selectByDate(df_merge_month, year =2018)$WS),
  mean(selectByDate(df_merge_month, year =2019)$WS),
  mean(selectByDate(df_merge_month, year = 2020)$WS)))

c(sd(selectByDate(df_merge_month, year =2016)$WS),
  sd(selectByDate(df_merge_month, year =2017)$WS),
  sd(selectByDate(df_merge_month, year =2018)$WS),
  sd(selectByDate(df_merge_month, year =2019)$WS),
  sd(selectByDate(df_merge_month, year = 2020)$WS))


NEM_30min <- selectByDate(df, month = c(12,1,2,3))
windRose(NEM_30min, ws="WS",wd='WD', paddle = F)

STM_30min <- selectByDate(df, month = c(4,5))
windRose(STM_30min, ws="WS",wd='WD', paddle = F)

SWM_30min <- selectByDate(df, month = c(6,7,8,9))
windRose(SWM_30min, ws="WS",wd='WD', paddle = F)

FTM_30min <- selectByDate(df, month = c(10,11))
windRose(FTM_30min, ws="WS",wd='WD', paddle = F)

summary(lm(FCO2 ~ CHL, data = df_merge_month))

summary(lm(FCO2 ~ PPFD, data = df_merge_month))

summary(lm(FCO2 ~ PAR, data = df_merge_month))


plot(df_merge_year$date,df_merge_year$FCO2)
plot(df_merge_year$date,df_merge_year$PAR)
plot(df_merge_year$date, df_merge_year$CHL)

plot(df_merge_year$date, df_merge_year$TA)
plot(df_merge_year$date, df_merge_year$TS)
plot(df_merge_year$date, df_merge_year$SST)

write.table(df, file = "co2flux_30min.csv")
write.table(df_merge_month, file = "co2flux_1month.csv")

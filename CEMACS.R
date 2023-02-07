df_chl_sampl<- read.csv('data/Data_1MH-SongSong.csv')
df_penang <- df_chl_sampl[which(df_chl_sampl$year==2016),(df_chl_sampl$state=="Penang")]



plot(df_chl_sampl$date,df_chl_sampl$Chl.a..mg.L.)
plot(df_chl_sampl$date,df_chl_sampl$Nitrate..mg.L.)
library(ggplot2)

jpeg(filename='figs/chlcemacs.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)
ggplot(df_chl_sampl, aes(Chl.a..mg.L.)) + geom_point(alpha = 0.3) + 
  geom_smooth(col = 'green', alpha = 0.1) +
  theme_bw()
dev.off()


plot(df_chl_sampl$Chl.a..mg.L.)

library(dplyr)
df_songsong <- read.csv('data/sampling/Data_MH - SongSong.csv', sep=',')

date_songsong <- paste0(df_songsong$date,"-",df_songsong$month,"-",df_songsong$year)
date <- strptime(date_songsong,"%d-%B-%Y",tz="Asia/Kuala_Lumpur")

group <- 0:(nrow(df_songsong) - 1)%/% 3
df_songsong <- cbind(date,group,df_songsong)
rm(date,date_songsong,group)



df_songsong <- df_songsong[,-c(3:6,9)]

df_songsong_avg <- df_songsong[,-c(1,3,4)]

df_songsong_avg <- df_songsong_avg %>% group_by(group) %>% 
  summarize(across(everything(), list(mean), na.rm=TRUE))

row.names(df_songsong_avg) <- c("Penang-Nov","K.Muda-Nov","P.Bidan-Nov","T.Terendak-Nov","Songsong-Nov",
                            "Penang-Dec","K.Muda-Dec","P.Bidan-Dec","T.Terendak-Dec","Songsong-Dec",
                            "Penang-Feb","K.Muda-Feb","P.Bidan-Feb","T.Terendak-Feb","Songsong-Feb")

#### Species Analysis ####
df_songsong_avg_1 <- df_songsong_avg[,-c(1:16)]

df_songsong_avg_1 <- df_songsong_avg_1[,-10]


row.names(df_songsong_avg_1) <- c("Penang-Nov","K.Muda-Nov","P.Bidan-Nov","T.Terendak-Nov","Songsong-Nov",
                                  "Penang-Dec","K.Muda-Dec","P.Bidan-Dec","T.Terendak-Dec","Songsong-Dec",
                                  "Penang-Feb","K.Muda-Feb","P.Bidan-Feb","T.Terendak-Feb","Songsong-Feb")

heatmap(t(df_songsong_avg_1))

#####

#### Water Parameter Analysis ####
df_songsong_avg_2 <- df_songsong_avg[,c(2:16)]

row.names(df_songsong_avg_2) <- c("Penang-Nov","K.Muda-Nov","P.Bidan-Nov","T.Terendak-Nov","Songsong-Nov",
                                  "Penang-Dec","K.Muda-Dec","P.Bidan-Dec","T.Terendak-Dec","Songsong-Dec",
                                  "Penang-Feb","K.Muda-Feb","P.Bidan-Feb","T.Terendak-Feb","Songsong-Feb")

df_songsong_avg_3 <- scale(df_songsong_avg_2)
heatmap(t(df_songsong_avg_3))

#####


#### Water Parameter without Photosynthetic Data Analysis ####
df_songsong_avg_4 <- df_songsong_avg[,-c(1,16,13,15,12,14,17:54)]

row.names(df_songsong_avg_4) <- c("Penang-Nov","K.Muda-Nov","P.Bidan-Nov","T.Terendak-Nov","Songsong-Nov",
                                  "Penang-Dec","K.Muda-Dec","P.Bidan-Dec","T.Terendak-Dec","Songsong-Dec",
                                  "Penang-Feb","K.Muda-Feb","P.Bidan-Feb","T.Terendak-Feb","Songsong-Feb")

df_songsong_avg_4 <- scale(df_songsong_avg_4)
heatmap(t(df_songsong_avg_4))

#####

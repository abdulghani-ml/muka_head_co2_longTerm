library(openair)

df_cor <- data.frame(df_merge_month$FCO2,
                     df_merge_month$CHL,
                     df_merge_month$WS,
                     df_merge_month$TA,
                     df_merge_month$TS,
                     df_merge_month$SST,
                     df_merge_month$PAR,
                     df_merge_month$RH,
                     df_merge_month$ZL,
                     df_merge_month$USTAR)

corPlot(df_cor)

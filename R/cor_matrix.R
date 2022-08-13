library(openair)

# monthly
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

# yearly

df_cor <- data.frame(df_merge_year$FCO2,
                     df_merge_year$CHL,
                     df_merge_year$WS,
                     df_merge_year$TA,
                     df_merge_year$TS,
                     df_merge_year$SST,
                     df_merge_year$PAR,
                     df_merge_year$RH,
                     df_merge_year$ZL,
                     df_merge_year$USTAR)

corPlot(df_cor)



df_cor_pos <- data.frame(FCO2=df_pos$FCO2,WS=df_pos$WS,WD=df_pos$WD,H=df_pos$H,LE=df_pos$LE,
                         USTAR=df_pos$USTAR,ZL=df_pos$ZL,CD=df_pos$CD,
                         RN=df_pos$RN,RG=df_pos$RG,PPFD=df_pos$PPFD,
                         EMA=df_pos$EMA,RH_ema=df_pos$RH_ema,TA_ema=df_pos$TA_ema,delT_ema=df_pos$delT_ema)

df_cor_neg <- data.frame(FCO2=df_neg$FCO2,WS=df_neg$WS,WD=df_neg$WD,H=df_neg$H,LE=df_neg$LE,
                         USTAR=df_neg$USTAR,ZL=df_neg$ZL,CD=df_neg$CD,
                         RN=df_neg$RN,RG=df_neg$RG,PPFD=df_neg$PPFD,
                         EMA=df_neg$EMA,RH_ema=df_neg$RH_ema,TA_ema=df_neg$TA_ema,delT_ema=df_neg$delT_ema)

df_cor_pos_neg <- data.frame(FCO2=df_pos_neg$FCO2,WS=df_pos_neg$WS,WD=df_pos_neg$WD,H=df_pos_neg$H,LE=df_pos_neg$LE,
                             USTAR=df_pos_neg$USTAR,ZL=df_pos_neg$ZL,CD=df_pos_neg$CD,
                             RN=df_pos_neg$RN,RG=df_pos_neg$RG,PPFD=df_pos_neg$PPFD,
                             EMA=df_pos_neg$EMA,RH_ema=df_pos_neg$RH_ema,TA_ema=df_pos_neg$TA_ema,delT_ema=df_pos_neg$delT_ema)


cor(df_cor_pos_neg, use='pairwise.complete.obs', method = 'spearman')
corPlot(df_cor_pos_neg,method = 'spearman')

cor(df_cor_pos, use='pairwise.complete.obs', method = 'spearman')
corPlot(df_cor_pos,method = 'spearman')

cor(df_cor_neg, use='pairwise.complete.obs', method = 'spearman')
corPlot(df_cor_neg,method = 'spearman')

cor.test(df_cor_pos$CD,df_cor_pos$FCO2, method='spearman')
cor.test(log10(df_cor_pos$CD),df_cor_pos$FCO2, method='spearman')


cor.test(df_cor_neg$CD,df_cor_neg$FCO2, method='spearman')
cor.test(log10(df_cor_neg$CD),df_cor_neg$FCO2, method='spearman')


#### U >= 0.5 ####

##### Positive FCO2 ####
df_fit_pos <- data.frame(WS=df_pos$WS[which(df_pos$WS >= 0.5)],
                         FCO2=df_pos$FCO2[which(df_pos$WS >= 0.5)])
WS2 <- df_fit_pos$WS^2
WS3 <- df_fit_pos$WS^3

plot(df_fit_pos$WS,df_fit_pos$FCO2,pch=19)

fit1_fco2_u_pos <- lm(FCO2 ~ 0 + WS, data = df_fit_pos)
summary(fit1_fco2_u_pos)

fit2_fco2_u_pos <- lm(FCO2 ~ 0 + WS2, data = df_fit_pos)
summary(fit2_fco2_u_pos)

fit3_fco2_u_pos <- lm(FCO2 ~ 0 + WS3, data = df_fit_pos)
summary(fit3_fco2_u_pos)

##### Negative FCO2 ####
df_fit_neg <- data.frame(WS=df_neg$WS[which(df_neg$WS >= 0.5)],
                         FCO2=df_neg$FCO2[which(df_neg$WS >= 0.5)])
WS2 <- df_fit_neg$WS^2
WS3 <- df_fit_neg$WS^3

plot(df_fit_neg$WS,abs(df_fit_neg$FCO2),pch=19)

fit1_fco2_u_neg <- lm(abs(FCO2) ~ 0 + WS, data = df_fit_neg)
summary(fit1_fco2_u_neg)

fit2_fco2_u_neg <- lm(abs(FCO2) ~ 0 + WS2, data = df_fit_neg)
summary(fit2_fco2_u_neg)

fit3_fco2_u_neg <- lm(abs(FCO2) ~ 0 + WS3, data = df_fit_neg)
summary(fit3_fco2_u_neg)


##### Positive and Negative FCO2 ####
df_fit_pos_neg <- data.frame(WS=df_pos_neg$WS[which(df_pos_neg$WS >= 0.5)],
                         FCO2=df_pos_neg$FCO2[which(df_pos_neg$WS >= 0.5)])
WS2 <- df_fit_pos_neg$WS^2
WS3 <- df_fit_pos_neg$WS^3

plot(df_fit_pos_neg$WS,abs(df_fit_pos_neg$FCO2),pch=19)

fit1_fco2_u_neg <- lm(abs(FCO2) ~ 0 + WS, data = df_fit_pos_neg)
summary(fit1_fco2_u_neg)

fit2_fco2_u_neg <- lm(abs(FCO2) ~ 0 + WS2, data = df_fit_pos_neg)
summary(fit2_fco2_u_neg)

fit3_fco2_u_neg <- lm(abs(FCO2) ~ 0 + WS3, data = df_fit_pos_neg)
summary(fit3_fco2_u_neg)




plot(df_fit$WS,df_fit$FCO2)

abline(fit1_fco2_u)
abline(fit2_fco2_u,col='red')
abline(fit3_fco2_u,col='orange')

#### U < 0.02 ####
df_fit_05 <- data.frame(WS=df$WS[which(df$WS < 0.02)],
                        FCO2=abs(df$FCO2[which(df$WS < 0.02)]))
WS2_05 <- df_fit_05$WS^2
WS3_05 <- df_fit_05$WS^3

fit1_fco2_u05 <- lm(FCO2 ~ 0 + WS, data = df_fit_05)
summary(fit1_fco2_u05)

fit2_fco2_u05 <- lm(FCO2 ~ 0 + WS2_05, data = df_fit_05)
summary(fit2_fco2_u05)

fit3_fco2_u05 <- lm(FCO2 ~ 0 + WS3_05, data = df_fit_05)
summary(fit3_fco2_u05)


plot(df_fit_05$WS,df_fit_05$FCO2)

abline(fit1_fco2_u05)
abline(fit2_fco2_u05,col='red')
abline(fit3_fco2_u05,col='orange')

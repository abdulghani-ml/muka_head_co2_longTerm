jpeg(filename='/Users/Yusri/Downloads/co2.jpeg', 
     unit = 'cm', width = 10, height = 10, res = 360)
par(mar=c(4,4,1,1),xpd=FALSE)


plot(FCO2_2016$date, FCO2_2016$FCO2, type = 'l', 
     xlab = '', ylab = '',
     ylim = c(-0.15,0.10), xaxt = "n", lwd = 2)
lines(FCO2_2017$date,FCO2_2017$FCO2, lty = 2, lwd = 2)
lines(FCO2_2018$date,FCO2_2018$FCO2, lty = 3, lwd = 2)
lines(FCO2_2019$date,FCO2_2019$FCO2, lty = 4, lwd = 2)
lines(FCO2_2020$date,FCO2_2020$FCO2, lty = 5, lwd = 2)
axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", 
                                        "Apr", "May", "Jun", 
                                        "Jul", "Aug", "Sep", 
                                        "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
#mtext('Months', side = 1, line = 2.5)
mtext(expression(paste('CO'['2'],' flux (','mol C',' ','m'^{'-2'}, 
                       ' ', 'yr'^{'-1'},')',sep = "")), side = 2, line = 2)
legend_order <- c(2016:2020)
legend_line<-c( 1:5)
legend("topleft", legend = legend_order, bty = "n",
       lty = legend_line, lwd = rep(2,by = 5), ncol = 2)
#grid(nx =25,ny = 5, lwd =0.5, lty = 1, col = "gray")
dev.off()



jpeg(filename='/Users/Yusri/Downloads/co2_ggplot.jpeg', unit = 'cm', 
     width = 10, height = 10, res = 360)    #TIME SERIES WITH SMOOTH BEST FIT LINES
ggplot() +
  geom_smooth(data = FCO2_2016, aes(x = as.numeric(date), y = FCO2, lty= 1), 
              group = 1, linetype = 1, col = 'grey30', alpha = 0.1) + 
  geom_smooth(data = FCO2_2017, aes(x = as.numeric(date), y = FCO2,), 
              group = 2, linetype = 2, col = 'grey30', alpha = 0.1) +
  geom_smooth(data = FCO2_2018, aes(x = as.numeric(date), y = FCO2,), 
              group = 3, linetype = 3, col = 'grey30', alpha = 0.1) +
  geom_smooth(data = FCO2_2019, aes(x = as.numeric(date), y = FCO2,), 
              group = 4, linetype = 4, col = 'grey30', alpha = 0.1) +
  geom_smooth(data = FCO2_2020, aes(x = as.numeric(date), y = FCO2,), 
              group = 5, linetype = 5, col = 'grey30', alpha = 0.1) +
  theme_bw() +
  coord_cartesian(ylim = c(-0.15,0.15)) +
  xlab('Month') +
  
  ylab(expression(paste('F'['CO'['2']],' (',mu,'mol ',' m'^{'-2'}, ' s'^{'-1'},')'))) 
#geom_text(data = df_merge_month, aes(date, CO2),y = 20, color = "red", hjust = -0.5) #These line is to put wordsin the plot
#geom_text() +
#annotate("text", label = "t3", x = date, y = 24.5, size = 8, colour = "red")
dev.off()


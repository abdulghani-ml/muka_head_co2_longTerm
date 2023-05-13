#install.packages('marmap')
#install.packages('rgdal')
#install.packages('raster')
library(marmap)
library(rgdal)
library(raster)

bath_ppinang <- getNOAA.bathy(lon1 = 98.2, lon2 = 102, 
                              lat1 = 3, lat2 = 8,
                              resolution = 1)
summary(bath_ppinang)
plot(bath_ppinang)

# # Simple plot
# jpeg(filename = "figs/bath_ppinang.jpg",height=8,width=8,
#      bg = "white",units='cm', res = 360)
# par(family='Times', mar = c(3.1, 3.1, 0.6, 0.6))
# plot(bath_ppinang, image = TRUE,
#      xlim = c(100,101), ylim = c(5.0,7.65),
#      lwd = c(0.2,0.4,0.6, 0.8, 1), lty = c(1,1,1, 1, 1),
#      col = c("lightgrey", "darkgrey", "black"),
#      drawlabel = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), 
#      cex.lab = 1, cex.axis = 0.6,
#      xlab = '', ylab = '')
# mtext(side = 1, 'Longitude', line = 2)
# mtext(side = 2, 'Latitude', line = 2)
# points(100.2025,5.4750, pch = 19, col = 'red', cex = 1)
# scaleBathy(bath_ppinang, deg = 0.045, x = "bottomright", inset = 5, 
#            cex = 0.8)
# dev.off()

jpeg(filename = "figs/bath_ppinang2.jpg",height=15,width=15,
     bg = "white",units='cm', res = 360)
plot(bath_ppinang, image = TRUE,
     xlim = c(99.9,100), ylim = c(5.0,7.65),
     lwd = c(0.6, 0.8, 1), lty = c(1, 1, 1),
     col = c("lightgrey", "darkgrey", "black"),
     drawlabel = c(TRUE, TRUE, TRUE), 
     cex.lab = 1, cex.axis = 0.6,
     xlab = '', ylab = '')
mtext(side = 1, 'Longitude', line = 2)
mtext(side = 2, 'Latitude', line = 2)
points(100.2025,5.4750, pch = 19, col = 'red', cex = 1)
scaleBathy(bath_ppinang, deg = 0.045, x = "bottomright", inset = 5, 
           cex = 0.8)
dev.off()


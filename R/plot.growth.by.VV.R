#=========================================================================
# Comparing, by drawing figures, the stand characteristics of PipeQual
# output to those given by the models of Vuokila & V?liaho (1980) in
# Norway spruce.
## Before running this script, the stand characteristics by Vuokila & V?liaho
# should have been computed with the script "compute.growth.by.VV.R". In
# that script, the temporal development of dominant height can be chosen to
# be predicted in three alternative ways ("VV", "bonity" and "PipeQual").
# The results are output in the data frames "VV.growth" and "VV.thinnings",
# with the name of the dominant height prediction method appended in the data
# frame name if other than "VV".
## In the comparison, the characteristics resulting from all the three
# alternative dominant height prediction methods, if employed, are plotted
# in the same figure.
## Input: General settings in the script; PipeQual output file "sta.out" in a
# user-specified directory; data frames produced by the script
# "compute.growth.by.VV.R", see above
# Output: Figure file "compare.PipeQual.output.to.VV.pdf" in a user-specified
# directory

#==========================================================================
# SET: General settings
#----------------------
# Directory where the PipeQual output files are located
#datadir <- "C:/LocalData/amakela/MyDocuments/Research/USER/Visual Studio/GrowthVsVuokilaValiaho/R/OUT/Nyn?s3_2/"
#datadir <- "C:/LocalData/amakela/MyDocuments/Research/USER/Visual Studio/Nitrogen/nitrogen/nitrogen/"

# Directory where figure files should be saved
figuredir <- paste0(getwd(),"/figures/")
    #"C:/LocalData/amakela/MyDocuments/Research/USER/Visual Studio/GrowthVsVuokilaValiaho/R/PDF/"

# Reading into R the PipeQual output file sta.out containing the stand
# characteristics w.r.t. time
#----------------------------------------------------------------------

#sta.temp <- read.table(file = paste(datadir, "sta.out", sep = ""),	
#                       header = F, sep = "", dec = ".", 	
#                       na.strings = "NA", skip = 1)

#names(sta.temp) <- c("time", "Wf", "Wr", "Wb", "Wbd", "Wc", "Hdom", "hav", "BA",	
#                     "V", "N", "A", "Marklund", "Wb2", "Wbd2", "CC")
rm(VV.growth.bonity)

# Plotting stand characteristics vs. time 

#----------------------------------------  

pdf(file = paste(figuredir, "plot.VV.pdf", sep = ""),	
    width = 11, height = 8, family = "Times", paper = "a4r")

par(mfrow = c(2, 2), pty = "m", oma = c(2, 2, 2, 2), mar = c(4.5, 4.5, 4, 4))

if (exists("VV.growth"))lyear <- max( VV.growth$T, na.rm = T) 

# Last simulation year

if (exists("VV.growth.bonity"))lyear <- max(VV.growth.bonity$T, na.rm = T) 

# Last simulation year
#if (exists("VV.growth.PipeQual"))lyear <- max(c(sta.temp$time, VV.growth.PipeQual$T), na.rm = T) 

# Last simulation year

vlines <- seq(0, by = 50, length = floor((lyear + 10) / 50) + 1)
xlim <- c(0, lyear + 10) 

# Dominant height

#temp <- sta.temp$Hdom

if (exists("VV.growth")) temp <- VV.growth$H

if (exists("VV.growth.bonity")) temp <- VV.growth.bonity$H

#if (exists("VV.growth.PipeQual")) temp <- c(temp, VV.growth.PipeQual$H)
ylim <- c(0, 1.05 * max(temp, na.rm = T))

#plot(x = sta.temp$time, y = sta.temp$Hdom, type = "l",	xlim = xlim, ylim = ylim,	
#     xlab = "Time (a)",	ylab = "Dominant height (m)",	col = "red", lwd = 1.5,	cex.axis = 1.4, cex.lab = 1.4)

if (exists("VV.growth")) plot(x = VV.growth$T, y = VV.growth$H,  type = "l",	xlim = xlim, ylim = ylim,	
          xlab = "Time (a)",	ylab = "Dominant height (m)",col = "black", lwd = 1.5, cex.axis = 1.4, cex.lab = 1.4)

if (exists("VV.growth.bonity")) lines(x = VV.growth.bonity$T, y = VV.growth.bonity$H,  col = "red", lwd = 1.5)


#legend(x = "bottomright",
#       legend = c("PipeQual", "V & V"),
#       col = c("red", "darkblue"), lwd = 1.5, bty = "n", cex = 1.2)

#         legend(x = "bottomright",
#	 legend = c("PipeQual", "V & V", "V & V & bonity H"),
#	 col = c("red", "black", "darkblue"), lwd = 1.5, bty = "n", cex = 1.2)
#    abline(h = 0, v = vlines)
#   legend(x = "bottomright",
#	 legend = c("PipeQual", "V & V", "V & V & bonity H", "V & V & PipeQual H"),
#	 col = c("red", "black", "darkblue", "blue"), lwd = 1.5, bty = "n", cex = 1.2)
#         legend(x = "bottomright",
#	 legend = c("PipeQual", "V & V"),	 col = c("red", "black"), lwd = 1.5, bty = "n", cex = 1.2)

# Stand densitytemp <- sta.temp$N

if (exists("VV.growth")) temp <- c(temp, VV.growth$N)
if (exists("VV.growth.bonity")) temp <- c(temp, VV.growth.bonity$N)
#if (exists("VV.growth.PipeQual")) temp <- c(temp, VV.growth.PipeQual$N)

ylim <- c(0, 1.05 * max(temp, na.rm = T))

if (exists("VV.growth")) plot(x = VV.growth$T, y = VV.growth$N, type = "l",	
     xlim = xlim, ylim = ylim,	
     xlab = "Time (a)",	
     ylab = expression(paste("Stand density (", ha^-1, ")", sep = "")),	
     col = "red", lwd = 1.5,	cex.axis = 1.4, cex.lab = 1.4)

if (exists("VV.growth.bonity")) lines(x = VV.growth.bonity$T, y = VV.growth.bonity$N,
                                      col = "darkblue", lwd = 1.5)
#if (exists("VV.growth.PipeQual")) lines(x = VV.growth.PipeQual$T, y = VV.growth.PipeQual$N,
#   col = "blue", lwd = 1.5)abline(h = 0, v = vlines)

# Basal area
#temp <- sta.temp$BA

if (exists("VV.growth")) temp <- VV.growth$G

if (exists("VV.growth.bonity")) temp <- VV.growth.bonity$G
#if (exists("VV.growth.PipeQual")) temp <- c(temp, VV.growth.PipeQual$G)

ylim <- c(0, 1.05 * max(temp, na.rm = T))

if (exists("VV.growth")) plot(x = VV.growth$T, y = VV.growth$G, type = "l",xlim = xlim, ylim = ylim,	xlab = "Time (a)",	
     ylab = expression(paste("Basal area (", m^2, ha^-1, ")", sep = "")),	col = "red", lwd = 1.5,	cex.axis = 1.4, cex.lab = 1.4)


if (exists("VV.growth.bonity")) lines(x = VV.growth.bonity$T, y = VV.growth.bonity$G,
                                      col = "darkblue", lwd = 1.5)

#if (exists("VV.growth.PipeQual")) lines(x = VV.growth.PipeQual$T, y = VV.growth.PipeQual$G,
#   col = "blue", lwd = 1.5)abline(h = 0, v = vlines)

# Stem volumetemp <- sta.temp$V

if (exists("VV.growth")) temp <- VV.growth$V

if (exists("VV.growth.bonity")) temp <- VV.growth.bonity$V
#if (exists("VV.growth.PipeQual")) temp <- c(temp, VV.growth.PipeQual$V)

ylim <- c(0, 1.05 * max(temp, na.rm = T))

if (exists("VV.growth")) plot(x = VV.growth$T, y = VV.growth$V, type = "l",	xlim = xlim, ylim = ylim,	
     xlab = "Time (a)",	
     ylab = expression(paste("Stem volume (", m^3, ha^-1, ")", 
                             sep = "")),	
     col = "red", lwd = 1.5,	cex.axis = 1.4, cex.lab = 1.4)


if (exists("VV.growth.bonity")) lines(x = VV.growth.bonity$T, y = VV.growth.bonity$V,
                                      col = "darkblue", lwd = 1.5)
#if (exists("VV.growth.PipeQual")) lines(x = VV.growth.PipeQual$T, y = VV.growth.PipeQual$V,
#   col = "blue", lwd = 1.5)abline(h = 0, v = vlines)

# Title for the page
#mtext(text = "Stand characteristics: PipeQual vs. Vuokila & V?liaho",
#	   side = 3, line = 0, outer = T, cex = 1.4)

dev.off()

rm(figuredir,  lyear, xlim, temp, ylim, vlines)
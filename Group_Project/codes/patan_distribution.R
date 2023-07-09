library(fitdistrplus)
library(stats4)
library(caret)
library(MASS)
##############################################
df <- read.csv("D://IITH/Semester 1/CE6501/Project/data/Monthly.csv")
df1<-df[,3:5] # monthly 3:5
process <- preProcess(as.data.frame(df1), method=c("range"))
norm_scale <- predict(process, as.data.frame(df1))
df[,3:5]<-norm_scale # monthly 3:5
#write.csv(df,"scaled_monthly.csv",row.names = FALSE)
##############################################
# Distribution of Q
expo<- fitdist(df$Q, "exp",method='mme') 
norm<- fitdist(df$Q, "norm",method='mme')
png("figures/Q_monthly_curvefit.png")
par(mfrow=c(2,2))
plot.legend <- c("Exponential","Normal")
denscomp(list(expo,norm), legendtext = plot.legend)
qqcomp(list(expo,norm), legendtext = plot.legend)
cdfcomp(list(expo,norm), legendtext = plot.legend)
ppcomp(list(expo,norm), legendtext = plot.legend)
mtext("Curve Fitting for StreamFlow",                   # Add main title
      side = 3,
      line = - 1,
      outer = TRUE)
dev.off()
#############################################################################
# Distribution of P
expo<- fitdist(df$P, "exp",method='mme') 
norm<- fitdist(df$P, "norm")
png("figures/P_monthly_curvefit.png")
par(mfrow=c(2,2))
plot.legend <- c("Exponential","Normal")
denscomp(list(expo,norm), legendtext = plot.legend)
qqcomp(list(expo,norm), legendtext = plot.legend)
cdfcomp(list(expo,norm), legendtext = plot.legend)
ppcomp(list(expo,norm), legendtext = plot.legend)
mtext("Curve Fitting for Precipitation",                   # Add main title
      side = 3,
      line = - 1,
      outer = TRUE)
dev.off()
#################################################################################
# Distribution of E
expo<- fitdist(df$E, "exp",method='mme') 
norm<- fitdist(df$E, "norm")
png("figures/E_monthly_curvefit.png")
par(mfrow=c(2,2))
plot.legend <- c("Exponential","Normal")
denscomp(list(expo,norm), legendtext = plot.legend)
qqcomp(list(expo,norm), legendtext = plot.legend)
cdfcomp(list(expo,norm), legendtext = plot.legend)
ppcomp(list(expo,norm), legendtext = plot.legend)
mtext("Curve Fitting for Evapotranspiration",                   # Add main title
      side = 3,
      line = - 1,
      outer = TRUE)
dev.off()
##################################################################################


